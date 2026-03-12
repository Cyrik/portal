(ns portal.ssr.server
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [portal.runtime :as rt]
            [portal.runtime.index :as index]
            [portal.runtime.json :as json]
            [portal.ssr.session :as session]
            [portal.ssr.viewer :as viewer])
  (:import [java.util UUID]))

;; This file is loaded via (require 'portal.ssr.server) at the bottom of
;; portal.runtime.jvm.server, so the route multimethod is already defined.
;; We reference it by fully qualified symbol to avoid circular dependency.


(def ^:private cleanup-timeout-ms (* 5 60 1000)) ; 5 minutes

;; -- Query param parsing --

(defn- parse-query-params [qs]
  (when qs
    (into {}
          (for [pair (str/split qs #"&")
                :let [[k v] (str/split pair #"=" 2)]
                :when k]
            [k (or v "")]))))

(defn- try-parse-uuid [s]
  (try (UUID/fromString s) (catch Exception _ nil)))

;; -- SSE helpers --

(defn- sse-encode-html
  "Encode multi-line HTML for SSE: each line needs its own data: elements prefix."
  [html-str]
  (->> (str/split-lines html-str)
       (map #(str "data: elements " %))
       (str/join "\n")))

(defn- sse-patch-elements [html-str]
  (str "event: datastar-patch-elements\n"
       (sse-encode-html html-str) "\n\n"))

(defn- sse-patch-prepend [selector html-str]
  (str "event: datastar-patch-elements\n"
       "data: selector " selector "\n"
       "data: mode prepend\n"
       (sse-encode-html html-str) "\n\n"))

(defn- send-full! [proto session-id ch]
  (let [state (session/get-state session-id)
        html  (viewer/render-page-html state proto)]
    (case proto
      :a (http/send! ch (sse-patch-elements html) false)
      :b (http/send! ch
           (json/write {"op" "full" "id" "tap-list" "html" html})
           false))))

;; -- Reconcile on connect --

(defn- stored-matches-tail?
  "Check if stored values (newest-first) are the tail of live-taps by identity."
  [state live-taps]
  (let [order  (:order state [])
        values (:values state {})
        n      (count order)
        tail   (drop (- (count live-taps) n) live-taps)]
    (and (<= n (count live-taps))
         (= n (count (seq tail)))
         (every? (fn [[vid tap]] (identical? (get values vid) tap))
                 (map vector order tail)))))

(defn- reset-and-snapshot! [session-id]
  (swap! session/sessions update session-id
         (fn [s]
           (merge {:session-id session-id
                   :next-id    0
                   :tap-count  0
                   :order      []
                   :values     {}
                   :expanded   #{}
                   :limits     {}
                   :nav-stack  []}
                  (select-keys s [:channel :tap-cleanup]))))
  (session/snapshot-existing! session-id))

(defn- reconcile! [session-id]
  (let [live-taps  @@#'rt/tap-list            ;; newest-first
        live-count (count live-taps)
        state      (session/get-state session-id)
        tap-count  (:tap-count state 0)]
    (cond
      ;; Stored values match the tail of live-taps → add any new ones
      (and (>= live-count tap-count)
           (stored-matches-tail? state live-taps))
      (when (> live-count tap-count)
        (let [new-taps (take (- live-count tap-count) live-taps)]
          (doseq [v (reverse new-taps)]
            (session/add-value! session-id v))
          (session/update-tap-count! session-id live-count)))

      ;; Any mismatch (clear, replace, same-count-different-values) → full reset
      :else
      (reset-and-snapshot! session-id))))

;; -- Proto A: Datastar (SSE + POST) --

(defmethod portal.runtime.jvm.server/route [:get "/ssr/a"] [_request]
  (let [session-id (UUID/randomUUID)]
    (session/ensure! session-id)
    (session/snapshot-existing! session-id)
    (let [state       (session/get-state session-id)
          body-html   (viewer/render-page-html state :a)
          datastar-js "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-RC.8/bundles/datastar.js"]
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    (index/html
                 {:code-url     nil
                  :extra-head   (str "<script type=\"module\" src=\"" datastar-js "\"></script>")
                  :body-content (str "<div data-init=\"@get('/ssr/sse?s=" session-id "')\">"
                                     body-html
                                     "</div>")})})))

(defmethod portal.runtime.jvm.server/route [:get "/ssr/sse"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))]
    (if-not session-id
      {:status 400 :body "Missing session id"}
      (http/as-channel request
      {:on-open
       (fn [ch]
         (http/send! ch {:status  200
                         :headers {"Content-Type"  "text/event-stream"
                                   "Cache-Control" "no-cache"
                                   "Connection"    "keep-alive"}}
                     false)
         (session/ensure! session-id)
         (session/touch! session-id)
         (session/set-channel! session-id ch)
         ;; Reconcile missed taps
         (reconcile! session-id)
         ;; Push full page re-render
         (send-full! :a session-id ch)
         ;; Register watchers
         (let [cleanup-taps (session/watch-taps! session-id
                              (fn [sid vid _value]
                                (when-let [sse-ch (session/get-channel sid)]
                                  (let [state (session/get-state sid)]
                                    (when (empty? (:nav-stack state))
                                      (let [html (viewer/render-value-html state vid :a)]
                                        (http/send! sse-ch (sse-patch-prepend "#tap-list" html) false)))))))
               cleanup-tap-list (session/watch-tap-list! session-id
                                  (fn [sid _old-taps _new-taps]
                                    (when-let [sse-ch (session/get-channel sid)]
                                      (reconcile! sid)
                                      (send-full! :a sid sse-ch))))
               cleanup (fn []
                         (cleanup-tap-list)
                         (cleanup-taps))]
           ;; Store cleanup fn
           (swap! session/sessions assoc-in [session-id :tap-cleanup] cleanup)))

       :on-close
       (fn [_ch _status]
         ;; Remove watcher
         (when-let [cleanup (get-in @session/sessions [session-id :tap-cleanup])]
           (cleanup))
         (session/remove-channel! session-id)
         (swap! session/sessions update session-id dissoc :tap-cleanup)
         ;; Schedule cleanup after timeout
         (session/schedule-cleanup! session-id cleanup-timeout-ms))}))))

(defmethod portal.runtime.jvm.server/route [:post "/ssr/toggle"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))
        value-id   (some-> (get params "v") parse-long)
        path       (some-> (get params "p")
                           viewer/url-safe-base64-decode
                           edn/read-string)]
    (if (and session-id value-id)
      (do
        (session/toggle! session-id value-id path)
        (let [state (session/get-state session-id)
              html  (viewer/render-node-html state value-id path :a)]
          ;; Return HTML directly — Datastar morphs by element ID
          {:status  200
           :headers {"Content-Type" "text/html"}
           :body    html}))
      {:status 400})))

(defmethod portal.runtime.jvm.server/route [:post "/ssr/show-more"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))
        value-id   (some-> (get params "v") parse-long)
        path       (some-> (get params "p")
                           viewer/url-safe-base64-decode
                           edn/read-string)]
    (if (and session-id (some? value-id))
      (do
        (session/show-more! session-id value-id path)
        (let [state (session/get-state session-id)
              html  (viewer/render-node-html state value-id path :a)]
          {:status  200
           :headers {"Content-Type" "text/html"}
           :body    html}))
      {:status 400})))

(defmethod portal.runtime.jvm.server/route [:post "/ssr/select"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))
        value-id   (some-> (get params "v") parse-long)
        path       (some-> (get params "p")
                           viewer/url-safe-base64-decode
                           edn/read-string)]
    (if (and session-id (some? value-id))
      (do
        (session/nav-push! session-id {:value-id value-id :path path})
        (let [state (session/get-state session-id)
              html  (viewer/render-page-html state :a)]
          {:status  200
           :headers {"Content-Type" "text/html"}
           :body    html}))
      {:status 400})))

(defmethod portal.runtime.jvm.server/route [:post "/ssr/nav-pop-to"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))
        n          (some-> (get params "n") parse-long)]
    (if (and session-id (some? n))
      (do
        (session/nav-pop-to! session-id n)
        (let [state (session/get-state session-id)
              html  (viewer/render-page-html state :a)]
          {:status  200
           :headers {"Content-Type" "text/html"}
           :body    html}))
      {:status 400})))

;; -- Proto B: Self-made (WebSocket) --

(defmethod portal.runtime.jvm.server/route [:get "/ssr/b"] [_request]
  (let [session-id (UUID/randomUUID)]
    (session/ensure! session-id)
    (session/snapshot-existing! session-id)
    (let [state     (session/get-state session-id)
          body-html (viewer/render-page-html state :b)]
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    (index/html
                 {:code-url     nil
                  :extra-head   (str "<script>window.SSR_SESSION=\"" session-id "\";</script>"
                                     "<script defer src=\"/ssr/client.js\"></script>")
                  :body-content body-html})})))

(defmethod portal.runtime.jvm.server/route [:get "/ssr/ws"] [request]
  (let [params     (parse-query-params (:query-string request))
        session-id (try-parse-uuid (get params "s"))]
    (http/as-channel request
      {:on-open
       (fn [ch]
         (session/ensure! session-id)
         (session/touch! session-id)
         (session/set-channel! session-id ch)
         ;; Reconcile
         (reconcile! session-id)
         ;; Push full page state
         (send-full! :b session-id ch)
         ;; Register watchers
         (let [cleanup-taps (session/watch-taps! session-id
                              (fn [sid vid _value]
                                (when-let [ws-ch (session/get-channel sid)]
                                  (let [state (session/get-state sid)]
                                    (when (empty? (:nav-stack state))
                                      (let [html (viewer/render-value-html state vid :b)]
                                        (http/send! ws-ch
                                          (json/write {"op" "prepend" "id" "tap-list" "html" html})
                                          false)))))))
               cleanup-tap-list (session/watch-tap-list! session-id
                                  (fn [sid _old-taps _new-taps]
                                    (when-let [ws-ch (session/get-channel sid)]
                                      (reconcile! sid)
                                      (send-full! :b sid ws-ch))))
               cleanup (fn []
                         (cleanup-tap-list)
                         (cleanup-taps))]
           (swap! session/sessions assoc-in [session-id :tap-cleanup] cleanup)))

       :on-receive
       (fn [_ch message]
         (let [msg (json/read message {:key-fn keyword})
               op  (:op msg)]
           (case op
             "toggle"
             (let [value-id (some-> (:valueId msg) parse-long)
                   path     (some-> (:path msg)
                                    viewer/url-safe-base64-decode
                                    edn/read-string)]
               (when (and session-id value-id)
                 (session/toggle! session-id value-id path)
                 (when-let [ws-ch (session/get-channel session-id)]
                   (let [state   (session/get-state session-id)
                         node-id (str "n-" value-id
                                      (when (seq path)
                                        (str "-" (str/join "-" (map viewer/encode-segment path)))))
                         html    (viewer/render-node-html state value-id path :b)]
                     (http/send! ws-ch
                       (json/write {"op" "replace" "id" node-id "html" html})
                       false)))))

             "show-more"
             (let [value-id (some-> (:valueId msg) parse-long)
                   path     (some-> (:path msg)
                                    viewer/url-safe-base64-decode
                                    edn/read-string)]
               (when (and session-id (some? value-id))
                 (session/show-more! session-id value-id path)
                 (when-let [ws-ch (session/get-channel session-id)]
                   (let [state   (session/get-state session-id)
                         node-id (str "n-" value-id
                                      (when (seq path)
                                        (str "-" (str/join "-" (map viewer/encode-segment path)))))
                         html    (viewer/render-node-html state value-id path :b)]
                     (http/send! ws-ch
                       (json/write {"op" "replace" "id" node-id "html" html})
                       false)))))

             "select"
             (let [value-id (some-> (:valueId msg) parse-long)
                   path     (some-> (:path msg)
                                    viewer/url-safe-base64-decode
                                    edn/read-string)]
               (when (and session-id (some? value-id))
                 (session/nav-push! session-id {:value-id value-id :path path})
                 (when-let [ws-ch (session/get-channel session-id)]
                   (let [state (session/get-state session-id)
                         html  (viewer/render-page-html state :b)]
                     (http/send! ws-ch
                       (json/write {"op" "full" "id" "tap-list" "html" html})
                       false)))))

             "nav-pop-to"
             (let [n (some-> (:index msg) parse-long)]
               (when (and session-id (some? n))
                 (session/nav-pop-to! session-id n)
                 (when-let [ws-ch (session/get-channel session-id)]
                   (let [state (session/get-state session-id)
                         html  (viewer/render-page-html state :b)]
                     (http/send! ws-ch
                       (json/write {"op" "full" "id" "tap-list" "html" html})
                       false)))))

             nil)))

       :on-close
       (fn [_ch _status]
         (when-let [cleanup (get-in @session/sessions [session-id :tap-cleanup])]
           (cleanup))
         (session/remove-channel! session-id)
         (swap! session/sessions update session-id dissoc :tap-cleanup)
         (session/schedule-cleanup! session-id cleanup-timeout-ms))})))

(defmethod portal.runtime.jvm.server/route [:get "/ssr/client.js"] [_request]
  {:status  200
   :headers {"Content-Type" "text/javascript"}
   :body    (slurp (io/resource "portal-ssr/client.js"))})
