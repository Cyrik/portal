(ns portal.ssr.viewer
  (:require [clojure.string :as str]
            [portal.colors :as c]
            [portal.ssr.html :as html]))

;; -- Depth color cycling (matches portal.ui.theme/order) --

(def ^:private color-order
  [::c/diff-remove ::c/diff-add ::c/keyword ::c/tag ::c/number ::c/uri])

(defn- depth-color [ctx]
  (let [theme (:theme ctx)
        idx   (mod (:depth ctx) (count color-order))]
    (get theme (nth color-order idx))))

;; -- Path encoding for DOM IDs --

(defn encode-segment [x]
  (cond
    (keyword? x) (str "k" (when (namespace x) (str (namespace x) "."))
                      (name x))
    (string? x)  (str "s" (str/replace x #"[^a-zA-Z0-9]" "_"))
    (symbol? x)  (str "y" (str x))
    :else         (str x)))

(defn- node-id [value-id path]
  (str "n-" value-id
       (when (seq path)
         (str "-" (str/join "-" (map encode-segment path))))))

;; -- URL-safe Base64 for path encoding in events --

#?(:clj
   (defn url-safe-base64-encode [^String s]
     (-> (java.util.Base64/getUrlEncoder)
         (.withoutPadding)
         (.encodeToString (.getBytes s "UTF-8")))))

#?(:clj
   (defn url-safe-base64-decode [^String s]
     (String. (.decode (java.util.Base64/getUrlDecoder) s) "UTF-8")))

;; -- Cycle detection --

(defn- navigable? [value]
  (or (map? value)
      #?(:clj (instance? java.util.Map value))
      (coll? value)
      (and (seqable? value) (not (string? value)))))

(defn- identity-hash [value]
  #?(:clj  (System/identityHashCode value)
     :cljs (hash value)))

(defn- cycle? [ctx value]
  (when (navigable? value)
    (contains? (:ancestors ctx) (identity-hash value))))

(defn- with-ancestor [ctx value]
  (update ctx :ancestors conj (identity-hash value)))

;; -- Value navigation --

(defn- get-in-value [value path]
  (reduce
   (fn [v k]
     (cond
       (nil? v) nil
       (map? v) (get v k)
       #?@(:clj [(instance? java.util.Map v) (.get ^java.util.Map v k)])
       (and (sequential? v) (int? k)) (nth v k nil)
       (set? v) (nth (seq v) k nil)
       (seqable? v) (nth (seq v) k nil)
       :else nil))
   value
   path))

(defn- build-ancestors
  "Walk from root to parent of target path, collecting identity hashes."
  [root-value path]
  (loop [v         root-value
         remaining (butlast path)
         ancestors #{}]
    (if (or (nil? remaining) (empty? remaining))
      (if (navigable? v)
        (conj ancestors (identity-hash v))
        ancestors)
      (let [ancestors (if (navigable? v)
                        (conj ancestors (identity-hash v))
                        ancestors)
            k         (first remaining)
            next-v    (cond
                        (map? v) (get v k)
                        #?@(:clj [(instance? java.util.Map v) (.get ^java.util.Map v k)])
                        (and (sequential? v) (int? k)) (nth v k nil)
                        (seqable? v) (nth (seq v) k nil)
                        :else nil)]
        (recur next-v (rest remaining) ancestors)))))

;; -- Map entry iteration --

(defn- map-entries [value]
  #?(:clj
     (if (instance? java.util.Map value)
       (map (fn [^java.util.Map$Entry e] [(.getKey e) (.getValue e)])
            (.entrySet ^java.util.Map value))
       (seq value))
     :cljs (seq value)))

;; -- Pagination --

(def ^:private default-limit 20)

(defn- effective-limit [ctx]
  (get (:limits ctx) [(:value-id ctx) (:path ctx)] default-limit))

(defn- item-count-label [total]
  (if (> total 1000) "1000+ items" (str total " items")))

;; -- Toggle button --

(defn- toggle-btn [ctx expanded?]
  (let [session-id (:session-id ctx)
        value-id   (:value-id ctx)
        path       (:path ctx)
        path-enc   (url-safe-base64-encode (pr-str path))]
    (case (:proto ctx)
      :a [:span {"data-on:click"
                 (str "@post('/ssr/toggle?s=" session-id
                      "&v=" value-id
                      "&p=" path-enc "')")
                 :style {:cursor      "pointer"
                         :user-select "none"
                         :padding     "0 4px"
                         :font-size   "0.8em"}}
          (if expanded? "\u25BC" "\u25B6")]
      :b [:span {:data-action   "toggle"
                 :data-value-id (str value-id)
                 :data-path     path-enc
                 :style         {:cursor      "pointer"
                                 :user-select "none"
                                 :padding     "0 4px"
                                 :font-size   "0.8em"}}
          (if expanded? "\u25BC" "\u25B6")])))

;; -- Show-more button --

(defn- show-more-btn [ctx]
  (let [session-id (:session-id ctx)
        value-id   (:value-id ctx)
        path       (:path ctx)
        path-enc   (url-safe-base64-encode (pr-str path))
        theme      (:theme ctx)]
    (case (:proto ctx)
      :a [:div {"data-on:click" (str "@post('/ssr/show-more?s=" session-id
                                     "&v=" value-id "&p=" path-enc "')")
                :style {:cursor "pointer" :color (::c/uri theme)
                        :padding "2px 0" :font-size "0.9em"}}
          "show more\u2026"]
      :b [:div {:data-action   "show-more"
                :data-value-id (str value-id)
                :data-path     path-enc
                :style {:cursor "pointer" :color (::c/uri theme)
                        :padding "2px 0" :font-size "0.9em"}}
          "show more\u2026"])))

;; -- Scalar rendering --

(defn- scalar [ctx value]
  (let [theme (:theme ctx)
        [color text]
        (cond
          (nil? value)     [(::c/text theme) "nil"]
          (boolean? value) [(::c/boolean theme) (str value)]
          (number? value)  [(::c/number theme) (str value)]
          (string? value)  [(::c/string theme) (pr-str value)]
          (keyword? value) [(::c/keyword theme) (str value)]
          (symbol? value)  [(::c/symbol theme) (str value)]
          (uuid? value)    [(::c/tag theme) (str "#uuid \"" value "\"")]
          :else            [(::c/text theme) (pr-str value)])]
    [:span {:style {:color color}} text]))

;; -- Tree rendering --

(declare inspect)

;; -- Select summary (clickable collapsed count) --

(defn- select-summary [ctx label]
  (let [session-id (:session-id ctx)
        value-id   (:value-id ctx)
        path       (:path ctx)
        path-enc   (url-safe-base64-encode (pr-str path))]
    (case (:proto ctx)
      :a [:span {"data-on:click" (str "@post('/ssr/select?s=" session-id
                                      "&v=" value-id "&p=" path-enc "')")
                 :style {:cursor "pointer" :color (::c/text (:theme ctx))
                         :opacity 0.5}}
          label]
      :b [:span {:data-action   "select"
                 :data-value-id (str value-id)
                 :data-path     path-enc
                 :style {:cursor "pointer" :color (::c/text (:theme ctx))
                         :opacity 0.5}}
          label])))

(defn- expanded-key [ctx]
  [(:value-id ctx) (:path ctx)])

(defn- is-expanded? [ctx]
  (contains? (:expanded ctx) (expanded-key ctx)))

(defn- tree-map [ctx value]
  (let [expanded  (is-expanded? ctx)
        bracket-c (depth-color ctx)
        total     (count value)
        ctx       (with-ancestor ctx value)]
    [:div {:id (node-id (:value-id ctx) (:path ctx))}
     (toggle-btn ctx expanded)
     [:span {:style {:color bracket-c}} "{"]
     (if-not expanded
       [:<>
        (select-summary ctx (str total " items"))
        [:span {:style {:color bracket-c}} "}"]]
       (let [limit    (effective-limit ctx)
             entries  (take limit (map-entries value))
             has-more (> total limit)]
         [:<>
          [:div {:style {:padding-left "1.2em"}}
           (for [[k v] entries]
             (let [child-path (conj (:path ctx) k)
                   child-ctx  (-> ctx
                                  (assoc :path child-path)
                                  (update :depth inc))]
               [:div {:key (str "e-" (encode-segment k))
                      :style {:display "flex" :flex-wrap "wrap" :gap "0 4px"}}
                (scalar ctx k)
                (inspect child-ctx v)]))
           (when has-more
             (show-more-btn ctx))]
          [:span {:style {:color bracket-c}} "}"]]))]))

(defn- coll-brackets [value]
  (cond
    (vector? value) ["[" "]"]
    (set? value)    ["#{" "}"]
    (list? value)   ["(" ")"]
    :else           ["(" ")"]))

(defn- tree-coll [ctx value]
  (let [expanded     (is-expanded? ctx)
        bracket-c    (depth-color ctx)
        [open close] (coll-brackets value)
        ctx          (with-ancestor ctx value)]
    [:div {:id (node-id (:value-id ctx) (:path ctx))}
     (toggle-btn ctx expanded)
     [:span {:style {:color bracket-c}} open]
     (if-not expanded
       (let [total (bounded-count 1001 value)]
         [:<>
          (select-summary ctx (item-count-label total))
          [:span {:style {:color bracket-c}} close]])
       (let [limit    (effective-limit ctx)
             items+1  (vec (take (inc limit) (seq value)))
             items    (take limit items+1)
             has-more (> (count items+1) limit)]
         [:<>
          [:div {:style {:padding-left "1.2em"}}
           (for [[idx v] (map-indexed vector items)]
             (let [child-path (conj (:path ctx) idx)
                   child-ctx  (-> ctx
                                  (assoc :path child-path)
                                  (update :depth inc))]
               [:div {:key (str "i-" idx)}
                (inspect child-ctx v)]))
           (when has-more
             (show-more-btn ctx))]
          [:span {:style {:color bracket-c}} close]]))]))

(defn- inspect [ctx value]
  (cond
    (cycle? ctx value)
    [:span {:style {:color (::c/exception (:theme ctx))}} "\u21BB cycle"]

    (map? value)
    (tree-map ctx value)

    #?@(:clj [(instance? java.util.Map value)
              (tree-map ctx value)])

    (coll? value)
    (tree-coll ctx value)

    (and (seqable? value) (not (string? value)))
    (tree-coll ctx (seq value))

    :else
    (scalar ctx value)))

;; -- Public API --

(def ^:private default-theme (get c/themes ::c/nord))

(defn- make-ctx [session-state value-id path ancestors proto]
  {:session-id (:session-id session-state)
   :value-id   value-id
   :path       path
   :depth      (count path)
   :theme      default-theme
   :ancestors  ancestors
   :proto      proto
   :limits     (:limits session-state)
   :expanded   (:expanded session-state)})

;; -- Breadcrumb --

(defn- breadcrumb-label [{:keys [value-id path]}]
  (if (empty? path)
    (str "#" value-id)
    (pr-str (last path))))

(defn- nav-pop-btn [session-id proto n label & [bold?]]
  (if bold?
    [:span {:style {:font-weight "bold" :color (::c/text default-theme)}} label]
    (case proto
      :a [:span {"data-on:click" (str "@post('/ssr/nav-pop-to?s=" session-id "&n=" n "')")
                 :style {:cursor "pointer" :color (::c/uri default-theme)}}
          label]
      :b [:span {:data-action "nav-pop-to" :data-index (str n)
                 :style {:cursor "pointer" :color (::c/uri default-theme)}}
          label])))

(defn- breadcrumb-bar [session-state nav-stack proto]
  (let [session-id (:session-id session-state)
        n          (count nav-stack)]
    [:div {:style {:padding       "6px 0"
                   :margin-bottom "8px"
                   :border-bottom (str "1px solid " (::c/border default-theme))
                   :display       "flex"
                   :gap           "4px"
                   :align-items   "center"
                   :flex-wrap     "wrap"
                   :font-size     "0.9em"}}
     (nav-pop-btn session-id proto 0 "taps")
     (for [[idx entry] (map-indexed vector nav-stack)]
       (let [last? (= idx (dec n))]
         [:span {:key (str "bc-" idx)}
          [:span {:style {:color (::c/text default-theme) :opacity 0.5}} " / "]
          (nav-pop-btn session-id proto (inc idx) (breadcrumb-label entry) last?)]))]))

;; -- Tap-list select button --

(defn- tap-select-btn [ctx]
  (let [session-id (:session-id ctx)
        value-id   (:value-id ctx)
        path-enc   (url-safe-base64-encode (pr-str []))]
    (case (:proto ctx)
      :a [:span {"data-on:click" (str "@post('/ssr/select?s=" session-id
                                      "&v=" value-id "&p=" path-enc "')")
                 :style {:cursor "pointer" :padding "0 4px"
                         :color (::c/uri (:theme ctx)) :user-select "none"}}
          "\u2192"]
      :b [:span {:data-action   "select"
                 :data-value-id (str value-id)
                 :data-path     path-enc
                 :style {:cursor "pointer" :padding "0 4px"
                         :color (::c/uri (:theme ctx)) :user-select "none"}}
          "\u2192"])))

;; -- Page rendering --

(defn render-page
  "Render the full tap list or detail view. Returns hiccup."
  [session-state proto]
  (let [{:keys [order values nav-stack]} session-state]
    [:div {:id "tap-list"
           :style {:padding     "8px"
                   :font-family "monospace"
                   :font-size   "12pt"
                   :color       (::c/text default-theme)
                   :background  (::c/background default-theme)
                   :min-height  "100vh"}}
     (if (seq nav-stack)
       ;; Detail view
       (let [{:keys [value-id path]} (peek nav-stack)
             root-value (get values value-id)
             value      (if (empty? path) root-value (get-in-value root-value path))
             ancestors  (if (empty? path) #{} (build-ancestors root-value path))
             ctx        (make-ctx session-state value-id path ancestors proto)]
         [:<>
          (breadcrumb-bar session-state nav-stack proto)
          (inspect ctx value)])
       ;; Tap-list view
       (if (empty? order)
         [:div {:data-placeholder true
                :style {:color   (::c/text default-theme)
                        :opacity 0.5
                        :padding "2em"
                        :text-align "center"}}
          "Waiting for tap> values..."]
         (for [vid order]
           (let [value (get values vid)
                 ctx   (make-ctx session-state vid [] #{} proto)]
             [:div {:id    (str "v-" vid)
                    :key   (str "v-" vid)
                    :style {:margin-bottom  "8px"
                            :border-bottom  (str "1px solid " (::c/border default-theme))
                            :padding-bottom "8px"
                            :display        "flex"
                            :align-items    "flex-start"
                            :gap            "4px"}}
              (tap-select-btn ctx)
              [:div {:style {:flex "1"}}
               (inspect ctx value)]]))))]))

(defn render-value
  "Render a single tapped value. Returns hiccup."
  [session-state value-id proto]
  (let [value (get-in session-state [:values value-id])
        ctx   (make-ctx session-state value-id [] #{} proto)]
    [:div {:id    (str "v-" value-id)
           :key   (str "v-" value-id)
           :style {:margin-bottom  "8px"
                   :border-bottom  (str "1px solid " (::c/border default-theme))
                   :padding-bottom "8px"
                   :display        "flex"
                   :align-items    "flex-start"
                   :gap            "4px"}}
     (tap-select-btn ctx)
     [:div {:style {:flex "1"}}
      (inspect ctx value)]]))

(defn render-node
  "Render a single tree node (for toggle re-render). Returns hiccup."
  [session-state value-id path proto]
  (let [root-value (get-in session-state [:values value-id])
        value      (if (empty? path) root-value (get-in-value root-value path))
        ancestors  (if (empty? path) #{} (build-ancestors root-value path))
        ctx        (make-ctx session-state value-id path ancestors proto)]
    (inspect ctx value)))

(defn render-page-html [session-state proto]
  (html/render (render-page session-state proto)))

(defn render-value-html [session-state value-id proto]
  (html/render (render-value session-state value-id proto)))

(defn render-node-html [session-state value-id path proto]
  (html/render (render-node session-state value-id path proto)))
