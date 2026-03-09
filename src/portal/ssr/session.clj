(ns portal.ssr.session
  (:require [portal.runtime :as rt])
  (:import [java.util UUID]))

(defonce sessions (atom {}))

;; Ensure tap> flows into Portal's tap-list. add-tap uses a set,
;; so adding the same var is idempotent even if the user also calls it.
(defonce ^:private _ensure-portal-tap
  (do (require 'portal.api)
      (add-tap (resolve 'portal.api/submit))))

(defn ensure! [session-id]
  (get
   (swap! sessions
          (fn [s]
            (if (contains? s session-id)
              s
              (assoc s session-id
                     {:session-id session-id
                      :next-id    0
                      :tap-count  0
                      :order      []
                      :values     {}
                      :expanded   #{}
                      :limits     {}
                      :nav-stack  []}))))
   session-id))

(defn add-value! [session-id value]
  (let [vid (atom nil)]
    (swap! sessions
           (fn [s]
             (let [session (get s session-id)
                   id      (:next-id session)]
               (reset! vid id)
               (assoc s session-id
                      (-> session
                          (update :next-id inc)
                          (update :order #(into [id] %))
                          (assoc-in [:values id] value))))))
    @vid))

(defn toggle! [session-id value-id path]
  (swap! sessions
         (fn [s]
           (update-in s [session-id :expanded]
                      (fn [expanded]
                        (let [k [value-id path]]
                          (if (contains? expanded k)
                            (disj expanded k)
                            (conj (or expanded #{}) k)))))))
  nil)

(defn show-more! [session-id value-id path]
  (swap! sessions update-in [session-id :limits [value-id path]]
         (fn [current] (+ (or current 20) 20)))
  nil)

(defn nav-push! [session-id entry]
  (swap! sessions update-in [session-id :nav-stack]
         (fn [stack] (if (= entry (peek stack)) stack (conj stack entry))))
  nil)

(defn nav-pop-to! [session-id n]
  (swap! sessions update-in [session-id :nav-stack]
         (fn [stack] (vec (take n stack))))
  nil)

(defn get-state [session-id]
  (get @sessions session-id))

(defn set-channel! [session-id ch]
  (swap! sessions assoc-in [session-id :channel] ch))

(defn get-channel [session-id]
  (get-in @sessions [session-id :channel]))

(defn remove-channel! [session-id]
  (swap! sessions update session-id dissoc :channel))

(defn update-tap-count! [session-id n]
  (swap! sessions assoc-in [session-id :tap-count] n))

(defonce ^:private cleanup-futs (atom {}))

(defn- cancel-cleanup! [session-id]
  (when-let [fut (get @cleanup-futs session-id)]
    (future-cancel fut)
    (swap! cleanup-futs dissoc session-id)))

(defn schedule-cleanup! [session-id timeout-ms]
  (cancel-cleanup! session-id)
  (let [fut (future
              (Thread/sleep timeout-ms)
              (swap! sessions dissoc session-id)
              (swap! cleanup-futs dissoc session-id))]
    (swap! cleanup-futs assoc session-id fut)))

(defn touch! [session-id]
  (cancel-cleanup! session-id))

(defn snapshot-existing! [session-id]
  (let [taps (reverse @@#'rt/tap-list)]
    (doseq [v taps]
      (add-value! session-id v))
    (swap! sessions assoc-in [session-id :tap-count] (count taps))))

(defn watch-taps! [session-id callback]
  (let [f (fn [value]
            (let [vid (add-value! session-id value)]
              (swap! sessions update-in [session-id :tap-count] inc)
              (callback session-id vid value)))]
    (add-tap f)
    (fn [] (remove-tap f))))

(defn cleanup! [session-id]
  (cancel-cleanup! session-id)
  (swap! sessions dissoc session-id))
