(ns portal.ssr.html
  (:require [clojure.string :as str]))

(def ^:private void-elements
  #{:br :hr :img :input :meta :link :area :base :col :embed :source :track :wbr})

(defn- escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- attr-name [k]
  (if (keyword? k) (name k) (str k)))

(defn- value->css [v]
  (cond
    (and (number? v) (not (zero? v))) (str v "px")
    (number? v)  (str v)
    (keyword? v) (name v)
    (vector? v)  (str/join " " (map value->css v))
    :else        (str v)))

(def ^:private css-unitless #{:opacity :z-index :flex :flex-grow :flex-shrink :order :font-weight
                              :grid-row :grid-column})

(defn- style->css [style]
  (reduce-kv
   (fn [css k v]
     (if (or (nil? k) (nil? v))
       css
       (str css (name k) ":"
            (if (css-unitless k) v (value->css v))
            ";")))
   "" style))

(defn- render-attr [k v]
  (cond
    (nil? v)    nil
    (false? v)  nil
    (= k :style) (when (and (map? v) (seq v))
                    (str " style=\"" (escape-html (style->css v)) "\""))
    (true? v)   (str " " (attr-name k))
    :else       (str " " (attr-name k) "=\"" (escape-html (str v)) "\"")))

(defn- render-attrs [attrs]
  (when (map? attrs)
    (reduce-kv (fn [s k v] (str s (render-attr k v))) "" attrs)))

(declare render)

(defn- render-children [children]
  (reduce (fn [s child] (str s (render child))) "" children))

(defn render [hiccup]
  (cond
    (nil? hiccup)    ""
    (string? hiccup) (escape-html hiccup)
    (number? hiccup) (str hiccup)
    (seq? hiccup)    (render-children hiccup)
    (vector? hiccup)
    (let [[tag & rest] hiccup]
      (if (= :<> tag)
        (let [rest (if (map? (first rest)) (next rest) rest)]
          (render-children rest))
        (let [[attrs children] (if (map? (first rest))
                                 [(first rest) (next rest)]
                                 [nil rest])]
          (if (void-elements tag)
            (str "<" (name tag) (render-attrs attrs) " />")
            (str "<" (name tag) (render-attrs attrs) ">"
                 (render-children children)
                 "</" (name tag) ">")))))
    :else (escape-html (str hiccup))))
