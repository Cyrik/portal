(ns tasks.tools
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [io.aviso.ansi :as a])
  (:import [java.time Duration]))

(def ^:dynamic *cwd* nil)
(def ^:dynamic *opts* nil)

(defn- now [] (System/currentTimeMillis))

(defn- format-millis [ms]
  (let [duration (Duration/ofMillis ms)
        h        (mod (.toHours duration) 24)
        m        (mod (.toMinutes duration) 60)
        s        (mod (.toSeconds duration) 60)
        ms       (mod ms 1000)]
    (str
     (a/bold-blue "->")
     " "
     (a/bold-yellow
      (str
       (when (> h 0)
         (str h " hours, "))
       (when (> m 0)
         (str h " minutes, "))
       s "." ms " seconds")))))

(def ^:private in-bb? (some? (System/getProperty "babashka.version")))

(def ^:private fns
  (when in-bb? {:clojure (requiring-resolve 'babashka.deps/clojure)}))

(defn sh [& args]
  (println (a/bold-blue "=>")
           (a/bold-green (name (first args)))
           (a/bold (str/join " " (map name (rest args)))))
  (let [opts   (merge {:dir      *cwd*
                       :shutdown p/destroy-tree}
                      (merge *opts* (when-not (:inherit *opts*)
                                      {:out *out* :err *err*})))
        start  (now)
        result @(if-let [f (get fns (first args))]
                  (f (map name (rest args)) opts)
                  (p/process (map name args) opts))
        exit   (:exit result)]
    (when-not (:inherit opts)
      (.flush *out*)
      (.flush *err*))
    (println
     (str (format-millis (- (now) start))
          (when-not (zero? exit)
            (str " " (a/bold-red (str "(exit: " exit ")"))))
          "\n"))
    (when-not (zero? exit)
      (throw (ex-info "Non-zero exit code"
                      (assoc (select-keys result [:cmd :exit]) :opts *opts*)))))
  true)

(def bb     (partial #'sh :bb))
(def clj    (partial #'sh :clojure))
(def git    (partial #'sh :git))
(def gradle (partial #'sh "./gradlew" "--warning-mode" "all"))
(def node   (partial #'sh :node))
(def npm    (partial #'sh :npm))
(def npx    (partial #'sh :npx))
(def shadow (partial #'clj "-M:cljs:shadow" "-m" "shadow.cljs.devtools.cli"))