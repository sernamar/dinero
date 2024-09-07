(ns dinero.utils
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io File]
           [java.math RoundingMode]))

(defn keyword->rounding-mode
  "Returns the rounding mode for the given keyword."
  [mode]
  (case mode
    :up RoundingMode/UP
    :down RoundingMode/DOWN
    :ceiling RoundingMode/CEILING
    :floor RoundingMode/FLOOR
    :half-up RoundingMode/HALF_UP
    :half-down RoundingMode/HALF_DOWN
    :half-even RoundingMode/HALF_EVEN
    :unnecessary RoundingMode/UNNECESSARY
    (throw (ex-info "Invalid rounding mode" {:mode mode}))))

(defn read-config
  "Reads the configuration from the given pathname."
  [pathname]
  (when (File/.exists (File. ^String pathname))
      (-> pathname
          slurp
          edn/read-string)))

(defn to-uppercase-string
  "Returns the upper-case string representation of the given argument."
  [arg]
  (str/upper-case
   (cond
     (string? arg) arg
     (keyword? arg) (name arg)
     (symbol? arg) (name arg)
     :else (throw (ex-info "Invalid argument" {:arg arg})))))
