;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.utils
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io File]
           [java.math RoundingMode]
           [java.util Currency Locale]))

(set! *warn-on-reflection* true)

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
     (or (keyword? arg) (symbol? arg)) (name arg)
     :else (throw (ex-info "Invalid argument" {:arg arg})))))

(defn get-locale-currency
  "Returns the currency for the given locale."
  [locale]
  (keyword (str/lower-case (Currency/.getCurrencyCode (Currency/getInstance ^Locale locale)))))

(defn to-lowercase-keyword
  "Returns the lower-case keyword representation of the given argument."
  [arg]
  (keyword (str/lower-case
            (cond
              (string? arg) arg
              (or (keyword? arg) (symbol? arg)) (name arg)
              :else (throw (ex-info "Invalid argument" {:arg arg}))))))
