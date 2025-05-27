;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.parse
  (:require [dinero.core :as core]
            [dinero.currency :as currency]
            [clojure.string :as str]
            [dinero.utils :as utils])
  (:import [java.text DecimalFormat DecimalFormatSymbols ParseException]
           [java.util Locale]))

(set! *warn-on-reflection* true)

(defn- get-currency-symbol
  "Returns the currency symbol or code for the given currency and locale."
  [locale currency currency-style]
  (cond
    (= currency-style :symbol)
    (currency/get-symbol currency locale)

    (= currency-style :code)
    (currency/get-currency-code currency)

    :else (throw (ex-info "Currency style not supported" {:currency-style :currency-style}))))

(defn- make-formatter
  "Creates a new formatter for the given locale, currency and currency style."
  [locale currency currency-style]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)
        symbols (DecimalFormat/.getDecimalFormatSymbols formatter)
        currency-symbol (get-currency-symbol locale currency currency-style)]
    (DecimalFormat/.setParseBigDecimal formatter true)
    (DecimalFormatSymbols/.setCurrencySymbol symbols currency-symbol)
    (DecimalFormat/.setDecimalFormatSymbols formatter symbols)
    formatter))

(defn valid-grouping?
  "Checks if the grouping is valid."
  [string locale]
  (let [formatter (DecimalFormat/getInstance locale)
        symbols (DecimalFormat/.getDecimalFormatSymbols formatter)
        grouping-separator (DecimalFormatSymbols/.getGroupingSeparator symbols)
        grouping-size (DecimalFormat/.getGroupingSize formatter)
        index (str/last-index-of string grouping-separator)]
    (or (nil? index) ; return true if no grouping separator found
        (= grouping-size (count (take-while #(Character/isDigit ^char %) (subs string (inc index)))))))) ; count digits after grouping separator and check if it matches grouping size

(defn assert-valid-grouping
  "Asserts that the grouping is valid."
  [string locale]
  (when-not (valid-grouping? string locale)
    (throw (ParseException. (str "Unparseable number: \"" string "\"") 0))))

(defn parse-string-with-symbol-or-code
  "Parses a monetary string using currency symbol or code."
  [string locale currency]
  (assert-valid-grouping string locale)
  (let [string (str/replace string #"\s+" " ")] ; replace regular spaces with a non-breaking space
    (try
      (let [formatter (make-formatter locale currency :symbol)
            amount (DecimalFormat/.parse ^DecimalFormat formatter string)]
        (core/money-of amount currency))
      (catch Exception _e
        (let [formatter (make-formatter locale currency :code)
              amount (DecimalFormat/.parse ^DecimalFormat formatter string)]
          (core/money-of amount currency))))))

(defn attempt-parse-string-with-multiple-currencies
  "Tries to parse a monetary string using a list of currencies."
  [string locale currencies]
  (or (some #(try (parse-string-with-symbol-or-code string locale %)
                  (catch ParseException _e
                    nil)) ;; ignore parse exceptions
            currencies)
      (throw (ParseException. (str "Unparseable number: \"" string "\"") 0))))

(defn attempt-parse-string-with-all-currencies
  "Tries to parse a monetary string using all available currencies in the `resources/currencies.edn` file."
  [string locale]
  (attempt-parse-string-with-multiple-currencies string locale (currency/get-all-currencies)))

(defn parse-string
  "Parses a monetary string with optional locale and currency settings."
  [string & {:keys [locale currencies try-all-currencies?] :as _options}]
  (let [locale (or locale (Locale/getDefault))
        locale-currency (utils/get-locale-currency locale)
        currencies (or currencies [core/*default-currency* locale-currency])
        try-all-currencies? (or try-all-currencies? false)]
    (try
      (attempt-parse-string-with-multiple-currencies string locale currencies)
      (catch ParseException e
        (if try-all-currencies?
          (attempt-parse-string-with-all-currencies string locale)
          (throw e))))))
