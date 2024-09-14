(ns dinero.parse
  (:require [dinero.core :as core]
            [dinero.currency :as currency]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.text DecimalFormat DecimalFormatSymbols ParseException]
           [java.util Currency Locale]))

(defn- get-currency-symbol
  "Returns the currency symbol or code for the given currency and locale."
  [locale currency currency-style]
  (cond
    (= currency-style :symbol)
    (currency/get-symbol currency locale)

    (= currency-style :code)
    (currency/get-currency-code currency)

    :else (throw (ex-info "Currency style not supported" {:currency-style :currency-style}))))

(defn- get-currency-formatter
  "Returns a currency formatter for the given locale, currency and currency style."
  [locale currency currency-style]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)
        symbols (DecimalFormat/.getDecimalFormatSymbols formatter)
        currency-symbol (get-currency-symbol locale currency currency-style)]
    (DecimalFormat/.setParseBigDecimal formatter true)
    (DecimalFormatSymbols/.setCurrencySymbol symbols currency-symbol)
    (DecimalFormat/.setDecimalFormatSymbols formatter symbols)
    formatter))

(defn parse-with-symbol-or-code
  "Parses a monetary string using currency symbol or code."
  [string locale currency]
  (try
    (let [formatter (get-currency-formatter locale currency :symbol)
          amount (DecimalFormat/.parse ^DecimalFormat formatter string)]
      (core/money-of amount currency))
    (catch Exception _e
      (let [formatter (get-currency-formatter locale currency :code)
            amount (DecimalFormat/.parse ^DecimalFormat formatter string)]
        (core/money-of amount currency)))))

(defn attempt-parse-with-multiple-currencies
  "Tries to parse a monetary string using a list of currencies."
  [string locale currencies]
  (or (some #(try (parse-with-symbol-or-code string locale %)
                  (catch ParseException e
                    (log/warn (str (Exception/.getMessage e) " for locale " locale " and currency " %))))
            currencies)
      (throw (ParseException. (str "Unparseable number: \"" string "\"") 0))))

(defn attempt-parse-with-all-currencies
  "Tries to parse a monetary string using all available currencies in the `resources/currencies.edn` file."
  [string locale]
  (attempt-parse-with-multiple-currencies string locale (currency/get-all-currencies)))

(defn parse-money
  "Parses a monetary string with optional locale and currency settings."
  [string & {:keys [locale currencies try-all-currencies?] :as _options}]
  (let [locale (or locale (Locale/getDefault))
        locale-currency (-> (Currency/.getCurrencyCode (Currency/getInstance locale))
                            str/lower-case
                            keyword)
        currencies (or currencies [core/*default-currency*] [locale-currency])
        try-all-currencies? (or try-all-currencies? false)]
    (try
      (attempt-parse-with-multiple-currencies string locale currencies)
      (catch ParseException e
        (if try-all-currencies?
          (attempt-parse-with-all-currencies string locale)
          (throw e))))))
