(ns dinero.core
  (:refer-clojure :exclude [format])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dinero.utils :as utils])
  (:import [java.text DecimalFormat DecimalFormatSymbols]
           [java.util Currency Locale]))

(set! *warn-on-reflection* true)

;;; Configuration

(defonce config (utils/read-config "config.edn"))

(def ^:dynamic *default-currency* (:default-currency config))
(def ^:dynamic *default-rounding-mode* (:default-rounding-mode config))

;;; Currencies

(defonce currencies (-> "currencies.edn" io/resource slurp edn/read-string))

(defn- get-currency-code
  "Returns the currency code of the given currency."
  [currency]
  (get-in currencies [currency :currency-code]))

(defn- get-type
  "Returns the type of the given currency."
  [currency]
  (get-in currencies [currency :type]))

(defn- get-minor-unit
  "Returns the minor unit of the given currency."
  [currency]
  (get-in currencies [currency :minor-unit]))

(defn- get-symbol
  "Returns the currency symbol of the given currency."
  [currency]
  (get-in currencies [currency :symbol]))

(defn- iso-4217?
  "Returns true if the given currency is an ISO 4217 currency."
  [currency]
  (= :iso-4217 (get-type currency)))

(defn- assert-currency
  "Asserts that the given currency is a valid currency."
  [currency]
  (when-not currency
    (throw (ex-info "No currency provided" {})))
  (when-not (contains? currencies currency)
    (throw (ex-info (str "Unknown currency") {:currency currency}))))

;;; Money creation

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  ([]
   (money-of 0 *default-currency*))
  ([amount]
   (money-of amount *default-currency*))
  ([amount currency]
   (assert-currency currency)
   {:amount (bigdec amount) :currency currency}))

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (:amount money))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))

;;; Formatting

(defn- make-formatter
  "Creates a new formatter for the given currency, locale, rounding mode, and decimal places."
  [currency locale rounding-mode decimal-places]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)]
    (doto formatter
      (.setRoundingMode rounding-mode)
      (.setMinimumFractionDigits decimal-places)
      (.setMaximumFractionDigits decimal-places))
    (when (iso-4217? currency)
      (.setCurrency formatter (Currency/getInstance ^String (get-currency-code currency))))
    formatter))

(defn- format-amount
  "Formats the given amount with the given formatter, locale, and symbol style."
  [amount currency formatter locale symbol-style]
  (let [formatted-money (.format ^DecimalFormat formatter amount)
        locale-currency (Currency/getInstance ^Locale locale)
        locale-code (.getCurrencyCode locale-currency)
        locale-symbol (.getSymbol locale-currency locale)
        currency-code (get-currency-code currency)
        currency-symbol (or (get-symbol currency) currency-code)] ; default to code if symbol is not available
    (cond
      ;; ISO 4217 currency with symbol-style :symbol
      (and (iso-4217? currency) (= :symbol symbol-style))
      formatted-money
      ;; ISO 4217 currency with symbol-style :code
      (and (iso-4217? currency) (= :code symbol-style))
      (str/replace formatted-money locale-symbol locale-code)
      ;; Non-ISO 4217 currency with symbol-style :symbol
      (and (not (iso-4217? currency)) (= :symbol symbol-style))
      (str/replace formatted-money locale-symbol currency-symbol)
      ;; Non-ISO 4217 currency with symbol-style :code
      (and (not (iso-4217? currency)) (= :code symbol-style))
      (str/replace formatted-money locale-symbol currency-code)
      ;; Invalid symbol style
      :else(throw (ex-info "Invalid symbol style" {:symbol-style symbol-style})))))

(defn format
  "Formats the given monetary amount with the given options."
  [money & {:keys [locale rounding-mode decimal-places symbol-style] :as _options}]
  (let [amount (get-amount money)
        currency (get-currency money)
        locale (or locale (Locale/getDefault))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode *default-rounding-mode* :half-even))
        decimal-places (or decimal-places (get-minor-unit currency))
        symbol-style (or symbol-style :symbol)
        formatter (make-formatter currency locale rounding-mode decimal-places)]
    (format-amount amount currency formatter locale symbol-style)))

(defn format-with-pattern
  "Formats the given monetary amount with the given pattern and options."
  [money pattern & {:keys [locale rounding-mode] :as _options}]
  (let [amount (get-amount money)
        locale (or locale (Locale/getDefault))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode *default-rounding-mode* :half-even))
        symbols (DecimalFormatSymbols/getInstance locale)
        formatter (DecimalFormat. pattern symbols)]
    (.setRoundingMode formatter rounding-mode)
    (.format ^DecimalFormat formatter amount)))

;;; Parsing

(defn parse-iso-4217
  "Parses the given string, which represents a monetary amount that uses a ISO 4217 currency."
  [string & {:keys [locale] :as _options}]
  (let [locale (or locale (Locale/getDefault))
        formatter (DecimalFormat/getCurrencyInstance locale)
        amount (.parse ^DecimalFormat formatter string)
        currency (-> (.getCurrency formatter) str/lower-case keyword)]
    (money-of amount currency)))
