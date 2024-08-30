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

(defn- currency-code
  "Returns the currency code of the given currency."
  [currency]
  (get-in currencies [currency :currency-code]))

(defn- currency-type
  "Returns the type of the given currency."
  [currency]
  (get-in currencies [currency :type]))

(defn- minor-unit
  "Returns the minor unit of the given currency."
  [currency]
  (get-in currencies [currency :minor-unit]))

(defn- currency-symbol
  "Returns the currency symbol of the given currency."
  [currency]
  (get-in currencies [currency :symbol]))

(defn- iso-4217?
  "Returns true if the given currency is an ISO 4217 currency."
  [currency]
  (= :iso-4217 (currency-type currency)))

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
      (.setCurrency formatter (Currency/getInstance ^String (currency-code currency))))
    formatter))

(defn- replace-currency-symbol
  "Replaces the locale currency symbol with the currency symbol.

  If the currency has no symbol, the currency code is used instead."
  [string currency locale]
  (let [locale-currency (Currency/getInstance ^Locale locale)
        locale-symbol (.getSymbol locale-currency locale)
        currency-symbol (or (currency-symbol currency) (currency-code currency))]
    (str/replace string locale-symbol currency-symbol)))

(defn format
  "Formats the given monetary amount with the given options."
  [money & {:keys [locale rounding-mode decimal-places] :as _options}]
  (let [amount (get-amount money)
        currency (get-currency money)
        locale (or locale (Locale/getDefault))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode *default-rounding-mode* :half-even))
        decimal-places (or decimal-places (minor-unit currency))
        formatter (make-formatter currency locale rounding-mode decimal-places)
        formatted-money (.format ^DecimalFormat formatter amount)]
    (if (iso-4217? currency)
      formatted-money
      (replace-currency-symbol formatted-money currency locale))))

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
