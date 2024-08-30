(ns dinero.core
  (:refer-clojure :exclude [format])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.math RoundingMode]
           [java.text NumberFormat]
           [java.util Currency Locale]))

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
  [amount currency]
  (assert-currency currency)
  {:amount (bigdec amount) :currency currency})

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
  (let [formatter (NumberFormat/getCurrencyInstance locale)]
    (doto formatter
      (.setRoundingMode rounding-mode)
      (.setMinimumFractionDigits decimal-places)
      (.setMaximumFractionDigits decimal-places))
    (when (iso-4217? currency)
      (.setCurrency formatter (Currency/getInstance (currency-code currency))))
    formatter))

(defn format
  [money & {:keys [locale rounding-mode decimal-places] :as _options}]
  (let [amount (get-amount money)
        currency (get-currency money)
        locale (or locale (Locale/getDefault))
        rounding-mode (or rounding-mode RoundingMode/HALF_EVEN)
        decimal-places (or decimal-places (minor-unit currency))
        formatter (make-formatter currency locale rounding-mode decimal-places)
        formatted-money (.format formatter amount)]
    (if (iso-4217? currency)
      formatted-money
      (let [locale-currency (Currency/getInstance locale)
            locale-symbol (.getSymbol locale-currency locale)
            currency-symbol (currency-symbol currency)]
        (str/replace formatted-money locale-symbol currency-symbol)))))

(comment
  (format (money-of 1 :eur))
  (format (money-of 1 :eur) {:locale Locale/UK})
  (format (money-of 1 :gbp))
  (format (money-of 1 :gbp) {:locale Locale/UK})
  (format (money-of 1 :btc))
  (format (money-of 1 :btc) {:locale Locale/UK})
  )
