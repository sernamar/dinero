(ns dinero.core
  (:refer-clojure :exclude [format])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [dinero.utils :as utils])
  (:import [java.math RoundingMode]
           [java.text DecimalFormat DecimalFormatSymbols]
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

(defn- get-minor-units
  "Returns the minor units of the given currency."
  [currency]
  (get-in currencies [currency :minor-units]))

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

(defmacro with-currency
  "Evaluates the body with the given currency as the default currency."
  [currency & body]
  `(binding [*default-currency* ~currency]
     ~@body))

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
        decimal-places (or decimal-places (get-minor-units currency))
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

(defn parse-containing-iso-4217-symbol
  "Parses the given string containing an ISO 4217 currency symbol and returns a monetary amount."
  [string locale]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)
        amount (.parse ^DecimalFormat formatter string)
        currency (-> (.getCurrency formatter) str/lower-case keyword)]
    (money-of amount currency)))

;;; Equality and comparison

(defn- same-currency?
  "Returns true if all the given monetary amounts have the same currency."
  [& moneis]
  (apply = (map get-currency moneis)))

(defn- same-amount?
  "Returns true if all the given monetary amounts have the same amount."
  [money-1 money-2]
  (= (get-amount money-1) (get-amount money-2)))

(defn- assert-same-currency
  "Asserts that all the given monetary amounts have the same currency."
  [& moneis]
  (when-not (apply same-currency? moneis)
    (throw (ex-info "Currencies do not match" {:currencies (map get-currency moneis)}))))

(defn money=
  "Returns true if all the given monetary amounts have the same amount and currency."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (same-amount? money-1 money-2))

(defn money-not=
  "Returns true if the given monetary amounts do not have the same amount and currency."
  [money-1 money-2]
  (not (money= money-1 money-2)))

(defn money<
  "Returns true if the first monetary amount is less than the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (= -1 (.compareTo ^BigDecimal (get-amount money-1) (get-amount money-2))))

(defn money<=
  "Returns true if the first monetary amount is less than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (<= (.compareTo ^BigDecimal (get-amount money-1) (get-amount money-2)) 0))

(defn money>
  "Returns true if the first monetary amount is greater than the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (= 1 (.compareTo ^BigDecimal (get-amount money-1) (get-amount money-2))))

(defn money>=
  "Returns true if the first monetary amount is greater than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (>= (.compareTo ^BigDecimal (get-amount money-1) (get-amount money-2)) 0))

(defn money-zero?
  "Returns true if the given monetary amount is zero."
  [money]
  (zero? (get-amount money)))

(defn money-pos?
  "Returns true if the given monetary amount is positive."
  [money]
  (pos? (get-amount money)))

(defn money-neg?
  "Returns true if the given monetary amount is negative."
  [money]
  (neg? (get-amount money)))

;;; Arithmetic operations

(defn add
  "Adds the given monetary amounts."
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amount (reduce #(.add ^BigDecimal %1 %2) (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of amount currency)))

(defn subtract
  "Subtracts the given monetary amounts."
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amount (reduce #(.subtract ^BigDecimal %1 %2) (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of amount currency)))

(defn multiply
  "Multiplies the given monetary amount by the given factor."
  [money factor]
  (let [amount (get-amount money)
        product (.multiply ^BigDecimal amount (bigdec factor))
        currency (get-currency money)]
    (money-of product currency)))

(defn divide
  "Divides the given monetary amount by the given divisor."
  [money divisor]
  (let [amount (get-amount money)
        quotient (.divide ^BigDecimal amount (bigdec divisor))
        currency (get-currency money)]
    (money-of quotient currency)))

;;; Rounding

(defn create-rounding
  "Creates a rounding function with the given rounding mode and decimal places."
  ([]
   (create-rounding (or *default-rounding-mode* :half-even) (get-minor-units *default-currency*)))
  ([decimal-places]
   (create-rounding (or *default-rounding-mode* :half-even) decimal-places))
  ([rounding-mode decimal-places]
   (fn [money]
     (let [amount (get-amount money)
           currency (get-currency money)
           rounding-mode (utils/keyword->rounding-mode rounding-mode)
           rounded (.setScale ^BigDecimal amount ^int decimal-places ^RoundingMode rounding-mode)]
       (money-of rounded currency)))))

(defn chf-rounding
  "Creates a rounding function for Swiss Francs."
  []
  (fn [money]
    (let [amount (get-amount money)
          currency (get-currency money)
          scale 0
          rounded (-> (.divide ^BigDecimal amount (bigdec 0.05) scale RoundingMode/HALF_UP)
                      (.multiply (bigdec 0.05)))]
      (money-of rounded currency))))

(defn round
  "Rounds the given monetary amount using the given rounding function."
  ([money]
   (let [decimal-places (get-minor-units (get-currency money))
         rounding (create-rounding decimal-places)]
     (round money rounding)))
  ([money rounding]
   (rounding money)))
