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

;;; Monetary amounts

(defrecord Money [amount currency]
  Object
  (toString [_this]
    (str "Money{amount=" amount ", currency=" currency )))

(defrecord RoundedMoney [amount currency scale rounding-mode]
  Object
  (toString [_this]
    (str "RoundedMoney{amount=" amount ", currency=" currency ", scale=" scale ", rounding-mode=" rounding-mode)))

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  ([]
   (money-of 0 *default-currency*))
  ([amount]
   (money-of amount *default-currency*))
  ([amount currency]
   (assert-currency currency)
   (Money. (bigdec amount) currency)))

(defn rounded-money-of
  "Creates a rounded monetary amount with the given amount, currency, scale, and rounding mode."
  ([amount]
   (rounded-money-of amount *default-currency*))
  ([amount currency]
   (rounded-money-of amount currency (get-minor-units currency) *default-rounding-mode*))
  ([amount currency scale]
   (rounded-money-of amount currency scale *default-rounding-mode*))
  ([amount currency scale rounding-mode]
   (assert-currency currency)
   (let [scale (or scale (get-minor-units currency))
         rounding-mode (or rounding-mode *default-rounding-mode* :half-even)
         rounding-mode-object (utils/keyword->rounding-mode rounding-mode)]
     (when (neg? scale)
       (throw (ex-info "Scale must be non-negative" {:scale scale})))
     (RoundedMoney. (.setScale ^BigDecimal (bigdec amount) ^int scale ^RoundingMode rounding-mode-object)
                    currency
                    scale
                    rounding-mode))))

(defn money?
  "Returns true if the given value is a monetary amount of type `Money`."
  [money]
  (instance? Money money))

(defn rounded-money?
  "Returns true if the given value is a monetary amount of type `RoundedMoney`."
  [money]
  (instance? RoundedMoney money))

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (:amount money))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))

(defn get-scale
  "Returns the scale of the given rounded monetary amount."
  [money]
  (:scale money))

(defn get-rounding-mode
  "Returns the rounding mode of the given rounded monetary amount."
  [money]
  (:rounding-mode money))

(defmacro with-currency
  "Evaluates the body with the given currency as the default currency."
  [currency & body]
  `(binding [*default-currency* ~currency]
     ~@body))

;;; Formatting

(defn- make-formatter
  "Creates a new formatter for the given currency, locale, rounding mode, and decimal places."
  [currency locale decimal-places rounding-mode]
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
  [money & {:keys [locale decimal-places rounding-mode symbol-style] :as _options}]
  (let [amount (get-amount money)
        currency (get-currency money)
        locale (or locale (Locale/getDefault))
        decimal-places (or decimal-places (get-minor-units currency))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode *default-rounding-mode* :half-even))
        symbol-style (or symbol-style :symbol)
        formatter (make-formatter currency locale decimal-places rounding-mode)]
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
  "Parses the given string containing an ISO 4217 currency symbol and returns a monetary amount of type `Money`."
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

(defn- assert-same-currency
  "Asserts that all the given monetary amounts have the same currency."
  [& moneis]
  (when-not (apply same-currency? moneis)
    (throw (ex-info "Currencies do not match" {:currencies (map get-currency moneis)}))))

(defn- same-scale?
  "Returns true if all the given rounded monetary amounts have the same scale."
  [& rounded-moneis]
  (when (some nil? rounded-moneis)
    (throw (ex-info "Scale must be non-nil" {})))
  (apply = (map get-scale rounded-moneis)))

(defn- assert-same-scale
  "Asserts that all the given rounded monetary amounts have the same scale."
  [& rounded-moneis]
  (when-not (apply same-scale? rounded-moneis)
    (throw (ex-info "Scales do not match" {:scales (map get-scale rounded-moneis)}))))

(defn- same-rounding-mode?
  "Returns true if all the given rounded monetary amounts have the same rounding mode."
  [& rounded-moneis]
  (apply = (map get-rounding-mode rounded-moneis)))

(defn- assert-same-rounding-mode
  "Asserts that all the given rounded monetary amounts have the same rounding mode."
  [& rounded-moneis]
  (when-not (apply same-rounding-mode? rounded-moneis)
    (throw (ex-info "Rounding modes do not match" {:rounding-modes (map get-rounding-mode rounded-moneis)}))))

(defn- assert-same-scale-and-rounding-mode
  "Asserts that all the given rounded monetary amounts have the same scale and rounding mode."
  [& rounded-moneis]
  (or (apply assert-same-scale rounded-moneis)
      (apply assert-same-rounding-mode rounded-moneis)))

(defn- assert-same-currency-scale-and-rounding-mode
  "Asserts that all the given monetary amounts have the same currency, scale, and rounding mode."
  [& moneis]
  (or (apply assert-same-currency moneis)
      (apply assert-same-scale-and-rounding-mode moneis)))

(defmulti money<
  "Returns true if the first monetary amount is less than the second monetary amount."
  (fn [money-1 money-2]
    (if (and (money? money-1) (money? money-2))
      :money
      :rounded-money)))

(defmulti money<=
  "Returns true if the first monetary amount is less than or equal to the second monetary amount."
  (fn [money-1 money-2]
    (if (and (money? money-1) (money? money-2))
      :money
      :rounded-money)))

(defmulti money>
  "Returns true if the first monetary amount is greater than the second monetary amount."
  (fn [money-1 money-2]
    (if (and (money? money-1) (money? money-2))
      :money
      :rounded-money)))

(defmulti money>=
  "Returns true if the first monetary amount is greater than or equal to the second monetary amount."
  (fn [money-1 money-2]
    (if (and (money? money-1) (money? money-2))
      :money
      :rounded-money)))

(defmethod money< :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (< (get-amount money-1) (get-amount money-2)))

(defmethod money< :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (< (get-amount money-1) (get-amount money-2)))

(defmethod money<= :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (<= (get-amount money-1) (get-amount money-2)))

(defmethod money<= :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (<= (get-amount money-1) (get-amount money-2)))

(defmethod money> :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (> (get-amount money-1) (get-amount money-2)))

(defmethod money> :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (> (get-amount money-1) (get-amount money-2)))

(defmethod money>= :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (>= (get-amount money-1) (get-amount money-2)))

(defmethod money>= :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (>= (get-amount money-1) (get-amount money-2)))

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

(defmulti add
  "Adds the given monetary amounts."
  (fn [& moneis]
    (if (some money? moneis)
      :money
      :rounded-money)))

(defmulti subtract
  "Subtracts the given monetary amounts."
  (fn [& moneis]
    (if (some money? moneis)
      :money
      :rounded-money)))

(defmulti multiply
  "Multiplies the given monetary amount by the given factor."
  (fn [money factor]
    (if (money? money)
      :money
      :rounded-money)))

(defmulti divide
  "Divides the given monetary amount by the given divisor."
  (fn [money divisor]
    (if (money? money)
      :money
      :rounded-money)))

(defmulti negate
  "Negates the given monetary amount."
  (fn [money]
    (if (money? money)
      :money
      :rounded-money)))

(defmulti money-abs
  "Returns the absolute value of the given monetary amount."
  (fn [money]
    (if (money? money)
      :money
      :rounded-money)))

(defmulti money-max
  "Returns the maximum of the given monetary amounts."
  (fn [& moneis]
    (if (some money? moneis)
      :money
      :rounded-money)))

(defmulti money-min
  "Returns the minimum of the given monetary amounts."
  (fn [& moneis]
    (if (some money? moneis)
      :money
      :rounded-money)))

(defmethod add :money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [sum (reduce #(.add ^BigDecimal %1 %2) (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of sum currency)))

(defmethod add :rounded-money
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (when-not (apply assert-same-scale-and-rounding-mode moneis)
    (let [sum (reduce #(.add ^BigDecimal %1 %2) (map get-amount moneis))
          currency (get-currency (first moneis))
          scale (get-scale (first moneis))
          rounding-mode (get-rounding-mode (first moneis))]
      (rounded-money-of sum currency scale rounding-mode))))

(defmethod subtract :money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [difference (reduce #(.subtract ^BigDecimal %1 %2) (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of difference currency)))

(defmethod subtract :rounded-money
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (when-not (apply assert-same-scale-and-rounding-mode moneis)
    (let [difference (reduce #(.subtract ^BigDecimal %1 %2) (map get-amount moneis))
          currency (get-currency (first moneis))
          scale (get-scale (first moneis))
          rounding-mode (get-rounding-mode (first moneis))]
      (rounded-money-of difference currency scale rounding-mode))))

(defmethod multiply :money
  [money factor]
  (let [amount (get-amount money)
        product (.multiply ^BigDecimal amount (bigdec factor))
        currency (get-currency money)]
    (money-of product currency)))

(defmethod multiply :rounded-money
  [money factor]
  (let [amount (get-amount money)
        product (.multiply ^BigDecimal amount (bigdec factor))
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of product currency scale rounding-mode)))

(defmethod divide :money
  [money divisor]
  (let [amount (get-amount money)
        quotient (.divide ^BigDecimal amount (bigdec divisor))
        currency (get-currency money)]
    (money-of quotient currency)))

(defmethod divide :rounded-money
  [money divisor]
  (let [amount (get-amount money)
        quotient (.divide ^BigDecimal amount (bigdec divisor))
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of quotient currency scale rounding-mode)))

(defmethod negate :money
  [money]
  (let [amount (get-amount money)
        negated (.negate ^BigDecimal amount)
        currency (get-currency money)]
    (money-of negated currency)))

(defmethod negate :rounded-money
  [money]
  (let [amount (get-amount money)
        negated (.negate ^BigDecimal amount)
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of negated currency scale rounding-mode)))

(defmethod money-abs :money
  [money]
  (let [amount (get-amount money)
        absolute (.abs ^BigDecimal amount)
        currency (get-currency money)]
    (money-of absolute currency)))

(defmethod money-abs :rounded-money
  [money]
  (let [amount (get-amount money)
        absolute (.abs ^BigDecimal amount)
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of absolute currency scale rounding-mode)))

(defmethod money-max :money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map get-amount moneis)
        max-amount (apply max amounts)
        currency (get-currency (first moneis))]
    (money-of max-amount currency)))

(defmethod money-max :rounded-money
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map get-amount moneis)
          max-amount (apply max amounts)
          currency (get-currency (first moneis))
          scale (get-scale (first moneis))
          rounding-mode (get-rounding-mode (first moneis))]
      (rounded-money-of max-amount currency scale rounding-mode)))

(defmethod money-min :money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map get-amount moneis)
        min-amount (apply min amounts)
        currency (get-currency (first moneis))]
    (money-of min-amount currency)))

(defmethod money-min :rounded-money
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map get-amount moneis)
        min-amount (apply min amounts)
        currency (get-currency (first moneis))
        scale (get-scale (first moneis))
        rounding-mode (get-rounding-mode (first moneis))]
    (rounded-money-of min-amount currency scale rounding-mode)))

;;; Rounding

(defn round
  "Rounds the given monetary amount"
  ([money]
   (round money (get-minor-units (get-currency money)) (or *default-rounding-mode* :half-even)))
  ([money rounding-fn]
   (rounding-fn money))
  ([money decimal-places rounding-mode]
   (let [amount (get-amount money)
         currency (get-currency money)
         rounding-mode (utils/keyword->rounding-mode rounding-mode)
         rounded (.setScale ^BigDecimal amount ^int decimal-places ^RoundingMode rounding-mode)]
     (money-of rounded currency))))

(def chf-rounding-fn
  "Creates a rounding function for Swiss Francs."
  (fn [money]
    (let [amount (get-amount money)
          currency (get-currency money)
          scale 0
          rounded (-> (.divide ^BigDecimal amount (bigdec 0.05) scale RoundingMode/HALF_UP)
                      (.multiply (bigdec 0.05)))]
      (money-of rounded currency))))
