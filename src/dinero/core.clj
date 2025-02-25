(ns dinero.core
  (:require [dinero.currency :as currency]
            [dinero.utils :as utils]
            [clojure.pprint :as pp])
  (:import [java.math RoundingMode]))

(set! *warn-on-reflection* true)

;;; Configuration

(defonce config (utils/read-config "config.edn"))

(def ^:dynamic *default-currency* (:default-currency config))
(def ^:dynamic *default-rounding-mode* (:default-rounding-mode config))

(def ^:private fast-money-max-scale 5)

;;; Monetary amounts

(defrecord Money [amount currency])

(defrecord RoundedMoney [amount currency scale rounding-mode])

(defrecord FastMoney [amount currency scale])

(defn- to-fast-money-long
  "Converts the given amount to the `long`-based internal representation of `FastMoney`."
  [amount]
  (let [amount (bigdec amount)
        scale (BigDecimal/.scale amount)]
    (when (> scale fast-money-max-scale)
      (throw (ex-info "Scale exceeds the maximum allowed value" {:scale scale})))
    (try
      (-> amount
          (BigDecimal/.movePointRight fast-money-max-scale)
          (BigDecimal/.longValueExact))
      (catch ArithmeticException e
        (throw (ex-info "Amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                        {:amount amount
                         :error (ex-message e)}))))))

(defn- from-fast-money
  "Converts the `long`-based internal representation of `FastMoney` to a `double`."
  [amount scale]
  (/ amount (Math/pow 10 scale)))

(defmethod pp/simple-dispatch FastMoney [money]
  (let [{:keys [amount currency scale]} money
        amount (from-fast-money amount scale)]
    (print {:amount amount :currency currency})))

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  ([]
   (money-of 0 *default-currency*))
  ([amount]
   (money-of amount *default-currency*))
  ([amount currency]
   (currency/assert-currency currency)
   (Money. (bigdec amount) currency)))

(defn rounded-money-of
  "Creates a rounded monetary amount with the given amount, currency, scale, and rounding mode."
  ([]
   (rounded-money-of 0 *default-currency*))
  ([amount]
   (rounded-money-of amount *default-currency*))
  ([amount currency]
   (let [minor-units (currency/get-minor-units currency)
         scale (or minor-units (BigDecimal/.scale (bigdec amount)))]
     (rounded-money-of amount currency scale *default-rounding-mode*)))
  ([amount currency scale]
   (rounded-money-of amount currency scale *default-rounding-mode*))
  ([amount currency scale rounding-mode]
   (currency/assert-currency currency)
   (let [scale (or scale (currency/get-minor-units currency))
         rounding-mode (or rounding-mode *default-rounding-mode* :half-even)
         rounding-mode-object (utils/keyword->rounding-mode rounding-mode)]
     (when (neg? scale)
       (throw (ex-info "Scale must be non-negative" {:scale scale})))
     (RoundedMoney. (BigDecimal/.stripTrailingZeros (BigDecimal/.setScale ^BigDecimal (bigdec amount) ^int scale ^RoundingMode rounding-mode-object))
                    currency
                    scale
                    rounding-mode))))

(defn fast-money-of
  "Creates a fast monetary amount with the given amount and currency."
  ([]
   (fast-money-of 0 *default-currency*))
  ([amount]
   (fast-money-of amount *default-currency*))
  ([amount currency]
   (let [internal-amount (to-fast-money-long amount)]
     (FastMoney. internal-amount currency fast-money-max-scale))))

(defn money?
  "Returns true if the given value is a monetary amount of type `Money`."
  [money]
  (instance? Money money))

(defn rounded-money?
  "Returns true if the given value is a monetary amount of type `RoundedMoney`."
  [money]
  (instance? RoundedMoney money))

(defn fast-money?
  "Returns true if the given value is a monetary amount of type `FastMoney`."
  [money]
  (instance? FastMoney money))

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (if (fast-money? money)
    (let [{:keys [amount scale]} money]
      (from-fast-money amount scale))
    (:amount money)))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))

(defn get-scale
  "Returns the scale of the given monetary amount."
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
     (let [rest# (do ~@body)]
       rest#)))

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
  "Returns true if all the given monetary amounts have the same scale."
  [& moneis]
  (when (some nil? moneis)
    (throw (ex-info "Scale must be non-nil" {})))
  (apply = (map get-scale moneis)))

(defn- assert-same-scale
  "Asserts that all the given monetary amounts have the same scale."
  [& moneis]
  (when-not (apply same-scale? moneis)
    (throw (ex-info "Scales do not match" {:scales (map get-scale moneis)}))))

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

(defn money<
  "Returns true if the first monetary amount is less than the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (< (get-amount money-1) (get-amount money-2)))

(defn money<=
  "Returns true if the first monetary amount is less than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (<= (get-amount money-1) (get-amount money-2)))

(defn money>
  "Returns true if the first monetary amount is greater than the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (> (get-amount money-1) (get-amount money-2)))

(defn money>=
  "Returns true if the first monetary amount is greater than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
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
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      money? Money
      fast-money? FastMoney
      RoundedMoney)))

(defmulti subtract
  "Subtracts the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      money? Money
      fast-money? FastMoney
      RoundedMoney)))

(defmulti multiply
  "Multiplies the given monetary amount by the given factor."
  {:arglists '([money factor])}
  (fn [money factor]
    (class money)))

(defmulti divide
  "Divides the given monetary amount by the given divisor."
  {:arglists '([money divisor])}
  (fn [money divisor]
    (class money)))

(defmulti negate
  "Negates the given monetary amount."
  {:arglists '([money])}
  class)

(defmulti money-abs
  "Returns the absolute value of the given monetary amount."
  {:arglists '([money])}
  class)

(defmulti money-max
  "Returns the maximum of the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      money? Money
      fast-money? FastMoney
      RoundedMoney)))

(defmulti money-min
  "Returns the minimum of the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      money? Money
      fast-money? FastMoney
      RoundedMoney)))

(defmethod add Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [sum (reduce + (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of sum currency)))

(defmethod add RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [sum (reduce + (map get-amount moneis))
        currency (get-currency (first moneis))
        scale (get-scale (first moneis))
        rounding-mode (get-rounding-mode (first moneis))]
    (rounded-money-of sum currency scale rounding-mode)))

(defmethod add FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [sum (try (reduce + (map :amount moneis))
                 (catch ArithmeticException e
                   (throw (ex-info "`FastMoney` addition failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                   {:moneis moneis
                                    :error (ex-message e)}))))
        currency (get-currency (first moneis))]
    ;; use `FastMoney` constructor directly instead of `fast-money-of` to improve performance
    (FastMoney. sum currency fast-money-max-scale)))

(defmethod subtract Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [difference (reduce - (map get-amount moneis))
        currency (get-currency (first moneis))]
    (money-of difference currency)))

(defmethod subtract RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [difference (reduce - (map get-amount moneis))
        currency (get-currency (first moneis))
        scale (get-scale (first moneis))
        rounding-mode (get-rounding-mode (first moneis))]
    (rounded-money-of difference currency scale rounding-mode)))

(defmethod subtract FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [difference (try (reduce - (map :amount moneis))
                        (catch ArithmeticException e
                          (throw (ex-info "`FastMoney` subtraction failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                          {:moneis moneis
                                           :error (ex-message e)}))))
        currency (get-currency (first moneis))]
    ;; use `FastMoney` constructor directly instead of `fast-money-of` to improve performance
    (FastMoney. difference currency fast-money-max-scale)))

(defmethod multiply Money
  [money factor]
  (let [amount (get-amount money)
        product (* amount (bigdec factor))
        currency (get-currency money)]
    (money-of product currency)))

(defmethod multiply RoundedMoney
  [money factor]
  (let [amount (get-amount money)
        product (* amount (bigdec factor))
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of product currency scale rounding-mode)))

(defmethod multiply FastMoney
  [money factor]
  (let [amount-as-long (:amount money)
        factor-scaled (to-fast-money-long factor)
        product-scaled (try (* amount-as-long factor-scaled)
                            (catch ArithmeticException e
                              (throw (ex-info "`FastMoney` multiplication failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                              {:money money
                                               :factor factor
                                               :error (ex-message e)}))))
        product (long (from-fast-money product-scaled fast-money-max-scale))
        currency (get-currency money)]
    ;; use `FastMoney` constructor directly instead of `fast-money-of` to improve performance
    (FastMoney. product currency fast-money-max-scale)))

(defmethod divide Money
  [money divisor]
  (let [amount (get-amount money)
        scale 256 ; max precision
        rounding-mode (utils/keyword->rounding-mode (or *default-rounding-mode* :half-even))
        quotient (BigDecimal/.divide ^BigDecimal amount (bigdec divisor) scale ^RoundingMode rounding-mode)
        currency (get-currency money)]
    (money-of quotient currency)))

(defmethod divide RoundedMoney
  [money divisor]
  (let [amount (get-amount money)
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)
        rounding-mode-object (utils/keyword->rounding-mode rounding-mode)
        quotient (BigDecimal/.divide ^BigDecimal amount (bigdec divisor) ^int scale ^RoundingMode rounding-mode-object)]
    (rounded-money-of quotient currency scale rounding-mode)))

(defn- round-double
  "Rounds the given double value to the given precision."
  [value precission]
  (let [factor (Math/pow 10 precission)]
    (/ (Math/round (* value factor)) factor)))

(defmethod divide FastMoney
  [money divisor]
  (let [amount (get-amount money)
        quotient (/ amount divisor)
        quotient-rounded (round-double quotient fast-money-max-scale)
        currency (get-currency money)]
    (fast-money-of quotient-rounded currency)))

(defmethod negate Money
  [money]
  (let [amount (get-amount money)
        negated (BigDecimal/.negate amount)
        currency (get-currency money)]
    (money-of negated currency)))

(defmethod negate RoundedMoney
  [money]
  (let [amount (get-amount money)
        negated (BigDecimal/.negate amount)
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of negated currency scale rounding-mode)))

;; TODO: Implement `negate` for `FastMoney`
(defmethod negate FastMoney
  [money]
  :not-implemented)

(defmethod money-abs Money
  [money]
  (let [amount (get-amount money)
        absolute (abs amount)
        currency (get-currency money)]
    (money-of absolute currency)))

(defmethod money-abs RoundedMoney
  [money]
  (let [amount (get-amount money)
        absolute (abs amount)
        currency (get-currency money)
        scale (get-scale money)
        rounding-mode (get-rounding-mode money)]
    (rounded-money-of absolute currency scale rounding-mode)))

;; TODO: Implement `money-abs` for `FastMoney`
(defmethod money-abs FastMoney
  [money]
  :not-implemented)

(defmethod money-max Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map get-amount moneis)
        max-amount (apply max amounts)
        currency (get-currency (first moneis))]
    (money-of max-amount currency)))

(defmethod money-max RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map get-amount moneis)
        max-amount (apply max amounts)
        currency (get-currency (first moneis))
        scale (get-scale (first moneis))
        rounding-mode (get-rounding-mode (first moneis))]
    (rounded-money-of max-amount currency scale rounding-mode)))

;; TODO: Implement `money-max` for `FastMoney`
(defmethod money-max FastMoney
  [& moneis]
  :not-implemented)

(defmethod money-min Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map get-amount moneis)
        min-amount (apply min amounts)
        currency (get-currency (first moneis))]
    (money-of min-amount currency)))

(defmethod money-min RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map get-amount moneis)
        min-amount (apply min amounts)
        currency (get-currency (first moneis))
        scale (get-scale (first moneis))
        rounding-mode (get-rounding-mode (first moneis))]
    (rounded-money-of min-amount currency scale rounding-mode)))

;; TODO: Implement `money-min` for `FastMoney`
(defmethod money-min FastMoney
  [& moneis]
  :not-implemented)
