(ns dinero.math
  (:require [dinero.core :as core]
            [dinero.utils :as utils]
            [clojure.math :as math])
  (:import [dinero.core Money FastMoney RoundedMoney]
           [java.math RoundingMode]))

(set! *warn-on-reflection* true)

;;; Equality and comparison

(defn- same-currency?
  "Returns true if all the given monetary amounts have the same currency."
  [& moneis]
  (apply = (map core/get-currency moneis)))

(defn- assert-same-currency
  "Asserts that all the given monetary amounts have the same currency."
  [& moneis]
  (when-not (apply same-currency? moneis)
    (throw (ex-info "Currencies do not match" {:currencies (map core/get-currency moneis)}))))

(defn- same-scale?
  "Returns true if all the given monetary amounts have the same scale."
  [& moneis]
  (when (some nil? moneis)
    (throw (ex-info "Scale must be non-nil" {})))
  (apply = (map core/get-scale moneis)))

(defn- assert-same-scale
  "Asserts that all the given monetary amounts have the same scale."
  [& moneis]
  (when-not (apply same-scale? moneis)
    (throw (ex-info "Scales do not match" {:scales (map core/get-scale moneis)}))))

(defn- same-rounding-mode?
  "Returns true if all the given rounded monetary amounts have the same rounding mode."
  [& rounded-moneis]
  (apply = (map core/get-rounding-mode rounded-moneis)))

(defn- assert-same-rounding-mode
  "Asserts that all the given rounded monetary amounts have the same rounding mode."
  [& rounded-moneis]
  (when-not (apply same-rounding-mode? rounded-moneis)
    (throw (ex-info "Rounding modes do not match" {:rounding-modes (map core/get-rounding-mode rounded-moneis)}))))

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
  (< (core/get-amount money-1) (core/get-amount money-2)))

(defn money<=
  "Returns true if the first monetary amount is less than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (<= (core/get-amount money-1) (core/get-amount money-2)))

(defn money>
  "Returns true if the first monetary amount is greater than the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (> (core/get-amount money-1) (core/get-amount money-2)))

(defn money>=
  "Returns true if the first monetary amount is greater than or equal to the second monetary amount."
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (>= (core/get-amount money-1) (core/get-amount money-2)))

(defn money-zero?
  "Returns true if the given monetary amount is zero."
  [money]
  (zero? (core/get-amount money)))

(defn money-pos?
  "Returns true if the given monetary amount is positive."
  [money]
  (pos? (core/get-amount money)))

(defn money-neg?
  "Returns true if the given monetary amount is negative."
  [money]
  (neg? (core/get-amount money)))

;;; Arithmetic operations

(defmulti add
  "Adds the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      core/money? Money
      core/fast-money? FastMoney
      RoundedMoney)))

(defmulti subtract
  "Subtracts the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      core/money? Money
      core/fast-money? FastMoney
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
      core/money? Money
      core/fast-money? FastMoney
      RoundedMoney)))

(defmulti money-min
  "Returns the minimum of the given monetary amounts."
  {:arglists '([& moneis])}
  (fn [& moneis]
    (condp some moneis
      core/money? Money
      core/fast-money? FastMoney
      RoundedMoney)))

(defmethod add Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [sum (reduce + (map core/get-amount moneis))
        currency (core/get-currency (first moneis))]
    (core/money-of sum currency)))

(defmethod add RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [sum (reduce + (map core/get-amount moneis))
        currency (core/get-currency (first moneis))
        scale (core/get-scale (first moneis))
        rounding-mode (core/get-rounding-mode (first moneis))]
    (core/rounded-money-of sum currency scale rounding-mode)))

(defmethod add FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [sum (try (reduce math/add-exact (map :amount moneis))
                 (catch ArithmeticException e
                   (throw (ex-info "`FastMoney` addition failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                   {:moneis moneis
                                    :error {:type (type e)
                                            :message (ex-message e)}}))))
        currency (core/get-currency (first moneis))]
    ;; use `FastMoney` constructor because we are working with the internal representation (`long` amounts) directly
    (FastMoney. sum currency core/fast-money-max-scale)))

(defmethod subtract Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [difference (reduce - (map core/get-amount moneis))
        currency (core/get-currency (first moneis))]
    (core/money-of difference currency)))

(defmethod subtract RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [difference (reduce - (map core/get-amount moneis))
        currency (core/get-currency (first moneis))
        scale (core/get-scale (first moneis))
        rounding-mode (core/get-rounding-mode (first moneis))]
    (core/rounded-money-of difference currency scale rounding-mode)))

(defmethod subtract FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [difference (try (reduce math/subtract-exact (map :amount moneis))
                        (catch ArithmeticException e
                          (throw (ex-info "`FastMoney` subtraction failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                          {:moneis moneis
                                           :error {:type (type e)
                                                   :message (ex-message e)}}))))
        currency (core/get-currency (first moneis))]
    ;; use `FastMoney` constructor because we are working with the internal representation (`long` amounts) directly
    (FastMoney. difference currency core/fast-money-max-scale)))

(defmethod multiply Money
  [money factor]
  (let [amount (core/get-amount money)
        product (* amount (bigdec factor))
        currency (core/get-currency money)]
    (core/money-of product currency)))

(defmethod multiply RoundedMoney
  [money factor]
  (let [amount (core/get-amount money)
        product (* amount (bigdec factor))
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of product currency scale rounding-mode)))

(defmethod multiply FastMoney
  [money factor]
  (let [amount-as-long (:amount money)
        product-as-long (try (* amount-as-long factor)
                             (catch ArithmeticException e
                               (throw (ex-info "`FastMoney` multiplication failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                               {:money money
                                                :factor factor
                                                :error {:type (type e)
                                                        :message (ex-message e)}}))))
        currency (core/get-currency money)]
    ;; use `FastMoney` constructor because we are working with the internal representation (`long` amounts) directly
    (FastMoney. product-as-long currency core/fast-money-max-scale)))

(defmethod divide Money
  [money divisor]
  (let [amount (core/get-amount money)
        scale 256 ; max precision
        rounding-mode (utils/keyword->rounding-mode (or core/*default-rounding-mode* :half-even))
        quotient (BigDecimal/.divide ^BigDecimal amount (bigdec divisor) scale ^RoundingMode rounding-mode)
        currency (core/get-currency money)]
    (core/money-of (BigDecimal/.stripTrailingZeros quotient) currency)))

(defmethod divide RoundedMoney
  [money divisor]
  (let [amount (core/get-amount money)
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)
        rounding-mode-object (utils/keyword->rounding-mode rounding-mode)
        quotient (BigDecimal/.divide ^BigDecimal amount (bigdec divisor) ^int scale ^RoundingMode rounding-mode-object)]
    (core/rounded-money-of (BigDecimal/.stripTrailingZeros quotient) currency scale rounding-mode)))

(defmethod divide FastMoney
  [money divisor]
  (let [amount-as-long (:amount money)
        quotient (try (math/round (/ amount-as-long divisor))
                      (catch ArithmeticException e
                        (throw (ex-info "`FastMoney` division failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                        {:money money
                                         :divisor divisor
                                         :error {:type (type e)
                                                 :message (ex-message e)}}))))
        currency (core/get-currency money)]
    ;; use `FastMoney` constructor because we are working with the internal representation (`long` amounts) directly
    (FastMoney. quotient currency core/fast-money-max-scale)))

(defmethod negate Money
  [money]
  (let [amount (core/get-amount money)
        negated (BigDecimal/.negate amount)
        currency (core/get-currency money)]
    (core/money-of negated currency)))

(defmethod negate RoundedMoney
  [money]
  (let [amount (core/get-amount money)
        negated (BigDecimal/.negate amount)
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of negated currency scale rounding-mode)))

(defmethod negate FastMoney
  [money]
  (let [amount (:amount money)
        negated (try (- amount)
                     (catch ArithmeticException e
                       (throw (ex-info "`FastMoney` negation failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                                       {:money money
                                        :error {:type (type e)
                                                :message (ex-message e)}}))))
        currency (core/get-currency money)]
    (FastMoney. negated currency core/fast-money-max-scale)))

(defmethod money-abs Money
  [money]
  (let [amount (core/get-amount money)
        absolute (abs amount)
        currency (core/get-currency money)]
    (core/money-of absolute currency)))

(defmethod money-abs RoundedMoney
  [money]
  (let [amount (core/get-amount money)
        absolute (abs amount)
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of absolute currency scale rounding-mode)))

(defn- safe-long-abs
  [value]
  (if (= value Long/MIN_VALUE)
    (throw (ex-info "`FastMoney` absolute value failed: amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                    {:value value}))
    (abs value)))

(defmethod money-abs FastMoney
  [money]
  (let [amount (:amount money)
        absolute (safe-long-abs amount)
        currency (core/get-currency money)]
    (FastMoney. absolute currency core/fast-money-max-scale)))

(defmethod money-max Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map core/get-amount moneis)
        max-amount (apply max amounts)
        currency (core/get-currency (first moneis))]
    (core/money-of max-amount currency)))

(defmethod money-max RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map core/get-amount moneis)
        max-amount (apply max amounts)
        currency (core/get-currency (first moneis))
        scale (core/get-scale (first moneis))
        rounding-mode (core/get-rounding-mode (first moneis))]
    (core/rounded-money-of max-amount currency scale rounding-mode)))

(defmethod money-max FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map :amount moneis)
        max-amount (apply max amounts)
        currency (core/get-currency (first moneis))]
    (FastMoney. max-amount currency core/fast-money-max-scale)))

(defmethod money-min Money
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map core/get-amount moneis)
        min-amount (apply min amounts)
        currency (core/get-currency (first moneis))]
    (core/money-of min-amount currency)))

(defmethod money-min RoundedMoney
  [& moneis]
  (apply assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map core/get-amount moneis)
        min-amount (apply min amounts)
        currency (core/get-currency (first moneis))
        scale (core/get-scale (first moneis))
        rounding-mode (core/get-rounding-mode (first moneis))]
    (core/rounded-money-of min-amount currency scale rounding-mode)))

(defmethod money-min FastMoney
  [& moneis]
  (apply assert-same-currency moneis)
  (let [amounts (map :amount moneis)
        min-amount (apply min amounts)
        currency (core/get-currency (first moneis))]
    (FastMoney. min-amount currency core/fast-money-max-scale)))
