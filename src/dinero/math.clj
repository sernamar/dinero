(ns dinero.math
  (:require [dinero.comparison :as comparison]
            [dinero.core :as core])
  (:import [java.math BigDecimal]))

(defmulti add
  "Adds the given monetary amounts."
  (fn [& moneis]
    (if (some core/money? moneis)
      :money
      :rounded-money)))

(defmulti subtract
  "Subtracts the given monetary amounts."
  (fn [& moneis]
    (if (some core/money? moneis)
      :money
      :rounded-money)))

(defmulti multiply
  "Multiplies the given monetary amount by the given factor."
  (fn [money factor]
    (if (core/money? money)
      :money
      :rounded-money)))

(defmulti divide
  "Divides the given monetary amount by the given divisor."
  (fn [money divisor]
    (if (core/money? money)
      :money
      :rounded-money)))

(defmulti negate
  "Negates the given monetary amount."
  (fn [money]
    (if (core/money? money)
      :money
      :rounded-money)))

(defmulti money-abs
  "Returns the absolute value of the given monetary amount."
  (fn [money]
    (if (core/money? money)
      :money
      :rounded-money)))

(defmulti money-max
  "Returns the maximum of the given monetary amounts."
  (fn [& moneis]
    (if (some core/money? moneis)
      :money
      :rounded-money)))

(defmulti money-min
  "Returns the minimum of the given monetary amounts."
  (fn [& moneis]
    (if (some core/money? moneis)
      :money
      :rounded-money)))

(defmethod add :money
  [& moneis]
  (apply comparison/assert-same-currency moneis)
  (let [sum (reduce + (map core/get-amount moneis))
        currency (core/get-currency (first moneis))]
    (core/money-of sum currency)))

(defmethod add :rounded-money
  [& moneis]
  (apply comparison/assert-same-currency-scale-and-rounding-mode moneis)
  (let [sum (reduce + (map core/get-amount moneis))
          currency (core/get-currency (first moneis))
          scale (core/get-scale (first moneis))
          rounding-mode (core/get-rounding-mode (first moneis))]
      (core/rounded-money-of sum currency scale rounding-mode)))

(defmethod subtract :money
  [& moneis]
  (apply comparison/assert-same-currency moneis)
  (let [difference (reduce - (map core/get-amount moneis))
        currency (core/get-currency (first moneis))]
    (core/money-of difference currency)))

(defmethod subtract :rounded-money
  [& moneis]
  (apply comparison/assert-same-currency-scale-and-rounding-mode moneis)
  (let [difference (reduce - (map core/get-amount moneis))
          currency (core/get-currency (first moneis))
          scale (core/get-scale (first moneis))
          rounding-mode (core/get-rounding-mode (first moneis))]
      (core/rounded-money-of difference currency scale rounding-mode)))

(defmethod multiply :money
  [money factor]
  (let [amount (core/get-amount money)
        product (* amount (bigdec factor))
        currency (core/get-currency money)]
    (core/money-of product currency)))

(defmethod multiply :rounded-money
  [money factor]
  (let [amount (core/get-amount money)
        product (* amount (bigdec factor))
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of product currency scale rounding-mode)))

(defmethod divide :money
  [money divisor]
  (let [amount (core/get-amount money)
        quotient (/ amount (bigdec divisor))
        currency (core/get-currency money)]
    (core/money-of quotient currency)))

(defmethod divide :rounded-money
  [money divisor]
  (let [amount (core/get-amount money)
        quotient (/ amount (bigdec divisor))
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of quotient currency scale rounding-mode)))

(defmethod negate :money
  [money]
  (let [amount (core/get-amount money)
        negated (BigDecimal/.negate amount)
        currency (core/get-currency money)]
    (core/money-of negated currency)))

(defmethod negate :rounded-money
  [money]
  (let [amount (core/get-amount money)
        negated (BigDecimal/.negate amount)
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of negated currency scale rounding-mode)))

(defmethod money-abs :money
  [money]
  (let [amount (core/get-amount money)
        absolute (abs amount)
        currency (core/get-currency money)]
    (core/money-of absolute currency)))

(defmethod money-abs :rounded-money
  [money]
  (let [amount (core/get-amount money)
        absolute (abs amount)
        currency (core/get-currency money)
        scale (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of absolute currency scale rounding-mode)))

(defmethod money-max :money
  [& moneis]
  (apply comparison/assert-same-currency moneis)
  (let [amounts (map core/get-amount moneis)
        max-amount (apply max amounts)
        currency (core/get-currency (first moneis))]
    (core/money-of max-amount currency)))

(defmethod money-max :rounded-money
  [& moneis]
  (apply comparison/assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map core/get-amount moneis)
          max-amount (apply max amounts)
          currency (core/get-currency (first moneis))
          scale (core/get-scale (first moneis))
          rounding-mode (core/get-rounding-mode (first moneis))]
      (core/rounded-money-of max-amount currency scale rounding-mode)))

(defmethod money-min :money
  [& moneis]
  (apply comparison/assert-same-currency moneis)
  (let [amounts (map core/get-amount moneis)
        min-amount (apply min amounts)
        currency (core/get-currency (first moneis))]
    (core/money-of min-amount currency)))

(defmethod money-min :rounded-money
  [& moneis]
  (apply comparison/assert-same-currency-scale-and-rounding-mode moneis)
  (let [amounts (map core/get-amount moneis)
        min-amount (apply min amounts)
        currency (core/get-currency (first moneis))
        scale (core/get-scale (first moneis))
        rounding-mode (core/get-rounding-mode (first moneis))]
    (core/rounded-money-of min-amount currency scale rounding-mode)))
