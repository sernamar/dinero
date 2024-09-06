(ns dinero.conversion
  (:require [dinero.core :as core])
  (:import [dinero.core Money RoundedMoney]))

(defmulti convert-with-exchange-rate
  "Converts the given monetary amount to the term currency using the given exchange rate."
  {:arglists '([money term-currency exchange-rate])}
  (fn [money _term-currency _exchange-rate]
    (class money)))

(defmethod convert-with-exchange-rate Money
  [money term-currency exchange-rate]
  (let [amount (core/get-amount money)
        term-amount (* amount exchange-rate)]
    (core/money-of term-amount term-currency)))

(defmethod convert-with-exchange-rate RoundedMoney
  [money term-currency exchange-rate]
  (let [amount (core/get-amount money)
        term-amount (* amount exchange-rate)
        decimal-places (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of term-amount term-currency decimal-places rounding-mode)))
