(ns dinero.conversion.core
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

(defn convert
  "Converts the given monetary amount to the term currency using the given exchange rate provider function."
  ([money term-currency rate-provider-fn]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [exchange-rate (rate-provider-fn base-currency term-currency)]
         (convert-with-exchange-rate money term-currency exchange-rate)))))
  ([money term-currency date rate-provider-fn]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [exchange-rate (rate-provider-fn base-currency term-currency date)]
         (convert-with-exchange-rate money term-currency exchange-rate))))))
