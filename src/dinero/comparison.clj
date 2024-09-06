(ns dinero.comparison
  (:require [dinero.core :as core]))

;;; Equality and comparison

(defn- same-currency?
  "Returns true if all the given monetary amounts have the same currency."
  [& moneis]
  (apply = (map core/get-currency moneis)))

(defn assert-same-currency
  "Asserts that all the given monetary amounts have the same currency."
  [& moneis]
  (when-not (apply same-currency? moneis)
    (throw (ex-info "Currencies do not match" {:currencies (map core/get-currency moneis)}))))

(defn- same-scale?
  "Returns true if all the given rounded monetary amounts have the same scale."
  [& rounded-moneis]
  (when (some nil? rounded-moneis)
    (throw (ex-info "Scale must be non-nil" {})))
  (apply = (map core/get-scale rounded-moneis)))

(defn- assert-same-scale
  "Asserts that all the given rounded monetary amounts have the same scale."
  [& rounded-moneis]
  (when-not (apply same-scale? rounded-moneis)
    (throw (ex-info "Scales do not match" {:scales (map core/get-scale rounded-moneis)}))))

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

(defn assert-same-currency-scale-and-rounding-mode
  "Asserts that all the given monetary amounts have the same currency, scale, and rounding mode."
  [& moneis]
  (or (apply assert-same-currency moneis)
      (apply assert-same-scale-and-rounding-mode moneis)))

(defmulti money<
  "Returns true if the first monetary amount is less than the second monetary amount."
  (fn [money-1 money-2]
    (if (and (core/money? money-1) (core/money? money-2))
      :money
      :rounded-money)))

(defmulti money<=
  "Returns true if the first monetary amount is less than or equal to the second monetary amount."
  (fn [money-1 money-2]
    (if (and (core/money? money-1) (core/money? money-2))
      :money
      :rounded-money)))

(defmulti money>
  "Returns true if the first monetary amount is greater than the second monetary amount."
  (fn [money-1 money-2]
    (if (and (core/money? money-1) (core/money? money-2))
      :money
      :rounded-money)))

(defmulti money>=
  "Returns true if the first monetary amount is greater than or equal to the second monetary amount."
  (fn [money-1 money-2]
    (if (and (core/money? money-1) (core/money? money-2))
      :money
      :rounded-money)))

(defmethod money< :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (< (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money< :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (< (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money<= :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (<= (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money<= :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (<= (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money> :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (> (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money> :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
  (> (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money>= :money
  [money-1 money-2]
  (assert-same-currency money-1 money-2)
  (>= (core/get-amount money-1) (core/get-amount money-2)))

(defmethod money>= :rounded-money
  [money-1 money-2]
  (assert-same-currency-scale-and-rounding-mode money-1 money-2)
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
