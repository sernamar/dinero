;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.math-test
  (:require [dinero.core :as core]
            [dinero.math :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))

;;; Equality and comparison

(t/deftest comparison
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of 1 :eur)
          m3 (core/money-of 2 :eur)
          m4 (core/money-of 1 :gbp)]
      (t/is (sut/money< m1 m3))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m3 m1))
      (t/is (sut/money>= m3 m2))
      (t/is (thrown? ExceptionInfo (sut/money< m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m4)))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1 :eur)
          m2 (core/rounded-money-of 1 :eur)
          m3 (core/rounded-money-of 2 :eur)
          m4 (core/rounded-money-of 1 :gbp)]
      (t/is (sut/money< m1 m3))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m3 m1))
      (t/is (sut/money>= m3 m2))
      (t/is (thrown? ExceptionInfo (sut/money< m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m4)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of 1 :eur)
          m3 (core/fast-money-of 2 :eur)
          m4 (core/fast-money-of 1 :gbp)]
      (t/is (sut/money< m1 m3))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m3 m1))
      (t/is (sut/money>= m3 m2))
      (t/is (thrown? ExceptionInfo (sut/money< m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m4)))))
  (t/testing "Mix money types"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/rounded-money-of 3 :eur)]
      ;; money and fast money
      (t/is (thrown? ExceptionInfo (sut/money< m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m2)))
      ;; money and rounded money
      (t/is (thrown? ExceptionInfo (sut/money< m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m3)))
      ;; fast money and rounded money
      (t/is (thrown? ExceptionInfo (sut/money< m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money<= m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money> m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money>= m2 m3))))))

(t/deftest sign-operations
  (t/testing "Money"
    (let [m1 (core/money-of 0 :eur)
          m2 (core/money-of 1 :eur)
          m3 (core/money-of -1 :eur)]
      (t/is (sut/money-zero? m1))
      (t/is (sut/money-pos? m2))
      (t/is (sut/money-neg? m3))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 0 :eur 2 :down)
          m2 (core/rounded-money-of 1 :eur 2 :down)
          m3 (core/rounded-money-of -1 :eur 2 :down)]
      (t/is (sut/money-zero? m1))
      (t/is (sut/money-pos? m2))
      (t/is (sut/money-neg? m3))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 0 :eur)
          m2 (core/fast-money-of 1 :eur)
          m3 (core/fast-money-of -1 :eur)]
      (t/is (sut/money-zero? m1))
      (t/is (sut/money-pos? m2))
      (t/is (sut/money-neg? m3)))))

;;; Arithmetic operations

(t/deftest add
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of 2 :eur)
          m3 (core/money-of 3 :eur)]
      (t/is (= (core/money-of 3 :eur) (sut/add m1 m2)))
      (t/is (= (core/money-of 6 :eur) (reduce sut/add [m1 m2 m3])))
      (t/is (thrown? ExceptionInfo (sut/add (core/money-of 1 :eur) (core/money-of 1 :gbp))))))
  (t/testing "Rounded Money")
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of 1.555 :eur 2 :down)
        m3 (core/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (core/rounded-money-of 3.1 :eur 2 :down) (sut/add m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/add m1 m3))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/fast-money-of 3 :eur)]
      (t/is (= (core/fast-money-of 3 :eur) (sut/add m1 m2)))
      (t/is (= (core/fast-money-of 6 :eur) (reduce sut/add [m1 m2 m3])))
      ;; different currencies
      (t/is (thrown? ExceptionInfo (sut/add (core/fast-money-of 1 :eur) (core/fast-money-of 1 :gbp)))))
    ;; result cannot be represented as a long
    (let [max-value (/ Long/MAX_VALUE 100000) ; 10000 = 10^fast-money-max-scale
          min-value (/ Long/MIN_VALUE 100000)] ; 10000 = 10^fast-money-max-scale
      (t/is (thrown? ExceptionInfo (sut/add (core/fast-money-of max-value :eur) (core/fast-money-of 1 :eur))))
      (t/is (thrown? ExceptionInfo (sut/add (core/fast-money-of min-value :eur) (core/fast-money-of -1 :eur))))))
  (t/testing "Mix money types"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/rounded-money-of 3 :eur)]
      (t/is (thrown? ExceptionInfo (sut/add m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/add m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/add m2 m3))))))

(t/deftest subtract
  (t/testing "Money"
    (let [m1 (core/money-of 3 :eur)
          m2 (core/money-of 2 :eur)
          m3 (core/money-of 1 :eur)]
      (t/is (= (core/money-of 1 :eur) (sut/subtract m1 m2)))
      (t/is (= (core/money-of 0 :eur) (reduce sut/subtract [m1 m2 m3])))
      (t/is (thrown? ExceptionInfo (sut/subtract (core/money-of 1 :eur) (core/money-of 1 :gbp))))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
          m2 (core/rounded-money-of 1.555 :eur 2 :down)
          m3 (core/rounded-money-of 1.555 :eur 2 :up)]
      (t/is (= (core/rounded-money-of 0 :eur 2 :down) (sut/subtract m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/subtract m1 m3)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 3 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/fast-money-of 1 :eur)]
      (t/is (= (core/fast-money-of 1 :eur) (sut/subtract m1 m2)))
      (t/is (= (core/fast-money-of 0 :eur) (reduce sut/subtract [m1 m2 m3])))
      ;; different currencies
      (t/is (thrown? ExceptionInfo (sut/subtract (core/fast-money-of 1 :eur) (core/fast-money-of 1 :gbp)))))
    ;; result cannot be represented as a long
    (let [max-value (/ Long/MAX_VALUE 100000) ; 10000 = 10^fast-money-max-scale
          min-value (/ Long/MIN_VALUE 100000)] ; 10000 = 10^fast-money-max-scale
      (t/is (thrown? ExceptionInfo (sut/subtract (core/fast-money-of min-value :eur) (core/fast-money-of 1 :eur))))
      (t/is (thrown? ExceptionInfo (sut/subtract (core/fast-money-of max-value :eur) (core/fast-money-of -1 :eur))))))
  (t/testing "Mix money types"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/rounded-money-of 3 :eur)]
      (t/is (thrown? ExceptionInfo (sut/subtract m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/subtract m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/subtract m2 m3))))))

(t/deftest multiply
  (t/testing "Money"
    (let [money (core/money-of 1 :eur)
          factor-long 2
          factor-double 2.5]
      (t/is (= (core/money-of 2 :eur) (sut/multiply money factor-long)))
      (t/is (= (core/money-of 2.5 :eur) (sut/multiply money factor-double)))))
  (t/testing "Rounded Money"
    (let [money (core/rounded-money-of 1.555 :eur 2 :down)
          factor-long 2
          factor-double 2.5]
      (t/is (= (core/rounded-money-of 3.1 :eur 2 :down) (sut/multiply money factor-long)))
      (t/is (= (core/rounded-money-of 3.87 :eur 2 :down) (sut/multiply money factor-double)))))
  (t/testing "Fast Money"
    (let [money (core/fast-money-of 1 :eur)
          factor-long 2
          factor-double 2.5]
      (t/is (= (core/fast-money-of 2 :eur) (sut/multiply money factor-long)))
      (t/is (= (core/fast-money-of 2.5 :eur) (sut/multiply money factor-double))))
    ;; result cannot be represented as a long
    (let [max-value (/ Long/MAX_VALUE 100000) ; 10000 = 10^fast-money-max-scale
          min-value (/ Long/MIN_VALUE 100000)] ; 10000 = 10^fast-money-max-scale
      ;; result cannot be represented as a long
      (t/is (thrown? ExceptionInfo (sut/multiply (core/fast-money-of max-value :eur) 2)))
      (t/is (thrown? ExceptionInfo (sut/multiply (core/fast-money-of min-value :eur) 2))))))

(t/deftest divide
  (t/testing "Money"
    (let [money (core/money-of 2 :eur)
          divisor 2]
      (t/is (= (core/money-of 1 :eur) (sut/divide money divisor)))))
  (t/testing "Non-terminating decimal expansion"
    ;; Dividing 1 by 3 cannot be exactly represented as a BigDecimal.
    ;; Without specifying a scale and rounding mode, attempting this division
    ;; will throw a java.lang.ArithmeticException: Non-terminating decimal expansion.
    (let [money (core/money-of 1 :eur)
          divisor 3]
      (t/is (= (core/money-of 0.3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333M :eur) (sut/divide money divisor)))))
  (t/testing "Rounded Money"
    (let [money (core/rounded-money-of 1.555 :eur 2 :down)
          divisor 2]
      (t/is (= (core/rounded-money-of 0.77 :eur 2 :down) (sut/divide money divisor)))))
  (t/testing "Fast Money"
    (let [money (core/fast-money-of 2 :eur)
          divisor 2]
      (t/is (= (core/fast-money-of 1 :eur) (sut/divide money divisor))))
    (let [money (core/fast-money-of 1 :eur)
          divisor 3]
      (t/is (= (core/fast-money-of 0.33333 :eur) (sut/divide money divisor))))))

(t/deftest negate
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of -1 :eur)]
      (t/is (= m2 (sut/negate m1)))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
          m2 (core/rounded-money-of -1.555 :eur 2 :down)]
      (t/is (= m2 (sut/negate m1)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of -1 :eur)
          max-value (/ Long/MAX_VALUE 100000) ; 10000 = 10^fast-money-max-scale
          min-value (/ Long/MIN_VALUE 100000) ;10000 = 10^fast-money-max-scale
          min-value-plus-one (/ (inc Long/MIN_VALUE) 100000)] ; 10000 = 10^fast-money-max-scale
      (t/is (= m2 (sut/negate m1)))
      (t/is (= (core/fast-money-of min-value-plus-one :eur) (sut/negate (core/fast-money-of max-value :eur))))
      (t/is (thrown? ExceptionInfo (sut/negate (core/fast-money-of min-value :eur)))))))

(t/deftest money-abs
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of -1 :eur)]
      (t/is (= m1 (sut/money-abs m1)))
      (t/is (= m1 (sut/money-abs m2)))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
          m2 (core/rounded-money-of -1.555 :eur 2 :down)]
      (t/is (= m1 (sut/money-abs m1)))
      (t/is (= m1 (sut/money-abs m2)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of -1 :eur)
          min-value (/ Long/MIN_VALUE 100000)] ; 10000 = 10^fast-money-max-scale
      (t/is (= m1 (sut/money-abs m1)))
      (t/is (= m1 (sut/money-abs m2)))
      (t/is (thrown? ExceptionInfo (sut/money-abs (core/fast-money-of min-value :eur)))))))

(t/deftest money-max
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of 2 :eur)
          m3 (core/money-of 3 :eur)]
      (t/is (= m2 (sut/money-max m1 m2)))
      (t/is (= m3 (sut/money-max m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 (core/money-of 1 :gbp))))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
          m2 (core/rounded-money-of 2.555 :eur 2 :down)
          m3 (core/rounded-money-of 3.555 :eur 2 :down)
          m4 (core/rounded-money-of 1.555 :eur 2 :up)
          m5 (core/rounded-money-of 1.555 :gbp 2 :down)]
      (t/is (= m2 (sut/money-max m1 m2)))
      (t/is (= m3 (sut/money-max m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m5)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/fast-money-of 3 :eur)]
      (t/is (= m2 (sut/money-max m1 m2)))
      (t/is (= m3 (sut/money-max m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 (core/fast-money-of 1 :gbp))))))
  (t/testing "Mix money types"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/rounded-money-of 3 :eur)]
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m2 m3))))))

(t/deftest money-min
  (t/testing "Money"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/money-of 2 :eur)
          m3 (core/money-of 3 :eur)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 (core/money-of 1 :gbp))))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
          m2 (core/rounded-money-of 2.555 :eur 2 :down)
          m3 (core/rounded-money-of 3.555 :eur 2 :down)
          m4 (core/rounded-money-of 1.555 :eur 2 :up)
          m5 (core/rounded-money-of 1.555 :gbp 2 :down)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 m5)))))
  (t/testing "Fast Money"
    (let [m1 (core/fast-money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/fast-money-of 3 :eur)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 (core/fast-money-of 1 :gbp))))))
  (t/testing "Mix money types"
    (let [m1 (core/money-of 1 :eur)
          m2 (core/fast-money-of 2 :eur)
          m3 (core/rounded-money-of 3 :eur)]
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m2 m3))))))
