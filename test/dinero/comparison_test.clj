(ns dinero.comparison-test
  (:require [dinero.comparison :as sut]
            [dinero.core :as core]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))


;;; Equality and comparison

(t/deftest comparison
  ;; money
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
    (t/is (thrown? ExceptionInfo (sut/money>= m1 m4))))
  ;; rounded money
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
    (t/is (thrown? ExceptionInfo (sut/money>= m1 m4))))
  ;; mixed money and rounded money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/rounded-money-of 1 :eur)]
    (t/is (thrown? ExceptionInfo (sut/money< m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/money<= m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/money> m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/money>= m1 m2)))))

(t/deftest sign-operations
  (let [m1 (core/money-of 0 :eur)
        m2 (core/money-of 1 :eur)
        m3 (core/money-of -1 :eur)]
    (t/is (sut/money-zero? m1))
    (t/is (sut/money-pos? m2))
    (t/is (sut/money-neg? m3))))
