(ns dinero.math-test
  (:require [dinero.core :as core]
            [dinero.math :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))

(t/deftest add
  ;; money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of 2 :eur)
        m3 (core/money-of 3 :eur)]
    (t/is (= (core/money-of 3 :eur) (sut/add m1 m2)))
    (t/is (= (core/money-of 6 :eur) (sut/add m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/add (core/money-of 1 :eur) (core/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of 1.555 :eur 2 :down)
        m3 (core/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (core/rounded-money-of 3.1 :eur 2 :down) (sut/add m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/add m1 m3))))
  ;; mixed money and rounded money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/rounded-money-of 1 :eur)]
    (t/is (= (core/money-of 2 :eur) (sut/add m1 m2)))))

(t/deftest subtract
  ;; money
  (let [m1 (core/money-of 3 :eur)
        m2 (core/money-of 2 :eur)
        m3 (core/money-of 1 :eur)]
    (t/is (= (core/money-of 1 :eur) (sut/subtract m1 m2)))
    (t/is (= (core/money-of 0 :eur) (sut/subtract m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/subtract (core/money-of 1 :eur) (core/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of 1.555 :eur 2 :down)
        m3 (core/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (core/rounded-money-of 0 :eur 2 :down) (sut/subtract m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/subtract m1 m3))))
  ;; mixed money and rounded money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/rounded-money-of 1 :eur)]
    (t/is (= (core/money-of 0 :eur) (sut/subtract m1 m2)))))

(t/deftest multiply
  ;; money
  (let [money (core/money-of 1 :eur)
        factor 2]
    (t/is (= (core/money-of 2 :eur) (sut/multiply money factor))))
  ;; rounded money
  (let [money (core/rounded-money-of 1.555 :eur 2 :down)
        factor 2]
    (t/is (= (core/rounded-money-of 3.1 :eur 2 :down) (sut/multiply money factor)))))

(t/deftest divide
  ;; money
  (let [money (core/money-of 2 :eur)
        divisor 2]
    (t/is (= (core/money-of 1 :eur) (sut/divide money divisor))))
  ;; rounded money
  (let [money (core/rounded-money-of 1.555 :eur 2 :down)
        divisor 2]
    (t/is (= (core/rounded-money-of 0.77 :eur 2 :down) (sut/divide money divisor)))))

(t/deftest negate
  ;; money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of -1 :eur)]
    (t/is (= m2 (sut/negate m1))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of -1.555 :eur 2 :down)]
    (t/is (= m2 (sut/negate m1)))))

(t/deftest money-abs
  ;; money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of -1 :eur)]
    (t/is (= m1 (sut/money-abs m2))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of -1.555 :eur 2 :down)]
    (t/is (= m1 (sut/money-abs m2)))))

(t/deftest money-max
  ;; money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of 2 :eur)
        m3 (core/money-of 3 :eur)]
    (t/is (= m2 (sut/money-max m1 m2)))
    (t/is (= m3 (sut/money-max m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 (core/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of 2.555 :eur 2 :down)
        m3 (core/rounded-money-of 3.555 :eur 2 :down)
        m4 (core/rounded-money-of 1.555 :eur 2 :up)
        m5 (core/rounded-money-of 1.555 :gbp 2 :down)]
    (t/is (= m2 (sut/money-max m1 m2)))
    (t/is (= m3 (sut/money-max m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 m4)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 m5))))
  ;; mixed money and rounded money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/rounded-money-of 1 :eur)
        m3 (core/rounded-money-of 2 :eur)]
    (t/is (= m1 (sut/money-max m1 m2)))
    (t/is (= m1 (sut/money-max m2 m1)))
    (t/is (= (core/money-of 2 :eur) (sut/money-max m1 m3)))
    (t/is (= (core/money-of 2 :eur) (sut/money-max m3 m1)))))

(t/deftest money-min
  ;; money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of 2 :eur)
        m3 (core/money-of 3 :eur)]
    (t/is (= m1 (sut/money-min m1 m2)))
    (t/is (= m1 (sut/money-min m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 (core/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (core/rounded-money-of 1.555 :eur 2 :down)
        m2 (core/rounded-money-of 2.555 :eur 2 :down)
        m3 (core/rounded-money-of 3.555 :eur 2 :down)
        m4 (core/rounded-money-of 1.555 :eur 2 :up)
        m5 (core/rounded-money-of 1.555 :gbp 2 :down)]
    (t/is (= m1 (sut/money-min m1 m2)))
    (t/is (= m1 (sut/money-min m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 m4)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 m5))))
  ;; mixed money and rounded money
  (let [m1 (core/money-of 1 :eur)
        m2 (core/rounded-money-of 1 :eur)
        m3 (core/rounded-money-of 2 :eur)]
    (t/is (= m1 (sut/money-min m1 m2)))
    (t/is (= m1 (sut/money-min m2 m1)))
    (t/is (= (core/money-of 1 :eur) (sut/money-min m1 m3)))
    (t/is (= (core/money-of 1 :eur) (sut/money-min m3 m1)))))
