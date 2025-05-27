;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))

(t/deftest money-of
  (t/testing "Parse different types of amounts"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 1.0 :eur)
          m3 (sut/money-of 1M :eur)
          m4 (sut/money-of "1" :eur)]
      (t/is (= 1M (sut/get-amount m1)))
      (t/is (= 1M (sut/get-amount m2)))
      (t/is (= 1M (sut/get-amount m3)))
      (t/is (= 1M (sut/get-amount m4)))
      (t/is (= :eur (sut/get-currency m1)))
      (t/is (= :eur (sut/get-currency m2)))
      (t/is (= :eur (sut/get-currency m3)))
      (t/is (= :eur (sut/get-currency m4)))))
  (t/testing "Different currency representations"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 1 :EUR)
          m3 (sut/money-of 1 "eur")
          m4 (sut/money-of 1 "EUR")
          m5 (sut/money-of 1 'eur)
          m6 (sut/money-of 1 'EUR)]
      (t/is (= :eur (sut/get-currency m1)))
      (t/is (= :eur (sut/get-currency m2)))
      (t/is (= :eur (sut/get-currency m3)))
      (t/is (= :eur (sut/get-currency m4)))
      (t/is (= :eur (sut/get-currency m5)))
      (t/is (= :eur (sut/get-currency m6)))))
  (t/testing "Invalid currencies"
    (t/is (thrown? ExceptionInfo (sut/money-of 1 nil)))
    (t/is (thrown? ExceptionInfo (sut/money-of 1 :unknown-currency))))
  (t/testing "Currency with no minor units (`nil`)"
    (let [m1 (sut/money-of 1 :xau)
          m2 (sut/money-of 1.23 :xau)]
      (t/is (= 1M (sut/get-amount m1)))
      (t/is (= 1.23M (sut/get-amount m2))))))

(t/deftest rounded-money-of
  (t/testing "Different decimal places"
    (let [m1 (sut/rounded-money-of 1234.5678 :eur 2 :down)
          m2 (sut/rounded-money-of 1234.5678 :eur 0 :down)]
      (t/is (= 1234.56M (sut/get-amount m1)))
      (t/is (= 1234M (sut/get-amount m2)))
      (t/is (= :eur (sut/get-currency m1)))
      (t/is (= :eur (sut/get-currency m2)))
      (t/is (= 2 (sut/get-scale m1)))
      (t/is (zero? (sut/get-scale m2)))
      (t/is (= :down (sut/get-rounding-mode m1)))
      (t/is (= :down (sut/get-rounding-mode m2)))))
  (t/testing "Invalid (negative) decimal places"
    (t/is (thrown? ExceptionInfo (sut/rounded-money-of 1234.5678 :eur -1 :down))))
  (t/testing "Different currency representations"
    (let [m1 (sut/rounded-money-of 1234.5678 :eur 2 :down)
          m2 (sut/rounded-money-of 1234.5678 :EUR 2 :down)
          m3 (sut/rounded-money-of 1234.5678 "eur" 2 :down)
          m4 (sut/rounded-money-of 1234.5678 "EUR" 2 :down)
          m5 (sut/rounded-money-of 1234.5678 'eur 2 :down)
          m6 (sut/rounded-money-of 1234.5678 'EUR 2 :down)]
      (t/is (= :eur (sut/get-currency m1)))
      (t/is (= :eur (sut/get-currency m2)))
      (t/is (= :eur (sut/get-currency m3)))
      (t/is (= :eur (sut/get-currency m4)))
      (t/is (= :eur (sut/get-currency m5)))
      (t/is (= :eur (sut/get-currency m6)))))
  (t/testing "Invalid currencies"
    (t/is (thrown? ExceptionInfo (sut/rounded-money-of 1234.5678 nil 2 :down)))
    (t/is (thrown? ExceptionInfo (sut/rounded-money-of 1234.5678 :unknown-currency 2 :down))))
  (t/testing "Currency with no minor units (`nil`)"
    (let [m1 (sut/rounded-money-of 1 :xau)
          m2 (sut/rounded-money-of 1.23 :xau)]
      (t/is (= 1M (sut/get-amount m1)))
      (t/is (zero? (sut/get-scale m1)))
      (t/is (= 1.23M (sut/get-amount m2)))
      (t/is (= 2 (sut/get-scale m2))))))

(t/deftest fast-money-of
  (t/testing "Different amounts"
    (let [m1 (sut/fast-money-of 1234 :eur)
          m2 (sut/fast-money-of 1234.56 :eur)
          m3 (sut/fast-money-of 1234.5678 :eur)
          m4 (sut/fast-money-of 0.12345 :btc)]
      (t/is (= 1234M (sut/get-amount m1)))
      (t/is (= 1234.56M (sut/get-amount m2)))
      (t/is (= 1234.5678M (sut/get-amount m3)))
      (t/is (= 0.12345M (sut/get-amount m4)))
      ;; scale exceeds the maximum allowed value of 5
      (t/is (thrown? ExceptionInfo (sut/fast-money-of 0.123456 :btc))))
    (let [max-value (/ Long/MAX_VALUE 100000) ; 10000 = 10^fast-money-max-scale
          min-value (/ Long/MIN_VALUE 100000)] ; 10000 = 10^fast-money-max-scale
      ;; amount cannot be represented as a long
      (t/is (thrown? ExceptionInfo (sut/fast-money-of (inc max-value) :eur)))
      (t/is (thrown? ExceptionInfo (sut/fast-money-of (dec min-value) :eur)))))
  (t/testing "Different currency representations"
    (let [m1 (sut/fast-money-of 1234 :eur)
          m2 (sut/fast-money-of 1234 :EUR)
          m3 (sut/fast-money-of 1234 :eur)
          m4 (sut/fast-money-of 1234 :EUR)
          m5 (sut/fast-money-of 1234 'eur)
          m6 (sut/fast-money-of 1234 'EUR)]
      (t/is (= :eur (sut/get-currency m1)))
      (t/is (= :eur (sut/get-currency m2)))
      (t/is (= :eur (sut/get-currency m3)))
      (t/is (= :eur (sut/get-currency m4)))
      (t/is (= :eur (sut/get-currency m5)))
      (t/is (= :eur (sut/get-currency m6)))))
  (t/testing "Invalid currencies"
    (t/is (thrown? ExceptionInfo (sut/fast-money-of 1234 nil)))
    (t/is (thrown? ExceptionInfo (sut/fast-money-of 1234 :unknown-currency))))
  (t/testing "Currency with no minor units (`nil`)"
    (let [m1 (sut/fast-money-of 1 :xau)
          m2 (sut/fast-money-of 1.23 :xau)]
      (t/is (= 1M (sut/get-amount m1)))
      (t/is (= 1.23M (sut/get-amount m2))))))
