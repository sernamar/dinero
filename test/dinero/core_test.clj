(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [java.text ParseException]
           [java.util Locale]))

(t/deftest money-of
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
    (t/is (= :eur (sut/get-currency m4))))
  (t/is (thrown? ExceptionInfo (sut/money-of 1 nil)))
  (t/is (thrown? ExceptionInfo (sut/money-of 1 :unknown-currency))))

(t/deftest rounded-money-of
  (let [m1 (sut/rounded-money-of 1234.5678 :eur 2 :down)
        m2 (sut/rounded-money-of 1234.5678 :eur 0 :down)]
    (t/is (= 1234.56M (sut/get-amount m1)))
    (t/is (= 1234M (sut/get-amount m2)))
    (t/is (= :eur (sut/get-currency m1)))
    (t/is (= :eur (sut/get-currency m2)))
    (t/is (= 2 (sut/get-scale m1)))
    (t/is (zero? (sut/get-scale m2)))
    (t/is (= :down (sut/get-rounding-mode m1)))
    (t/is (= :down (sut/get-rounding-mode m2)))
    (t/is (thrown? ExceptionInfo (sut/rounded-money-of 1234.5678 :eur -1 :down)))))
