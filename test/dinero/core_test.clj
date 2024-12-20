(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))

;;; Money creation

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
  (t/testing "Currency with no minor units (`nil`)"
    (let [m1 (sut/rounded-money-of 1 :xau)
          m2 (sut/rounded-money-of 1.23 :xau)]
      (t/is (= 1M (sut/get-amount m1)))
      (t/is (zero? (sut/get-scale m1)))
      (t/is (= 1.23M (sut/get-amount m2)))
      (t/is (= 2 (sut/get-scale m2))))))

;;; Equality and comparison

(t/deftest comparison
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 1 :eur)
          m3 (sut/money-of 2 :eur)
          m4 (sut/money-of 1 :gbp)]
      (t/is (sut/money< m1 m3))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m3 m1))
      (t/is (sut/money>= m3 m2))
      (t/is (thrown? ExceptionInfo (sut/money< m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m4)))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1 :eur)
          m2 (sut/rounded-money-of 1 :eur)
          m3 (sut/rounded-money-of 2 :eur)
          m4 (sut/rounded-money-of 1 :gbp)]
      (t/is (sut/money< m1 m3))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m3 m1))
      (t/is (sut/money>= m3 m2))
      (t/is (thrown? ExceptionInfo (sut/money< m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money<= m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money> m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money>= m1 m4)))))
  (t/testing "Mix money and rounded money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/rounded-money-of 2 :eur)]
      (t/is (sut/money< m1 m2))
      (t/is (sut/money<= m1 m2))
      (t/is (sut/money> m2 m1))
      (t/is (sut/money>= m2 m1)))))

(t/deftest sign-operations
  (let [m1 (sut/money-of 0 :eur)
        m2 (sut/money-of 1 :eur)
        m3 (sut/money-of -1 :eur)]
    (t/is (sut/money-zero? m1))
    (t/is (sut/money-pos? m2))
    (t/is (sut/money-neg? m3))))

;;; Arithmetic operations

(t/deftest add
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 2 :eur)
          m3 (sut/money-of 3 :eur)]
      (t/is (= (sut/money-of 3 :eur) (sut/add m1 m2)))
      (t/is (= (sut/money-of 6 :eur) (sut/add m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/add (sut/money-of 1 :eur) (sut/money-of 1 :gbp))))))
  (t/testing "Rounded money")
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of 1.555 :eur 2 :down)
        m3 (sut/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (sut/rounded-money-of 3.1 :eur 2 :down) (sut/add m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/add m1 m3))))
  (t/testing "Mix money and rounded money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/rounded-money-of 1 :eur)]
      (t/is (= (sut/money-of 2 :eur) (sut/add m1 m2))))))

(t/deftest subtract
  (t/testing "Money"
    (let [m1 (sut/money-of 3 :eur)
          m2 (sut/money-of 2 :eur)
          m3 (sut/money-of 1 :eur)]
      (t/is (= (sut/money-of 1 :eur) (sut/subtract m1 m2)))
      (t/is (= (sut/money-of 0 :eur) (sut/subtract m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/subtract (sut/money-of 1 :eur) (sut/money-of 1 :gbp))))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
          m2 (sut/rounded-money-of 1.555 :eur 2 :down)
          m3 (sut/rounded-money-of 1.555 :eur 2 :up)]
      (t/is (= (sut/rounded-money-of 0 :eur 2 :down) (sut/subtract m1 m2)))
      (t/is (thrown? ExceptionInfo (sut/subtract m1 m3)))))
  (t/testing "Mix money and rounded money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/rounded-money-of 1 :eur)]
      (t/is (= (sut/money-of 0 :eur) (sut/subtract m1 m2))))))

(t/deftest multiply
  (t/testing "Money"
    (let [money (sut/money-of 1 :eur)
          factor 2]
      (t/is (= (sut/money-of 2 :eur) (sut/multiply money factor)))))
  (t/testing "Rounded money"
    (let [money (sut/rounded-money-of 1.555 :eur 2 :down)
          factor 2]
      (t/is (= (sut/rounded-money-of 3.1 :eur 2 :down) (sut/multiply money factor))))))

(t/deftest divide
  (t/testing "Money"
    (let [money (sut/money-of 2 :eur)
          divisor 2]
      (t/is (= (sut/money-of 1 :eur) (sut/divide money divisor)))))
  (t/testing "Non-terminating decimal expansion"
    ;; Dividing 1 by 3 cannot be exactly represented as a BigDecimal.
    ;; Without specifying a scale and rounding mode, attempting this division
    ;; will throw a java.lang.ArithmeticException: Non-terminating decimal expansion.
    (let [money (sut/money-of 1 :eur)
          divisor 3]
      (t/is (= (sut/money-of 0.3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333M :eur) (sut/divide money divisor)))))
  (t/testing "Rounded money"
    (let [money (sut/rounded-money-of 1.555 :eur 2 :down)
          divisor 2]
      (t/is (= (sut/rounded-money-of 0.77 :eur 2 :down) (sut/divide money divisor))))))

(t/deftest negate
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of -1 :eur)]
      (t/is (= m2 (sut/negate m1)))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
          m2 (sut/rounded-money-of -1.555 :eur 2 :down)]
      (t/is (= m2 (sut/negate m1))))))

(t/deftest money-abs
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of -1 :eur)]
      (t/is (= m1 (sut/money-abs m1)))
      (t/is (= m1 (sut/money-abs m2)))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
          m2 (sut/rounded-money-of -1.555 :eur 2 :down)]
      (t/is (= m1 (sut/money-abs m1)))
      (t/is (= m1 (sut/money-abs m2))))))

(t/deftest money-max
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 2 :eur)
          m3 (sut/money-of 3 :eur)]
      (t/is (= m2 (sut/money-max m1 m2)))
      (t/is (= m3 (sut/money-max m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 (sut/money-of 1 :gbp))))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
          m2 (sut/rounded-money-of 2.555 :eur 2 :down)
          m3 (sut/rounded-money-of 3.555 :eur 2 :down)
          m4 (sut/rounded-money-of 1.555 :eur 2 :up)
          m5 (sut/rounded-money-of 1.555 :gbp 2 :down)]
      (t/is (= m2 (sut/money-max m1 m2)))
      (t/is (= m3 (sut/money-max m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money-max m1 m5)))))
  (t/testing "Mix money and rounded money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/rounded-money-of 1 :eur)
          m3 (sut/rounded-money-of 2 :eur)]
      (t/is (= m1 (sut/money-max m1 m2)))
      (t/is (= m1 (sut/money-max m2 m1)))
      (t/is (= (sut/money-of 2 :eur) (sut/money-max m1 m3)))
      (t/is (= (sut/money-of 2 :eur) (sut/money-max m3 m1))))))

(t/deftest money-min
  (t/testing "Money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/money-of 2 :eur)
          m3 (sut/money-of 3 :eur)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 (sut/money-of 1 :gbp))))))
  (t/testing "Rounded money"
    (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
          m2 (sut/rounded-money-of 2.555 :eur 2 :down)
          m3 (sut/rounded-money-of 3.555 :eur 2 :down)
          m4 (sut/rounded-money-of 1.555 :eur 2 :up)
          m5 (sut/rounded-money-of 1.555 :gbp 2 :down)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m1 m2 m3)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 m4)))
      (t/is (thrown? ExceptionInfo (sut/money-min m1 m5)))))
  (t/testing "Mix money and rounded money"
    (let [m1 (sut/money-of 1 :eur)
          m2 (sut/rounded-money-of 1 :eur)
          m3 (sut/rounded-money-of 2 :eur)]
      (t/is (= m1 (sut/money-min m1 m2)))
      (t/is (= m1 (sut/money-min m2 m1)))
      (t/is (= (sut/money-of 1 :eur) (sut/money-min m1 m3)))
      (t/is (= (sut/money-of 1 :eur) (sut/money-min m3 m1))))))
