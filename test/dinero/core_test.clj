(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [java.text ParseException]
           [java.util Locale]))

;;; Money creation

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
    (t/is (= 0 (sut/get-scale m2)))
    (t/is (= :down (sut/get-rounding-mode m1)))
    (t/is (= :down (sut/get-rounding-mode m2)))
    (t/is (thrown? ExceptionInfo (sut/rounded-money-of 1234.5678 :eur -1 :down)))))

;;; Formatting

(t/deftest test-format
  ;; different locales
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 1 :gbp)
        m3 (sut/money-of 1 :jpy)
        m4 (sut/money-of 1 :btc)
        germany Locale/GERMANY
        uk Locale/UK
        japan Locale/JAPAN]
    (t/is (= "1,00 €" (sut/format m1 {:locale germany})))
    (t/is (= "1,00 £" (sut/format m2 {:locale germany})))
    (t/is (= "1 ¥" (sut/format m3 {:locale germany})))
    (t/is (= "1,00000000 ₿" (sut/format m4 {:locale germany})))
    (t/is (= "€1.00" (sut/format m1 {:locale uk})))
    (t/is (= "£1.00" (sut/format m2 {:locale uk})))
    (t/is (= "JP¥1" (sut/format m3 {:locale uk})))
    (t/is (= "₿1.00000000" (sut/format m4 {:locale uk})))
    (t/is (= "€1.00" (sut/format m1 {:locale japan})))
    (t/is (= "£1.00" (sut/format m2 {:locale japan})))
    (t/is (= "￥1" (sut/format m3 {:locale japan})))
    (t/is (= "₿1.00000000" (sut/format m4 {:locale japan}))))
  ;; different formatting options
  (let [m1 (sut/money-of 1234.5678 :eur)
        m2 (sut/money-of 1 :btc)
        germany Locale/GERMANY]
    (t/is (= "1.234,57 €" (sut/format m1 {:locale germany
                                          :rounding-mode :half-up
                                          :decimal-places 2})))
    (t/is (= "1.234,56 €" (sut/format m1 {:locale germany
                                          :rounding-mode :down
                                          :decimal-places 2})))
    (t/is (= "1.234 €" (sut/format m1 {:locale germany
                                       :rounding-mode :down
                                       :decimal-places 0})))
    (t/is (= "1.234,57 €" (sut/format m1 {:locale germany
                                          :rounding-mode :half-up
                                          :decimal-places 2
                                          :symbol-style :symbol})))
    (t/is (= "1.234,57 EUR" (sut/format m1 {:locale germany
                                            :rounding-mode :half-up
                                            :decimal-places 2
                                            :symbol-style :code})))
    (t/is (= "1,00 ₿" (sut/format m2 {:locale germany
                                      :rounding-mode :half-up
                                      :decimal-places 2
                                      :symbol-style :symbol})))
    (t/is (= "1,00 BTC" (sut/format m2 {:locale germany
                                        :rounding-mode :half-up
                                        :decimal-places 2
                                        :symbol-style :code})))))

(t/deftest format-with-pattern
  (let [money (sut/money-of 1234.5678 :eur)
        germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= "1.234,57 €" (sut/format-with-pattern money "#,##0.00 ¤" {:locale germany})))
    (t/is (= "1.234,57 EUR" (sut/format-with-pattern money "#,##0.00 ¤¤" {:locale germany})))
    (t/is (= "1.234,57 euros" (sut/format-with-pattern money "#,##0.00 euros" {:locale germany})))
    (t/is (= "1.234,568 €" (sut/format-with-pattern money "#,##0.000 ¤" {:locale germany})))
    (t/is (= "1,234.57 £" (sut/format-with-pattern money "#,##0.00 ¤" {:locale uk})))
    (t/is (= "1,234.57 GBP" (sut/format-with-pattern money "#,##0.00 ¤¤" {:locale uk})))
    (t/is (= "1.234,57 €" (sut/format-with-pattern money "#,##0.00 ¤" {:locale germany :rounding-mode :half-up})))
    (t/is (= "1.234,56 €" (sut/format-with-pattern money "#,##0.00 ¤" {:locale germany :rounding-mode :down})))))

;;; Parsing

(t/deftest parse-containing-iso-4217-symbol
  (let [germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= (sut/money-of 1234.56 :eur) (sut/parse-containing-iso-4217-symbol "1.234,56 €" germany)))
    (t/is (= (sut/money-of 1234.56 :gbp) (sut/parse-containing-iso-4217-symbol "£1,234.56" uk)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "1.234,56 EUR" germany)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "£1,234.56" germany)))))

;;; Equality and comparison

(t/deftest equality
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 1 :eur)
        m3 (sut/money-of 2 :eur)
        m4 (sut/money-of 1 :gbp)]
    (t/is (sut/money= m1 m2))
    (t/is (sut/money-not= m1 m3))
    (t/is (thrown? ExceptionInfo (sut/money= m1 m4)))))

(t/deftest comparison
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

(t/deftest sign-operations
  (let [m1 (sut/money-of 0 :eur)
        m2 (sut/money-of 1 :eur)
        m3 (sut/money-of -1 :eur)]
    (t/is (sut/money-zero? m1))
    (t/is (sut/money-pos? m2))
    (t/is (sut/money-neg? m3))))

;;; Arithmetic operations

(t/deftest add
  ;; money
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 3 :eur)]
    (t/is (= (sut/money-of 3 :eur) (sut/add m1 m2)))
    (t/is (= (sut/money-of 6 :eur) (sut/add m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/add (sut/money-of 1 :eur) (sut/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of 1.555 :eur 2 :down)
        m3 (sut/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (sut/rounded-money-of 3.1 :eur 2 :down) (sut/add m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/add m1 m3)))))

(t/deftest subtract
  ;; money
  (let [m1 (sut/money-of 3 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 1 :eur)]
    (t/is (= (sut/money-of 1 :eur) (sut/subtract m1 m2)))
    (t/is (= (sut/money-of 0 :eur) (sut/subtract m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/subtract (sut/money-of 1 :eur) (sut/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of 1.555 :eur 2 :down)
        m3 (sut/rounded-money-of 1.555 :eur 2 :up)]
    (t/is (= (sut/rounded-money-of 0 :eur 2 :down) (sut/subtract m1 m2)))
    (t/is (thrown? ExceptionInfo (sut/subtract m1 m3)))))

(t/deftest multiply
  ;; money
  (let [money (sut/money-of 1 :eur)
        factor 2]
    (t/is (= (sut/money-of 2 :eur) (sut/multiply money factor))))
  ;; rounded money
  (let [money (sut/rounded-money-of 1.555 :eur 2 :down)
        factor 2]
    (t/is (= (sut/rounded-money-of 3.1 :eur 2 :down) (sut/multiply money factor)))))

(t/deftest divide
  ;; money
  (let [money (sut/money-of 2 :eur)
        divisor 2]
    (t/is (= (sut/money-of 1 :eur) (sut/divide money divisor))))
  ;; rounded money
  (let [money (sut/rounded-money-of 1.555 :eur 2 :down)
        divisor 2]
    (t/is (= (sut/rounded-money-of 0.77 :eur 2 :down) (sut/divide money divisor)))))

(t/deftest negate
  ;; money
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of -1 :eur)]
    (t/is (= m2 (sut/negate m1))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of -1.555 :eur 2 :down)]
    (t/is (= m2 (sut/negate m1)))))

(t/deftest money-abs
  ;; money
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of -1 :eur)]
    (t/is (= m1 (sut/money-abs m2))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of -1.555 :eur 2 :down)]
    (t/is (= m1 (sut/money-abs m2)))))

(t/deftest money-max
  ;; money
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 3 :eur)]
    (t/is (= m2 (sut/money-max m1 m2)))
    (t/is (= m3 (sut/money-max m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 (sut/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of 2.555 :eur 2 :down)
        m3 (sut/rounded-money-of 3.555 :eur 2 :down)
        m4 (sut/rounded-money-of 1.555 :eur 2 :up)
        m5 (sut/rounded-money-of 1.555 :gbp 2 :down)]
    (t/is (= m2 (sut/money-max m1 m2)))
    (t/is (= m3 (sut/money-max m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 m4)))
    (t/is (thrown? ExceptionInfo (sut/money-max m1 m5)))))

(t/deftest money-min
  ;; money
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 3 :eur)]
    (t/is (= m1 (sut/money-min m1 m2)))
    (t/is (= m1 (sut/money-min m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 (sut/money-of 1 :gbp)))))
  ;; rounded money
  (let [m1 (sut/rounded-money-of 1.555 :eur 2 :down)
        m2 (sut/rounded-money-of 2.555 :eur 2 :down)
        m3 (sut/rounded-money-of 3.555 :eur 2 :down)
        m4 (sut/rounded-money-of 1.555 :eur 2 :up)
        m5 (sut/rounded-money-of 1.555 :gbp 2 :down)]
    (t/is (= m1 (sut/money-min m1 m2)))
    (t/is (= m1 (sut/money-min m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 m4)))
    (t/is (thrown? ExceptionInfo (sut/money-min m1 m5)))))

;;; Rounding

(t/deftest round
  (let [money(sut/money-of 1234.5678 :eur)]
    (t/is (= (sut/money-of 1234.57 :eur) (sut/round money 2 :up)))
    (t/is (= (sut/money-of 1234.56 :eur) (sut/round money 2 :down)))
    (t/is (= (sut/money-of 1235 :eur) (sut/round money 0 :up)))
    (t/is (= (sut/money-of 1234 :eur) (sut/round money 0 :down)))))

(t/deftest round-chf
  (let [m1 (sut/money-of 0.975 :chf)
        m2 (sut/money-of 1.024 :chf)
        m3 (sut/money-of 1.025 :chf)
        m4 (sut/money-of 1.074 :chf)
        m5 (sut/money-of 1.075 :chf)
        m6 (sut/money-of 1.124 :chf)]
    (t/is (= (sut/money-of 1 :chf) (sut/round m1 sut/chf-rounding-fn)))
    (t/is (= (sut/money-of 1 :chf) (sut/round m2 sut/chf-rounding-fn)))
    (t/is (= (sut/money-of 1.05 :chf) (sut/round m3 sut/chf-rounding-fn)))
    (t/is (= (sut/money-of 1.05 :chf) (sut/round m4 sut/chf-rounding-fn)))
    (t/is (= (sut/money-of 1.10 :chf) (sut/round m5 sut/chf-rounding-fn)))
    (t/is (= (sut/money-of 1.10 :chf) (sut/round m6 sut/chf-rounding-fn)))))
