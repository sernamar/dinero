(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [java.text ParseException]
           [java.util Locale]))

;;; Money creation

(t/deftest test-money-of
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

(t/deftest test-format-with-pattern
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

(t/deftest test-parse-containing-iso-4217-symbol
  (let [germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= (sut/money-of 1234.56 :eur) (sut/parse-containing-iso-4217-symbol "1.234,56 €" germany)))
    (t/is (= (sut/money-of 1234.56 :gbp) (sut/parse-containing-iso-4217-symbol "£1,234.56" uk)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "1.234,56 EUR" germany)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "£1,234.56" germany)))))

;;; Arithmetic operations

(t/deftest test-add
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 3 :eur)]
    (t/is (= (sut/money-of 3 :eur) (sut/add m1 m2)))
    (t/is (= (sut/money-of 6 :eur) (sut/add m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/add (sut/money-of 1 :eur) (sut/money-of 1 :gbp))))))

(t/deftest test-subtract
  (let [m1 (sut/money-of 3 :eur)
        m2 (sut/money-of 2 :eur)
        m3 (sut/money-of 1 :eur)]
    (t/is (= (sut/money-of 1 :eur) (sut/subtract m1 m2)))
    (t/is (= (sut/money-of 0 :eur) (sut/subtract m1 m2 m3)))
    (t/is (thrown? ExceptionInfo (sut/subtract (sut/money-of 1 :eur) (sut/money-of 1 :gbp))))))

(t/deftest test-multiply
  (let [money (sut/money-of 1 :eur)
        factor 2]
    (t/is (= (sut/money-of 2 :eur) (sut/multiply money factor)))))

(t/deftest test-divide
  (let [money (sut/money-of 2 :eur)
        divisor 2]
    (t/is (= (sut/money-of 1 :eur) (sut/divide money divisor)))))

;;; Rounding

(t/deftest test-round
  (let [money(sut/money-of 1234.5678 :eur)
        rounding-1 (sut/create-rounding :up 2)
        rounding-2 (sut/create-rounding :down 2)
        rounding-3 (sut/create-rounding :up 0)
        rounding-4 (sut/create-rounding :down 0)]
    (t/is (= (sut/money-of 1234.57 :eur) (sut/round money rounding-1)))
    (t/is (= (sut/money-of 1234.56 :eur) (sut/round money rounding-2)))
    (t/is (= (sut/money-of 1235 :eur) (sut/round money rounding-3)))
    (t/is (= (sut/money-of 1234 :eur) (sut/round money rounding-4)))))

(t/deftest test-round-chf
  (let [m1 (sut/money-of 0.975 :chf)
        m2 (sut/money-of 1.024 :chf)
        m3 (sut/money-of 1.025 :chf)
        m4 (sut/money-of 1.074 :chf)
        m5 (sut/money-of 1.075 :chf)
        m6 (sut/money-of 1.124 :chf)
        rounding (sut/chf-rounding)]
    (t/is (= (sut/money-of 1 :chf) (sut/round m1 rounding)))
    (t/is (= (sut/money-of 1 :chf) (sut/round m2 rounding)))
    (t/is (= (sut/money-of 1.05 :chf) (sut/round m3 rounding)))
    (t/is (= (sut/money-of 1.05 :chf) (sut/round m4 rounding)))
    (t/is (= (sut/money-of 1.10 :chf) (sut/round m5 rounding)))
    (t/is (= (sut/money-of 1.10 :chf) (sut/round m6 rounding)))))
