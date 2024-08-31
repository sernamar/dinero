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
