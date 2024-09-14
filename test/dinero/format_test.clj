(ns dinero.format-test
  (:require [dinero.core :as core]
            [dinero.format :as sut]
            [clojure.test :as t])
  (:import [java.util Locale]))

(t/deftest test-format-money
  ;; different locales
  (let [m1 (core/money-of 1 :eur)
        m2 (core/money-of 1 :gbp)
        m3 (core/money-of 1 :jpy)
        m4 (core/money-of 1 :btc)
        germany Locale/GERMANY
        uk Locale/UK
        japan Locale/JAPAN]
    (t/is (= "1,00 €" (sut/format-money m1 {:locale germany})))
    (t/is (= "1,00 £" (sut/format-money m2 {:locale germany})))
    (t/is (= "1 ¥" (sut/format-money m3 {:locale germany})))
    (t/is (= "1,00000000 ₿" (sut/format-money m4 {:locale germany})))
    (t/is (= "€1.00" (sut/format-money m1 {:locale uk})))
    (t/is (= "£1.00" (sut/format-money m2 {:locale uk})))
    (t/is (= "JP¥1" (sut/format-money m3 {:locale uk})))
    (t/is (= "₿1.00000000" (sut/format-money m4 {:locale uk})))
    (t/is (= "€1.00" (sut/format-money m1 {:locale japan})))
    (t/is (= "£1.00" (sut/format-money m2 {:locale japan})))
    (t/is (= "￥1" (sut/format-money m3 {:locale japan})))
    (t/is (= "₿1.00000000" (sut/format-money m4 {:locale japan}))))
  ;; different formatting options
  (let [m1 (core/money-of 1234.5678 :eur)
        m2 (core/money-of 1 :btc)
        germany Locale/GERMANY]
    (t/is (= "1.234,57 €" (sut/format-money m1 {:locale germany
                                                :rounding-mode :half-up
                                                :decimal-places 2})))
    (t/is (= "1.234,56 €" (sut/format-money m1 {:locale germany
                                                :rounding-mode :down
                                                :decimal-places 2})))
    (t/is (= "1.234 €" (sut/format-money m1 {:locale germany
                                             :rounding-mode :down
                                             :decimal-places 0})))
    (t/is (= "1.234,57 €" (sut/format-money m1 {:locale germany
                                                :rounding-mode :half-up
                                                :decimal-places 2
                                                :symbol-style :symbol})))
    (t/is (= "1.234,57 EUR" (sut/format-money m1 {:locale germany
                                                  :rounding-mode :half-up
                                                  :decimal-places 2
                                                  :symbol-style :code})))
    (t/is (= "1,00 ₿" (sut/format-money m2 {:locale germany
                                            :rounding-mode :half-up
                                            :decimal-places 2
                                            :symbol-style :symbol})))
    (t/is (= "1,00 BTC" (sut/format-money m2 {:locale germany
                                              :rounding-mode :half-up
                                              :decimal-places 2
                                              :symbol-style :code})))))

(t/deftest format-money-with-pattern
  (let [money (core/money-of 1234.5678 :eur)
        germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= "1.234,57 €" (sut/format-money-with-pattern money "#,##0.00 ¤" {:locale germany})))
    (t/is (= "1.234,57 EUR" (sut/format-money-with-pattern money "#,##0.00 ¤¤" {:locale germany})))
    (t/is (= "1.234,57 euros" (sut/format-money-with-pattern money "#,##0.00 euros" {:locale germany})))
    (t/is (= "1.234,568 €" (sut/format-money-with-pattern money "#,##0.000 ¤" {:locale germany})))
    (t/is (= "1,234.57 £" (sut/format-money-with-pattern money "#,##0.00 ¤" {:locale uk})))
    (t/is (= "1,234.57 GBP" (sut/format-money-with-pattern money "#,##0.00 ¤¤" {:locale uk})))
    (t/is (= "1.234,57 €" (sut/format-money-with-pattern money "#,##0.00 ¤" {:locale germany :rounding-mode :half-up})))
    (t/is (= "1.234,56 €" (sut/format-money-with-pattern money "#,##0.00 ¤" {:locale germany :rounding-mode :down})))))
