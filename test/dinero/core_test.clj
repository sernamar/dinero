(ns dinero.core-test
  (:require [dinero.core :as sut]
            [clojure.test :as t])
  (:import [java.math RoundingMode]
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
  (t/is (thrown? Exception (sut/money-of 1 nil)))
  (t/is (thrown? Exception (sut/money-of 1 :unknown-currency))))

;;; Formatting

(t/deftest format
  ;; different locales
  (let [m1 (sut/money-of 1 :eur)
        m2 (sut/money-of 1 :gbp)
        m3 (sut/money-of 1 :btc)
        germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= "1,00 €" (sut/format m1 {:locale germany})))
    (t/is (= "1,00 £" (sut/format m2 {:locale germany})))
    (t/is (= "1,00000000 ₿" (sut/format m3 {:locale germany})))
    (t/is (= "€1.00" (sut/format m1 {:locale uk})))
    (t/is (= "£1.00" (sut/format m2 {:locale uk})))
    (t/is (= "₿1.00000000" (sut/format m3 {:locale uk}))))
  ;; different formatting options
  (let [money (sut/money-of 1234.5678 :eur)
        germany Locale/GERMANY]
    (t/is (= "1.234,57 €" (sut/format money {:locale germany
                                             :rounding-mode RoundingMode/HALF_UP
                                             :decimal-places 2})))
    (t/is (= "1.234,56 €" (sut/format money {:locale germany
                                             :rounding-mode RoundingMode/DOWN
                                             :decimal-places 2})))
    (t/is (= "1.234 €" (sut/format money {:locale germany
                                          :rounding-mode RoundingMode/DOWN
                                          :decimal-places 0})))))
