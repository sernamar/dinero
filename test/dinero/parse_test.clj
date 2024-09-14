(ns dinero.parse-test
  (:require [dinero.core :as core]
            [dinero.parse :as sut]
            [clojure.test :as t])
  (:import [java.text ParseException]
           [java.util Locale]))

(t/deftest parse-with-symbol-or-code
  (let [m1 (core/money-of 1234.56 :eur)
        m2 (core/money-of 1234.56 :gbp)
        germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= m1 (sut/parse-with-symbol-or-code "1.234,56 €" germany :eur)))
    (t/is (= m1 (sut/parse-with-symbol-or-code "1.234,56 EUR" germany :eur)))
    (t/is (= m2 (sut/parse-with-symbol-or-code "1.234,56 £" germany :gbp)))
    (t/is (= m2 (sut/parse-with-symbol-or-code "1.234,56 GBP" germany :gbp)))
    (t/is (= m2 (sut/parse-with-symbol-or-code "£1,234.56" uk :gbp)))
    (t/is (= m2 (sut/parse-with-symbol-or-code "GBP1,234.56" uk :gbp)))
    (t/is (thrown? ParseException (sut/parse-with-symbol-or-code "1.234,56 €" germany :gbp)))
    (t/is (thrown? ParseException (sut/parse-with-symbol-or-code "£1,234.56" germany :gbp)))))

(t/deftest attempt-parse-with-multiple-currencies
  (let [m1 (core/money-of 1234.56 :eur)
        germany Locale/GERMANY]
    (t/is (= m1 (sut/attempt-parse-with-multiple-currencies "1.234,56 €" germany [:eur :gbp])))
    (t/is (= m1 (sut/attempt-parse-with-multiple-currencies "1.234,56 €" germany [:gbp :eur])))
    (t/is (thrown? ParseException (sut/attempt-parse-with-multiple-currencies "1.234,56 €" germany [:gbp :btc])))
    (t/is (thrown? ParseException (sut/attempt-parse-with-multiple-currencies "1.234,56 ₿" germany [:eur :gbp])))))

(t/deftest attempt-parse-with-all-currencies
  (let [m1 (core/money-of 1234.56 :eur)
        m2 (core/money-of 1234.56 :gbp)
        m3 (core/money-of 1234.56 :btc)
        germany Locale/GERMANY]
    (t/is (= m1 (sut/attempt-parse-with-all-currencies "1.234,56 €" germany)))
    (t/is (= m1 (sut/attempt-parse-with-all-currencies "1.234,56 EUR" germany)))
    (t/is (= m2 (sut/attempt-parse-with-all-currencies "1.234,56 £" germany)))
    (t/is (= m2 (sut/attempt-parse-with-all-currencies "1.234,56 GBP" germany)))
    (t/is (= m3 (sut/attempt-parse-with-all-currencies "1.234,56 ₿" germany)))
    (t/is (= m3 (sut/attempt-parse-with-all-currencies "1.234,56 BTC" germany)))
    (t/is (thrown? ParseException (sut/attempt-parse-with-all-currencies "1,234.56 €" germany)))))

(t/deftest parse-money
  (let [m1 (core/money-of 1234.56 :eur)
        m2 (core/money-of 1234.56 :gbp)
        m3 (core/money-of 1234.56 :btc)
        germany Locale/GERMANY]
    (t/is (= m1 (sut/parse-money "1.234,56 €" :locale germany)))
    (t/is (= m1 (sut/parse-money "1.234,56 €" :locale germany :currencies [:eur :gbp])))
    (t/is (= m1 (sut/parse-money "1.234,56 €" :locale germany :currencies [:gbp :eur])))
    (t/is (thrown? ParseException (sut/parse-money "1.234,56 £" :locale germany)))
    (t/is (= m2 (sut/parse-money "1.234,56 £" :locale germany :try-all-currencies? true)))
    (t/is (= m2 (sut/parse-money "1.234,56 £" :locale germany :currencies [:eur :gbp])))
    (t/is (thrown? ParseException (sut/parse-money "1.234,56 ₿" :locale germany)))
    (t/is (= m3 (sut/parse-money "1.234,56 ₿" :locale germany :try-all-currencies? true)))
    (t/is (= m3 (sut/parse-money "1.234,56 ₿" :locale germany :currencies [:eur :gbp :btc])))
    (t/is (thrown? ParseException (sut/parse-money "1,234.56 €" germany)))))
