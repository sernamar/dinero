(ns dinero.parse-test
  (:require [dinero.core :as core]
            [dinero.parse :as sut]
            [clojure.test :as t])
  (:import [java.text ParseException]
           [java.util Locale]))

(t/deftest valid-grouping?
  (let [germany Locale/GERMANY]
    (t/is (sut/valid-grouping? "1.234,56" germany))
    (t/is (sut/valid-grouping? "1.234,56 €" germany))
    (t/is (sut/valid-grouping? "1.234" germany))
    (t/is (sut/valid-grouping? "1.234 €" germany))
    (t/is (not (sut/valid-grouping? "12.34" germany)))
    (t/is (not (sut/valid-grouping? "12.34 €" germany)))
    (t/is (not (sut/valid-grouping? "12.3456" germany)))
    (t/is (not (sut/valid-grouping? "12.3456 €" germany))))
  (let [uk Locale/UK]
    (t/is (sut/valid-grouping? "1,234.56" uk))
    (t/is (sut/valid-grouping? "£1,234.56" uk))
    (t/is (sut/valid-grouping? "1,234" uk))
    (t/is (sut/valid-grouping? "£1,234" uk))
    (t/is (not (sut/valid-grouping? "12,34" uk)))
    (t/is (not (sut/valid-grouping? "£12,34" uk)))
    (t/is (not (sut/valid-grouping? "12,3456" uk)))
    (t/is (not (sut/valid-grouping? "£12,3456" uk)))))

(t/deftest parse-string-with-symbol-or-code
  (t/testing "Non-breaking space (U+00A0)"
    (let [m1 (core/money-of 1234.56 :eur)
          m2 (core/money-of 1234.56 :gbp)
          germany Locale/GERMANY
          uk Locale/UK]
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56 €" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56 EUR" germany :eur)))
      (t/is (= m2 (sut/parse-string-with-symbol-or-code "1.234,56 £" germany :gbp)))
      (t/is (= m2 (sut/parse-string-with-symbol-or-code "1.234,56 GBP" germany :gbp)))
      (t/is (= m2 (sut/parse-string-with-symbol-or-code "£1,234.56" uk :gbp)))
      (t/is (= m2 (sut/parse-string-with-symbol-or-code "GBP1,234.56" uk :gbp)))
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "1.234,56 €" germany :gbp)))
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "£1,234.56" germany :gbp)))))
  (t/testing "Multiple regular spaces"
    (let [m1 (core/money-of 1234.56 :eur)
          germany Locale/GERMANY]
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56 €" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56  €" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56    €" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56 EUR" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56  EUR" germany :eur)))
      (t/is (= m1 (sut/parse-string-with-symbol-or-code "1.234,56    EUR" germany :eur)))))
  (t/testing "Invalid grouping"
    (let [germany Locale/GERMANY]
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "12.34 €" germany :eur)))
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "12.34 EUR" germany :eur)))
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "12.3456 €" germany :eur)))
      (t/is (thrown? ParseException (sut/parse-string-with-symbol-or-code "12.3456 EUR" germany :eur))))))

(t/deftest attempt-parse-string-with-multiple-currencies
  (let [m1 (core/money-of 1234.56 :eur)
        germany Locale/GERMANY]
    (t/is (= m1 (sut/attempt-parse-string-with-multiple-currencies "1.234,56 €" germany [:eur :gbp])))
    (t/is (= m1 (sut/attempt-parse-string-with-multiple-currencies "1.234,56 €" germany [:gbp :eur])))
    (t/is (thrown? ParseException (sut/attempt-parse-string-with-multiple-currencies "1.234,56 €" germany [:gbp :btc])))
    (t/is (thrown? ParseException (sut/attempt-parse-string-with-multiple-currencies "1.234,56 ₿" germany [:eur :gbp])))))

(t/deftest attempt-parse-string-with-all-currencies
  (let [m1 (core/money-of 1234.56 :eur)
        m2 (core/money-of 1234.56 :gbp)
        m3 (core/money-of 1234.56 :btc)
        germany Locale/GERMANY]
    (t/is (= m1 (sut/attempt-parse-string-with-all-currencies "1.234,56 €" germany)))
    (t/is (= m1 (sut/attempt-parse-string-with-all-currencies "1.234,56 EUR" germany)))
    (t/is (= m2 (sut/attempt-parse-string-with-all-currencies "1.234,56 £" germany)))
    (t/is (= m2 (sut/attempt-parse-string-with-all-currencies "1.234,56 GBP" germany)))
    (t/is (= m3 (sut/attempt-parse-string-with-all-currencies "1.234,56 ₿" germany)))
    (t/is (= m3 (sut/attempt-parse-string-with-all-currencies "1.234,56 BTC" germany)))
    (t/is (thrown? ParseException (sut/attempt-parse-string-with-all-currencies "1,234.56 €" germany)))))

(t/deftest parse-string
  (t/testing "Different currencies"
      (let [m1 (core/money-of 1234.56 :eur)
            m2 (core/money-of 1234.56 :gbp)
            m3 (core/money-of 1234.56 :btc)
            germany Locale/GERMANY]
        (t/is (= m1 (sut/parse-string "1.234,56 €" {:locale germany})))
        (t/is (= m1 (sut/parse-string "1.234,56 €" {:locale germany :currencies [:eur :gbp]})))
        (t/is (= m1 (sut/parse-string "1.234,56 €" {:locale germany :currencies [:gbp :eur]})))
        (t/is (thrown? ParseException (sut/parse-string "1.234,56 £" {:locale germany})))
        (t/is (= m2 (sut/parse-string "1.234,56 £" {:locale germany :try-all-currencies? true})))
        (t/is (= m2 (sut/parse-string "1.234,56 £" {:locale germany :currencies [:eur :gbp]})))
        (t/is (thrown? ParseException (sut/parse-string "1.234,56 ₿" {:locale germany})))
        (t/is (= m3 (sut/parse-string "1.234,56 ₿" {:locale germany :try-all-currencies? true})))
        (t/is (= m3 (sut/parse-string "1.234,56 ₿" {:locale germany :currencies [:eur :gbp :btc]})))
        (t/is (thrown? ParseException (sut/parse-string "1,234.56 €" {:locale germany})))))
  (t/testing "Same symbol in different locales"
    (let [us-dollars (core/money-of 1234.56 :usd)
          canadian-dollars (core/money-of 1234.56 :cad)]
      (t/is (= us-dollars (sut/parse-string "$1,234.56" {:locale Locale/US})))
      (t/is (= canadian-dollars (sut/parse-string "$1,234.56" {:locale Locale/CANADA})))))
  (t/testing "Invalid grouping"
    (let [germany Locale/GERMANY]
      (t/is (thrown? ParseException (sut/parse-string "12.34 €" {:locale germany :currencies [:eur]})))
      (t/is (thrown? ParseException (sut/parse-string "12.3456 €" {:locale germany :currencies [:eur]}))))))
