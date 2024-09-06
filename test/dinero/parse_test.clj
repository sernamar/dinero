(ns dinero.parse-test
  (:require [dinero.core :as core]
            [dinero.parse :as sut]
            [clojure.test :as t])
  (:import [java.text ParseException]
           [java.util Locale]))

(t/deftest parse-containing-iso-4217-symbol
  (let [germany Locale/GERMANY
        uk Locale/UK]
    (t/is (= (core/money-of 1234.56 :eur) (sut/parse-containing-iso-4217-symbol "1.234,56 €" germany)))
    (t/is (= (core/money-of 1234.56 :gbp) (sut/parse-containing-iso-4217-symbol "£1,234.56" uk)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "1.234,56 EUR" germany)))
    (t/is (thrown? ParseException (sut/parse-containing-iso-4217-symbol "£1,234.56" germany)))))
