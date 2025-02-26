(ns dinero.conversion.core-test
  (:require [dinero.conversion.core :as sut]
            [dinero.conversion.ecb :as ecb]
            [dinero.conversion.test-helper :as h]
            [dinero.core :as core]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(t/deftest convert-using-exchange-rate
  (t/testing "Money"
    (let [money (core/money-of 1M :eur)
          term-currency :gbp
          exchange-rate 0.80
          converted (sut/convert-using-exchange-rate money term-currency exchange-rate)]
      (t/is (= (core/money-of 0.80 :gbp) converted))))
  (t/testing "Rounded Money"
    (let [money (core/rounded-money-of 1M :eur)
          term-currency :gbp
          exchange-rate 0.80
          converted (sut/convert-using-exchange-rate money term-currency exchange-rate)]
      (t/is (= (core/rounded-money-of 0.80 :gbp) converted))))
  (t/testing "Fast Money"
    (let [money (core/fast-money-of 1M :eur)
          term-currency :gbp
          exchange-rate 0.80
          converted (sut/convert-using-exchange-rate money term-currency exchange-rate)]
      (t/is (= (core/fast-money-of 0.80 :gbp) converted)))))

(t/use-fixtures :once h/db-fixture)

(t/deftest convert-using-db
  (let [m1 (core/money-of 1M :eur)
        converted (sut/convert-using-db m1 :gbp h/db-spec "exchange_rate" "from_currency" "to_currency" "rate")
        converted-back (sut/convert-using-db converted :eur h/db-spec "exchange_rate" "from_currency" "to_currency" "rate") ; round trip
        m1-again (sut/convert-using-db m1 :eur h/db-spec "exchange_rate" "from_currency" "to_currency" "rate")] ; same currency
    (t/is (= (core/money-of 0.80 :gbp) converted))
    (t/is (= 1M (BigDecimal/.setScale (core/get-amount converted-back) 15 BigDecimal/ROUND_HALF_UP)))
    (t/is (= :eur (core/get-currency converted-back)))
    (t/is (= (core/money-of 1M :eur) m1-again))
    (t/is (thrown? ExceptionInfo (sut/convert-using-db m1 :jpy h/db-spec "exchange_rate" "from_currency" "to_currency" "rate"))))
  (let [money (core/money-of 1M :eur)
        date (LocalDate/of 2024 9 8)
        converted (sut/convert-using-db money :gbp date h/db-spec "exchange_rate" "from_currency" "to_currency" "rate" "date")]
    (t/is (= (core/money-of 0.80 :gbp) converted))
    (t/is (thrown? ExceptionInfo (sut/convert-using-db money :gbp (LocalDate/of 2024 1 1) h/db-spec "exchange_rate" "from_currency" "to_currency" "rate" "date")))))

(t/deftest convert-using-ecb
  (t/testing "Current rates"
    (let [m1 (core/money-of 1M :eur)
          ecb-date (:date (ecb/get-ecb-rates))
          query-date (LocalDate/parse ecb-date (DateTimeFormatter/ofPattern "yyyy-M-d"))
          converted (sut/convert-using-ecb m1 :gbp query-date)
          converted-back (sut/convert-using-ecb converted :eur query-date) ; round trip
          m1-again (sut/convert-using-ecb m1 :eur query-date)] ; same currency
      (t/is (> 1M (core/get-amount converted)))
      (t/is (= :gbp (core/get-currency converted)))
      (t/is (= 1M (BigDecimal/.setScale (core/get-amount converted-back) 15 BigDecimal/ROUND_HALF_UP)))
      (t/is (= :eur (core/get-currency converted-back)))
      (t/is (= (core/money-of 1M :eur) m1-again))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb (core/money-of 1 :gbp) :jpy query-date)))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb m1 :gbp (LocalDate/of 2024 1 1))))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb m1 :invalid query-date)))))
  (t/testing "Historical rates"
    (let [m1 (core/money-of 1M :eur)
          ecb-dates (map :date (ecb/get-ecb-hist90-rates))
          query-date (LocalDate/parse (last ecb-dates) (DateTimeFormatter/ofPattern "yyyy-M-d"))
          converted (sut/convert-using-ecb m1 :gbp query-date)
          converted-back (sut/convert-using-ecb converted :eur query-date) ; round trip
          m1-again (sut/convert-using-ecb m1 :eur query-date)] ; same currency
      (t/is (> 1M (core/get-amount converted)))
      (t/is (= :gbp (core/get-currency converted)))
      (t/is (= 1M (BigDecimal/.setScale (core/get-amount converted-back) 15 BigDecimal/ROUND_HALF_UP)))
      (t/is (= :eur (core/get-currency converted-back)))
      (t/is (= (core/money-of 1M :eur) m1-again))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb (core/money-of 1 :gbp) :jpy query-date)))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb m1 :gbp (LocalDate/.minusDays query-date 1))))
      (t/is (thrown? ExceptionInfo (sut/convert-using-ecb m1 :invalid query-date))))))

(t/deftest convert-using-coinbase
  (t/testing "Current rates"
    (let [m1 (core/money-of 1M :btc)
          converted (sut/convert-using-coinbase m1 :eur)
          converted-back (sut/convert-using-coinbase converted :btc) ; round trip
          m1-again (sut/convert-using-coinbase m1 :btc)] ; same currency
      (t/is (< 1M (core/get-amount converted)))
      (t/is (= :eur (core/get-currency converted)))
      (t/is (= 1M (BigDecimal/.setScale (core/get-amount converted-back) 2 BigDecimal/ROUND_HALF_UP)))
      (t/is (= :btc (core/get-currency converted-back)))
      (t/is (= (core/money-of 1M :btc) m1-again))
      (t/is (thrown? ExceptionInfo (sut/convert-using-coinbase m1 :invalid)))))
  (t/testing "Historical rates"
    (let [m1 (core/money-of 1M :btc)
          date (LocalDate/of 2024 9 11)
          converted (sut/convert-using-coinbase m1 :eur date)
          converted-back (sut/convert-using-coinbase converted :btc date) ; round trip
          m1-again (sut/convert-using-coinbase m1 :btc date)] ; same currency
      (t/is (< 1M (core/get-amount converted)))
      (t/is (= :eur (core/get-currency converted)))
      (t/is (= 1M (BigDecimal/.setScale (core/get-amount converted-back) 2 BigDecimal/ROUND_HALF_UP)))
      (t/is (= :btc (core/get-currency converted-back)))
      (t/is (= (core/money-of 1M :btc) m1-again))
      (t/is (thrown? ExceptionInfo (sut/convert-using-coinbase m1 :invalid))))))
