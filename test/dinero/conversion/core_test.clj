(ns dinero.conversion.core-test
  (:require [dinero.conversion.core :as sut]
            [dinero.conversion.coinbase :as coinbase]
            [dinero.conversion.db :as db]
            [dinero.conversion.ecb :as ecb]
            [dinero.core :as core]
            [clojure.test :as t]
            [next.jdbc :as jdbc])
  (:import [clojure.lang ExceptionInfo]
           [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(t/deftest convert-with-exchange-rate
  (let [money (core/money-of 1M :eur)
        term-currency :gbp
        exchange-rate 0.80
        converted (sut/convert-with-exchange-rate money term-currency exchange-rate)]
    (t/is (= 0.80M (core/get-amount converted)))
    (t/is (= :gbp (core/get-currency converted)))))

(defn- create-db-for-testing
  []
  (let [db (jdbc/get-datasource {:dbtype "h2:mem" :dbname "core-test"})]
    (jdbc/execute-one! db ["CREATE TABLE exchange_rate (from_currency VARCHAR(3), to_currency VARCHAR(3), rate DOUBLE, date DATE)"])
    (jdbc/execute-one! db ["INSERT INTO exchange_rate (from_currency, to_currency, rate, date) VALUES ('EUR', 'GBP', 0.80, '2024-09-08')"])
    db))

(defonce db (create-db-for-testing))

(t/deftest convert-using-db-provider
  (let [m1 (core/money-of 1M :eur)
        m2 (core/money-of 0.8M :gbp)
        db-rate-provider (db/create-db-rate-provider db "exchange_rate" "from_currency" "to_currency" "rate")
        m1-converted (sut/convert m1 :gbp db-rate-provider)
        m2-converted (sut/convert m2 :eur db-rate-provider)
        m3-converted (sut/convert m1 :eur db-rate-provider)] ; same currency
    (t/is (= 0.80M (core/get-amount m1-converted)))
    (t/is (= :gbp (core/get-currency m1-converted)))
    (t/is (= 1M (core/get-amount m2-converted)))
    (t/is (= :eur (core/get-currency m2-converted)))
    (t/is (= 1M (core/get-amount m3-converted)))
    (t/is (= :eur (core/get-currency m3-converted)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :jpy db-rate-provider))))
  (let [money (core/money-of 1M :eur)
        date (LocalDate/of 2024 9 8)
        db-rate-provider (db/create-db-rate-provider db "exchange_rate" "from_currency" "to_currency" "rate" "date")
        converted (sut/convert money :gbp date db-rate-provider)]
    (t/is (= 0.80M (core/get-amount converted)))
    (t/is (= :gbp (core/get-currency converted)))
    (t/is (thrown? ExceptionInfo (sut/convert money :gbp (LocalDate/of 2024 1 1) db-rate-provider)))))

(t/deftest convert-using-ecb-provider
  (let [m1 (core/money-of 1M :eur)
        ecb-date (:date (ecb/get-ecb-rates))
        query-date (LocalDate/parse ecb-date (DateTimeFormatter/ofPattern "yyyy-M-d"))
        m1-converted (sut/convert m1 :gbp query-date ecb/current-rates-provider)
        m2-converted (sut/convert m1-converted :eur query-date ecb/current-rates-provider)
        m3-converted (sut/convert m1 :eur query-date ecb/current-rates-provider)] ; same currency
    (t/is (> 1M (core/get-amount m1-converted)))
    (t/is (= :gbp (core/get-currency m1-converted)))
    (t/is (= 1M (core/get-amount m2-converted)))
    (t/is (= :eur (core/get-currency m2-converted)))
    (t/is (= 1M (core/get-amount m3-converted)))
    (t/is (= :eur (core/get-currency m3-converted)))
    (t/is (thrown? ExceptionInfo (sut/convert (core/money-of 1 :gbp) :jpy query-date ecb/current-rates-provider)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :gbp (LocalDate/of 2024 1 1) ecb/current-rates-provider)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :invalid query-date ecb/current-rates-provider)))))

(t/deftest convert-using-ecb-historical-provider
  (let [m1 (core/money-of 1M :eur)
        ecb-dates (map :date (ecb/get-ecb-hist90-rates))
        query-date (LocalDate/parse (last ecb-dates) (DateTimeFormatter/ofPattern "yyyy-M-d"))
        m1-converted (sut/convert m1 :gbp query-date ecb/historical-rates-provider)
        m2-converted (sut/convert m1-converted :eur query-date ecb/historical-rates-provider)
        m3-converted (sut/convert m1 :eur query-date ecb/historical-rates-provider)] ; same currency
    (t/is (> 1M (core/get-amount m1-converted)))
    (t/is (= :gbp (core/get-currency m1-converted)))
    (t/is (= 1M (core/get-amount m2-converted)))
    (t/is (= :eur (core/get-currency m2-converted)))
    (t/is (= 1M (core/get-amount m3-converted)))
    (t/is (= :eur (core/get-currency m3-converted)))
    (t/is (thrown? ExceptionInfo (sut/convert (core/money-of 1 :gbp) :jpy query-date ecb/historical-rates-provider)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :gbp (LocalDate/.minusDays query-date 1) ecb/historical-rates-provider)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :invalid query-date ecb/historical-rates-provider)))))

(t/deftest convert-using-coinbase-provider
  (let [m1 (core/money-of 1M :btc)
        m2 (core/money-of 1M :eur)
        m1-converted (sut/convert m1 :btc coinbase/bitcoin-rate-provider)
        m2-converted (sut/convert m1 :eur coinbase/bitcoin-rate-provider)]
    (t/is (= 1M (core/get-amount m1-converted)))
    (t/is (= :btc (core/get-currency m1-converted)))
    (t/is (< 1M (core/get-amount m2-converted)))
    (t/is (= :eur (core/get-currency m2-converted)))
    (t/is (thrown? ExceptionInfo (sut/convert m2 :gbp coinbase/bitcoin-rate-provider)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :invalid coinbase/bitcoin-rate-provider)))))
