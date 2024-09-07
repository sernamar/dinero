(ns dinero.conversion-test
  (:require [dinero.conversion :as sut]
            [dinero.core :as core]
            [clojure.test :as t]
            [next.jdbc :as jdbc])
  (:import [clojure.lang ExceptionInfo]
           [java.time LocalDate]))

(t/deftest convert-with-exchange-rate
  (let [money (core/money-of 1M :eur)
        term-currency :gbp
        exchange-rate 0.80
        converted (sut/convert-with-exchange-rate money term-currency exchange-rate)]
    (t/is (= 0.80M (core/get-amount converted)))
    (t/is (= :gbp (core/get-currency converted)))))

(defn- create-db-for-testing
  []
  (let [db (jdbc/get-datasource {:dbtype "h2:mem" :dbname "test-db"})]
    (jdbc/execute-one! db ["CREATE TABLE exchange_rate (from_currency VARCHAR(3), to_currency VARCHAR(3), rate DOUBLE, date DATE)"])
    (jdbc/execute-one! db ["INSERT INTO exchange_rate (from_currency, to_currency, rate, date) VALUES ('EUR', 'GBP', 0.80, '2024-09-08')"])
    db))

(defonce db (create-db-for-testing))

(t/deftest convert
  ;; using a database
  (let [m1 (core/money-of 1M :eur)
        m2 (core/money-of 0.8M :gbp)
        rate-provider-fn (sut/create-rate-provider-fn-from-db db "exchange_rate" "from_currency" "to_currency" "rate")
        m1-converted (sut/convert m1 :gbp rate-provider-fn)
        m2-converted (sut/convert m2 :eur rate-provider-fn)]
    (t/is (= 0.80M (core/get-amount m1-converted)))
    (t/is (= :gbp (core/get-currency m1-converted)))
    (t/is (= 1M (core/get-amount m2-converted)))
    (t/is (= :eur (core/get-currency m2-converted)))
    (t/is (thrown? ExceptionInfo (sut/convert m1 :jpy rate-provider-fn))))
  (let [money (core/money-of 1M :eur)
        date (LocalDate/of 2024 9 8)
        rate-provider-fn (sut/create-rate-provider-fn-from-db db "exchange_rate" "from_currency" "to_currency" "rate" "date")
        converted (sut/convert money :gbp date rate-provider-fn)]
    (t/is (= 0.80M (core/get-amount converted)))
    (t/is (= :gbp (core/get-currency converted)))
    (t/is (thrown? ExceptionInfo (sut/convert money :gbp (LocalDate/of 2024 1 1) rate-provider-fn)))))
