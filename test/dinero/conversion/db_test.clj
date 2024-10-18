(ns dinero.conversion.db-test
  (:require [dinero.conversion.db :as sut]
            [clojure.test :as t]
            [next.jdbc :as jdbc])
  (:import [clojure.lang ExceptionInfo]
           [org.h2.jdbc JdbcSQLSyntaxErrorException]))

(defn- create-db-for-testing
  []
  (let [db (jdbc/get-datasource {:dbtype "h2:mem" :dbname "db-test"})]
    (jdbc/execute-one! db ["CREATE TABLE exchange_rate (from_currency VARCHAR(3), to_currency VARCHAR(3), rate DOUBLE, date DATE)"])
    (jdbc/execute-one! db ["INSERT INTO exchange_rate (from_currency, to_currency, rate, date) VALUES ('EUR', 'GBP', 0.80, '2024-09-08')"])
    db))

(defonce db (create-db-for-testing))

(t/deftest create-db-rates-provider
  (let [db-rate-provider (sut/create-db-rate-provider db "exchange_rate" "from_currency" "to_currency" "rate")]
    (t/is (= 0.80 (db-rate-provider :eur :gbp)))
    (t/is (= 1.25 (db-rate-provider :gbp :eur)))
    (t/is (thrown? ExceptionInfo (db-rate-provider :eur :jpy))))
  (t/testing "Wrong db"
    (let [wrong-db (jdbc/get-datasource {:dbtype "h2:mem" :dbname "wrong_db"})
          db-rate-provider (sut/create-db-rate-provider wrong-db "exchange_rate" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong table name"
    (let [db-rate-provider (sut/create-db-rate-provider db "wrong_table_name" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong column name"
    (let [db-rate-provider (sut/create-db-rate-provider db "exchange_rate" "wrong_column_name" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp))))))
