(ns dinero.conversion.db-test
  (:require [dinero.conversion.db :as sut]
            [clojure.test :as t]
            [next.jdbc :as jdbc])
  (:import [clojure.lang ExceptionInfo]
           [org.h2.jdbc JdbcSQLSyntaxErrorException]))

(def db-spec {:dbtype "h2:mem" :dbname "db-test"})

(defn- setup-db
  []
  (jdbc/execute-one! db-spec ["CREATE TABLE IF NOT EXISTS exchange_rate (from_currency VARCHAR(3), to_currency VARCHAR(3), rate DOUBLE, date DATE)"])
  (jdbc/execute-one! db-spec ["INSERT INTO exchange_rate (from_currency, to_currency, rate, date) VALUES ('EUR', 'GBP', 0.80, '2024-09-08')"]))

(defn- teardown-db
  []
  (jdbc/execute-one! db-spec ["DROP TABLE IF EXISTS exchange_rate"]))

(defn- db-fixture
  [f]
  (setup-db)
  (f)
  (teardown-db))

(t/use-fixtures :once db-fixture)

(t/deftest create-db-rates-provider
  (let [db-rate-provider (sut/create-db-rate-provider db-spec "exchange_rate" "from_currency" "to_currency" "rate")]
    (t/is (= 0.80 (db-rate-provider :eur :gbp)))
    (t/is (= 1.25 (db-rate-provider :gbp :eur)))
    (t/is (thrown? ExceptionInfo (db-rate-provider :eur :jpy))))
  (t/testing "Wrong db"
    (let [wrong-db {:dbtype "h2:mem" :dbname "wrong-db"}
          db-rate-provider (sut/create-db-rate-provider wrong-db "exchange_rate" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong table name"
    (let [db-rate-provider (sut/create-db-rate-provider db-spec "wrong_table_name" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong column name"
    (let [db-rate-provider (sut/create-db-rate-provider db-spec "exchange_rate" "wrong_column_name" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp))))))
