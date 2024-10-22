(ns dinero.conversion.db-test
  (:require [dinero.conversion.db :as sut]
            [dinero.conversion.test-helper :as h]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [org.h2.jdbc JdbcSQLSyntaxErrorException]))

(t/use-fixtures :once h/db-fixture)

(t/deftest create-db-rates-provider
  (let [db-rate-provider (sut/create-db-rate-provider h/db-spec "exchange_rate" "from_currency" "to_currency" "rate")]
    (t/is (= 0.80 (db-rate-provider :eur :gbp)))
    (t/is (= 1.25 (db-rate-provider :gbp :eur)))
    (t/is (thrown? ExceptionInfo (db-rate-provider :eur :jpy))))
  (t/testing "Wrong db"
    (let [wrong-db {:dbtype "h2:mem" :dbname "wrong-db"}
          db-rate-provider (sut/create-db-rate-provider wrong-db "exchange_rate" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong table name"
    (let [db-rate-provider (sut/create-db-rate-provider h/db-spec "wrong_table_name" "from_currency" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp)))))
  (t/testing "Wrong column name"
    (let [db-rate-provider (sut/create-db-rate-provider h/db-spec "exchange_rate" "wrong_column_name" "to_currency" "rate")]
      (t/is (thrown? JdbcSQLSyntaxErrorException (db-rate-provider :eur :gbp))))))
