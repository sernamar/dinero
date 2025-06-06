;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.conversion.test-helper
  (:require [next.jdbc :as jdbc]))

(def db-spec {:dbtype "h2:mem" :dbname "db-test"})

(defn- setup-db
  []
  (jdbc/execute-one! db-spec ["CREATE TABLE IF NOT EXISTS exchange_rate (from_currency VARCHAR(3), to_currency VARCHAR(3), rate DOUBLE, date DATE)"])
  (jdbc/execute-one! db-spec ["INSERT INTO exchange_rate (from_currency, to_currency, rate, date) VALUES ('EUR', 'GBP', 0.80, '2024-09-08')"]))

(defn- teardown-db
  []
  (jdbc/execute-one! db-spec ["DROP TABLE IF EXISTS exchange_rate"]))

(defn db-fixture
  [f]
  (setup-db)
  (f)
  (teardown-db))

