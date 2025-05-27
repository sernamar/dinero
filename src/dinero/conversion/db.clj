;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.conversion.db
  (:require [dinero.utils :as utils]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn create-db-rate-provider
  "Creates a function to fetch currency conversion rates from a database."
  ([db table from-field to-field rate-field]
   (fn [from-currency to-currency]
     (let [query (str "SELECT " rate-field
                      " FROM " table
                      " WHERE " from-field " = ? AND " to-field " = ?")
           from-currency (utils/to-uppercase-string from-currency)
           to-currency (utils/to-uppercase-string to-currency)
           query-with-params (fn [from to]
                               (jdbc/execute-one! db [query from to]
                                                  {:builder-fn rs/as-unqualified-lower-maps}))]
       (if-let [result (query-with-params from-currency to-currency)]
         (:rate result)
         (if-let [inverse-result (query-with-params to-currency from-currency)]
           (/ 1 (:rate inverse-result))
           (throw (ex-info "Rate not found" {:currencies [from-currency to-currency]})))))))
  ([db table from-field to-field rate-field date-field]
   (fn [from-currency to-currency date]
     (let [query (str "SELECT " rate-field
                      " FROM " table
                      " WHERE " from-field " = ? AND " to-field " = ? AND " date-field " = ?")
           from-currency (utils/to-uppercase-string from-currency)
           to-currency (utils/to-uppercase-string to-currency)
           query-with-params (fn [from to date]
                               (jdbc/execute-one! db [query from to date]
                                                  {:builder-fn rs/as-unqualified-lower-maps}))]
       (if-let [result (query-with-params from-currency to-currency date)]
         (:rate result)
         (if-let [inverse-result (query-with-params to-currency from-currency date)]
           (/ 1 (:rate inverse-result))
           (throw (ex-info "Rate not found" {:currencies [from-currency to-currency]}))))))))
