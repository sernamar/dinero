(ns dinero.conversion
  (:require [dinero.core :as core]
            [dinero.utils :as utils]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [dinero.core Money RoundedMoney]))

(defmulti convert-with-exchange-rate
  "Converts the given monetary amount to the term currency using the given exchange rate."
  {:arglists '([money term-currency exchange-rate])}
  (fn [money _term-currency _exchange-rate]
    (class money)))

(defmethod convert-with-exchange-rate Money
  [money term-currency exchange-rate]
  (let [amount (core/get-amount money)
        term-amount (* amount exchange-rate)]
    (core/money-of term-amount term-currency)))

(defmethod convert-with-exchange-rate RoundedMoney
  [money term-currency exchange-rate]
  (let [amount (core/get-amount money)
        term-amount (* amount exchange-rate)
        decimal-places (core/get-scale money)
        rounding-mode (core/get-rounding-mode money)]
    (core/rounded-money-of term-amount term-currency decimal-places rounding-mode)))

(defn convert
  "Converts the given monetary amount to the term currency using the given exchange rate provider function."
  ([money term-currency rate-provider-fn]
   (let [exchange-rate (rate-provider-fn (core/get-currency money) term-currency)]
     (convert-with-exchange-rate money term-currency exchange-rate)))
  ([money term-currency date rate-provider-fn]
   (let [exchange-rate (rate-provider-fn (core/get-currency money) term-currency date)]
     (convert-with-exchange-rate money term-currency exchange-rate))))

(defn create-rate-provider-fn-from-db
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
