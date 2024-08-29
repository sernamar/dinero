(ns dinero.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

;;; Currencies

(defonce currencies (-> "currencies.edn" io/resource slurp edn/read-string))

;;; Money creation

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  [amount currency]
  (when-not currency
    (throw (ex-info "No currency provided" {})))
  (when-not (contains? currencies currency)
    (throw (ex-info (str "Unknown currency") {:currency currency})))
  {:amount (bigdec amount) :currency currency})

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (:amount money))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))
