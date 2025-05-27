;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.conversion.coinbase
  (:require [hato.client :as hato])
  (:import [clojure.lang ExceptionInfo]))

(set! *warn-on-reflection* true)

(defn current-rate-provider
  "Returns the exchange rate for the given currency pair from Coinbase."
  [from-currency to-currency]
  (if (= from-currency to-currency)
    1
    (let [currency-pair (str (name from-currency) "-" (name to-currency))
          url (str "https://api.coinbase.com/v2/prices/" currency-pair "/spot")]
      (try
        (let [response (hato/get url {:as :json})]
          (bigdec (get-in response [:body :data :amount])))
        (catch ExceptionInfo _e
          (throw (ex-info "Rate not found" {:currencies [from-currency to-currency]})))))))

(defn historical-rate-provider
  "Returns the historical exchange rate for the given currency pair from Coinbase."
  [from-currency to-currency date]
  (if (= from-currency to-currency)
    1
    (let [currency-pair (str (name from-currency) "-" (name to-currency))
          url (str "https://api.coinbase.com/v2/prices/" currency-pair "/spot?date=" date)]
      (try
        (let [response (hato/get url {:as :json})]
          (bigdec (get-in response [:body :data :amount])))
        (catch ExceptionInfo _e
          (throw (ex-info "Rate not found" {:currencies [from-currency to-currency]
                                            :date (str date)})))))))
