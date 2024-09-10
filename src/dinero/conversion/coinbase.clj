(ns dinero.conversion.coinbase
  (:require [hato.client :as hato]
            [clojure.string :as str]))

(defn- get-supported-currencies
  "Returns the supported currencies from Coinbase."
  []
  (let [url "https://api.coinbase.com/v2/currencies"
        response (hato/get url {:as :json})
        currencies (get-in response [:body :data])]
    (map #(keyword (str/lower-case (get % :id))) currencies)))

(defonce supported-currencies (get-supported-currencies))

(defn- get-exchange-rates
  "Returns the exchange rates from Coinbase."
  []
  (let [url "https://api.coinbase.com/v2/exchange-rates?currency=BTC"
        response (hato/get url {:as :json})
        rates (get-in response [:body :data :rates])]
    (reduce-kv
     (fn [acc k v]
       (assoc acc (-> k name str/lower-case keyword) (parse-double v)))
     (sorted-map)
     rates)))

(defn bitcoin-rate-provider
  "Returns the exchange rate for the given currency pair from Coinbase."
  [from-currency to-currency]
  (if (= from-currency to-currency)
    1
    (let [rates (get-exchange-rates)
          base-currency :btc]
      (cond
        (not (contains? #{from-currency to-currency} base-currency))
        (throw (ex-info "The Coinbase provider only supports conversions from/to :btc"
                        {:requested-currencies [from-currency to-currency]
                         :provider :coinbase}))

        (not-any? #{from-currency to-currency} supported-currencies)
        (throw (ex-info "Coinbase exchange rate not available for the requested currency pair"
                        {:requested-currencies [from-currency to-currency]
                         :provider :coinbase}))

        :else
        (if (= from-currency base-currency)
          (rates to-currency)
          (/ 1 (rates from-currency)))))))
