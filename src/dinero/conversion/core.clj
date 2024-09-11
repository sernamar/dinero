(ns dinero.conversion.core
  (:require [dinero.core :as core]
            [dinero.conversion.coinbase :as coinbase]
            [dinero.conversion.db :as db]
            [dinero.conversion.ecb :as ecb]))

(defn convert-using-exchange-rate
  "Converts the given monetary amount to the term currency using the given exchange rate."
  [money term-currency exchange-rate]
  (if (core/money? money)
    (let [amount (core/get-amount money)
          term-amount (* amount exchange-rate)]
      (core/money-of term-amount term-currency))
    (let [amount (core/get-amount money)
          decimal-places (core/get-scale money)
          rounding-mode (core/get-rounding-mode money)
          term-amount (* amount exchange-rate)]
      (core/rounded-money-of term-amount term-currency decimal-places rounding-mode))))

(defn convert-using-db
  "Converts the given monetary amount to the term currency using the given database and schema information."
  ([money term-currency db table from-field to-field rate-field]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [rate-provider (db/create-db-rate-provider db table from-field to-field rate-field)
             rate (rate-provider base-currency term-currency)]
         (convert-using-exchange-rate money term-currency rate)))))
  ([money term-currency date db table from-field to-field rate-field date-field]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [rate-provider (db/create-db-rate-provider db table from-field to-field rate-field date-field)
             rate (rate-provider base-currency term-currency date)]
         (convert-using-exchange-rate money term-currency rate))))))

(defn convert-using-ecb
  "Converts the given monetary amount to the term currency using the European Central Bank rates."
  ([money term-currency]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [rate (ecb/current-rates-provider base-currency term-currency)]
         (convert-using-exchange-rate money term-currency rate)))))
  ([money term-currency date]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [rate (ecb/historical-rates-provider base-currency term-currency date)]
         (convert-using-exchange-rate money term-currency rate))))))

(defn convert-using-coinbase
  "Converts the given monetary amount to the term currency using the Coinbase rates."
  [money term-currency]
  (let [base-currency (core/get-currency money)]
    (if (= base-currency term-currency)
      money
      (let [exchange-rate (coinbase/bitcoin-rate-provider base-currency term-currency)]
        (convert-using-exchange-rate money term-currency exchange-rate)))))
