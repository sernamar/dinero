(ns dinero.conversion.core
  (:require [dinero.core :as core]
            [dinero.conversion.coinbase :as coinbase]
            [dinero.conversion.db :as db]
            [dinero.conversion.ecb :as ecb]))

(defn convert-using-exchange-rate
  "Converts the given monetary amount to the term currency using the given exchange rate."
  [money term-currency exchange-rate]
  (let [amount (core/get-amount money)
        term-amount (* amount exchange-rate)]
    (cond
      (core/money? money) (core/money-of term-amount term-currency)
      (core/fast-money? money) (core/fast-money-of term-amount term-currency)
      :else (let [decimal-places (core/get-scale money)
                  rounding-mode (core/get-rounding-mode money)]
              (core/rounded-money-of term-amount term-currency decimal-places rounding-mode)))))

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
  ([money term-currency]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [exchange-rate (coinbase/current-rate-provider base-currency term-currency)]
         (convert-using-exchange-rate money term-currency exchange-rate)))))
  ([money term-currency date]
   (let [base-currency (core/get-currency money)]
     (if (= base-currency term-currency)
       money
       (let [exchange-rate (coinbase/historical-rate-provider base-currency term-currency date)]
         (convert-using-exchange-rate money term-currency exchange-rate))))))
