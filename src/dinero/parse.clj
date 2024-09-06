(ns dinero.parse
  (:require [dinero.core :as core]
            [clojure.string :as str])
  (:import [java.text DecimalFormat]))

(defn parse-containing-iso-4217-symbol
  "Parses the given string containing an ISO 4217 currency symbol and returns a monetary amount of type `Money`."
  [string locale]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)
        amount (DecimalFormat/.parse ^DecimalFormat formatter string)
        currency (-> (DecimalFormat/.getCurrency formatter) str/lower-case keyword)]
    (core/money-of amount currency)))
