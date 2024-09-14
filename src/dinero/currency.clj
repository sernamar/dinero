(ns dinero.currency
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [dinero.utils :as utils])
  (:import [java.util Currency Locale]))

(defonce currencies (-> "currencies.edn" io/resource slurp edn/read-string))

(defn get-currency-code
  "Returns the currency code of the given currency."
  [currency]
  (get-in currencies [currency :currency-code]))

(defn- get-type
  "Returns the type of the given currency."
  [currency]
  (get-in currencies [currency :type]))

(defn get-minor-units
  "Returns the minor units of the given currency."
  [currency]
  (get-in currencies [currency :minor-units]))

(defn iso-4217?
  "Returns true if the given currency is an ISO 4217 currency."
  [currency]
  (= :iso-4217 (get-type currency)))

(defn get-symbol
  "Returns the currency symbol of the given currency."
  ([currency]
   (get-symbol currency (Locale/getDefault)))
  ([currency locale]
   (if (iso-4217? currency)
     (-> currency utils/to-uppercase-string Currency/getInstance (Currency/.getSymbol locale))
     (get-in currencies [currency :symbol]))))

(defn assert-currency
  "Asserts that the given currency is a valid currency."
  [currency]
  (when-not currency
    (throw (ex-info "No currency provided" {})))
  (when-not (contains? currencies currency)
    (throw (ex-info "Unknown currency" {:currency currency}))))

(defn get-all-currencies
  "Returns all available currencies."
  []
  (keys currencies))
