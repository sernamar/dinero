(ns dinero.core
  (:require [dinero.currency :as currency]
            [dinero.utils :as utils])
  (:import [java.math RoundingMode]))

(set! *warn-on-reflection* true)

;;; Configuration

(defonce config (utils/read-config "config.edn"))

(def ^:dynamic *default-currency* (:default-currency config))
(def ^:dynamic *default-rounding-mode* (:default-rounding-mode config))

;;; Monetary amounts

(defrecord Money [amount currency]
  Object
  (toString [_this]
    (str "Money{amount=" amount ", currency=" currency )))

(defrecord RoundedMoney [amount currency scale rounding-mode]
  Object
  (toString [_this]
    (str "RoundedMoney{amount=" amount ", currency=" currency ", scale=" scale ", rounding-mode=" rounding-mode)))

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  ([]
   (money-of 0 *default-currency*))
  ([amount]
   (money-of amount *default-currency*))
  ([amount currency]
   (currency/assert-currency currency)
   (Money. (bigdec amount) currency)))

(defn rounded-money-of
  "Creates a rounded monetary amount with the given amount, currency, scale, and rounding mode."
  ([amount]
   (rounded-money-of amount *default-currency*))
  ([amount currency]
   (rounded-money-of amount currency (currency/get-minor-units currency) *default-rounding-mode*))
  ([amount currency scale]
   (rounded-money-of amount currency scale *default-rounding-mode*))
  ([amount currency scale rounding-mode]
   (currency/assert-currency currency)
   (let [scale (or scale (currency/get-minor-units currency))
         rounding-mode (or rounding-mode *default-rounding-mode* :half-even)
         rounding-mode-object (utils/keyword->rounding-mode rounding-mode)]
     (when (neg? scale)
       (throw (ex-info "Scale must be non-negative" {:scale scale})))
     (RoundedMoney. (BigDecimal/.setScale ^BigDecimal (bigdec amount) ^int scale ^RoundingMode rounding-mode-object)
                    currency
                    scale
                    rounding-mode))))

(defn money?
  "Returns true if the given value is a monetary amount of type `Money`."
  [money]
  (instance? Money money))

(defn rounded-money?
  "Returns true if the given value is a monetary amount of type `RoundedMoney`."
  [money]
  (instance? RoundedMoney money))

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (:amount money))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))

(defn get-scale
  "Returns the scale of the given rounded monetary amount."
  [money]
  (:scale money))

(defn get-rounding-mode
  "Returns the rounding mode of the given rounded monetary amount."
  [money]
  (:rounding-mode money))

(defmacro with-currency
  "Evaluates the body with the given currency as the default currency."
  [currency & body]
  `(binding [*default-currency* ~currency]
     (let [rest# (do ~@body)]
       rest#)))
