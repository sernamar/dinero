(ns dinero.core
  (:require [dinero.currency :as currency]
            [dinero.utils :as utils]
            [clojure.pprint :as pp])
  (:import [java.math RoundingMode]))

(set! *warn-on-reflection* true)

;;; Configuration

(defonce config (utils/read-config "config.edn"))

(def ^:dynamic *default-currency* (:default-currency config))
(def ^:dynamic *default-rounding-mode* (:default-rounding-mode config))

(def fast-money-max-scale 5)

;;; Monetary amounts

(defrecord Money [^BigDecimal amount currency])

(defrecord RoundedMoney [^BigDecimal amount currency scale rounding-mode])

(defrecord FastMoney [^long amount currency scale])

(defn to-fast-money-long
  "Converts the given amount to the `long`-based internal representation of `FastMoney`."
  [amount]
  (let [big-decimal-amount (bigdec amount)
        scale (BigDecimal/.scale big-decimal-amount)]
    (when (> scale fast-money-max-scale)
      (throw (ex-info "Scale exceeds the maximum allowed value" {:scale scale})))
    (when (< big-decimal-amount (bigdec Long/MIN_VALUE))
      (throw (ex-info "Amount is less than the minimum allowed value" {:amount amount :min-value Long/MIN_VALUE})))
    (when (> big-decimal-amount (bigdec Long/MAX_VALUE))
      (throw (ex-info "Amount exceeds the maximum allowed value" {:amount amount :max-value Long/MAX_VALUE})))
    (try
      (-> big-decimal-amount
          (BigDecimal/.movePointRight fast-money-max-scale)
          BigDecimal/.longValueExact)
      (catch ArithmeticException e
        (throw (ex-info "Amount exceeds precision of `FastMoney` (`long`-based). Consider using `Money` (`BigDecimal`-based) instead."
                        {:amount amount
                         :error (ex-message e)}))))))

(defn- from-fast-money
  "Converts the `long`-based internal representation of `FastMoney` to a `BigDecimal`."
  [amount scale]
  (BigDecimal/.stripTrailingZeros (BigDecimal/valueOf amount scale)))

(defmethod pp/simple-dispatch FastMoney [money]
  (let [{:keys [amount currency scale]} money
        amount (from-fast-money amount scale)]
    (print {:amount amount :currency currency})))

(defn money-of
  "Creates a monetary amount with the given amount and currency."
  ([]
   (money-of 0 *default-currency*))
  ([amount]
   (money-of amount *default-currency*))
  ([amount currency]
   (let [currency (utils/to-lowercase-keyword currency)]
     (currency/assert-currency currency)
     (Money. (bigdec amount) currency))))

(defn rounded-money-of
  "Creates a rounded monetary amount with the given amount, currency, scale, and rounding mode."
  ([]
   (rounded-money-of 0 *default-currency*))
  ([amount]
   (rounded-money-of amount *default-currency*))
  ([amount currency]
   (let [minor-units (currency/get-minor-units currency)
         scale (or minor-units (BigDecimal/.scale (bigdec amount)))]
     (rounded-money-of amount currency scale *default-rounding-mode*)))
  ([amount currency scale]
   (rounded-money-of amount currency scale *default-rounding-mode*))
  ([amount currency scale rounding-mode]
   (let [currency (utils/to-lowercase-keyword currency)
         scale (or scale (currency/get-minor-units currency))
         rounding-mode (or rounding-mode *default-rounding-mode* :half-even)
         rounding-mode-object (utils/keyword->rounding-mode rounding-mode)]
     (when (neg? scale)
       (throw (ex-info "Scale must be non-negative" {:scale scale})))
     (currency/assert-currency currency)
     (RoundedMoney. (BigDecimal/.stripTrailingZeros (BigDecimal/.setScale ^BigDecimal (bigdec amount) ^int scale ^RoundingMode rounding-mode-object))
                    currency
                    scale
                    rounding-mode))))

(defn fast-money-of
  "Creates a fast monetary amount with the given amount and currency."
  ([]
   (fast-money-of 0 *default-currency*))
  ([amount]
   (fast-money-of amount *default-currency*))
  ([amount currency]
   (let [currency (utils/to-lowercase-keyword currency)
         internal-amount (to-fast-money-long amount)]
     (currency/assert-currency currency)
     (FastMoney. internal-amount currency fast-money-max-scale))))

(defn money?
  "Returns true if the given value is a monetary amount of type `Money`."
  [money]
  (instance? Money money))

(defn rounded-money?
  "Returns true if the given value is a monetary amount of type `RoundedMoney`."
  [money]
  (instance? RoundedMoney money))

(defn fast-money?
  "Returns true if the given value is a monetary amount of type `FastMoney`."
  [money]
  (instance? FastMoney money))

(defn get-amount
  "Returns the amount of the given monetary amount."
  [money]
  (if (fast-money? money)
    (let [{:keys [amount scale]} money]
      (from-fast-money amount scale))
    (:amount money)))

(defn get-currency
  "Returns the currency of the given monetary amount."
  [money]
  (:currency money))

(defn get-scale
  "Returns the scale of the given monetary amount."
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
