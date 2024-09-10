(ns dinero.conversion.ecb
  (:require [clojure.string :as str]
            [clojure.xml :as xml])
  (:import [java.time LocalDate]))

(defn- currency-cube->map
  "Transforms a currency cube entry into a map."
  [cube]
  (let [currency (-> cube :attrs :currency str/lower-case keyword)
        rate (-> cube :attrs :rate parse-double)]
    {currency rate}))

(defn- date-cube->map
  "Transforms a date cube entry into a map."
  [cube]
  (let [time (-> cube :attrs :time)
        rates (->> cube :content
                   (filter (comp #{:Cube} :tag))
                   (map currency-cube->map)
                   (into (sorted-map)))]
    {:date time
     :rates rates}))

(defn- get-ecb-rates-from-url
  "Returns the ECB rates from the given XML data source."
  [url]
  (->> url
       xml/parse
       :content
       (filter (comp #{:Cube} :tag))
       first
       :content
       (map date-cube->map)))

;;; ECB current rates (updated daily at around 16:00 CET every working day, except on TARGET closing days)

(defn get-ecb-rates
  "Returns the ECB current rates."
  []
  (first (get-ecb-rates-from-url "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))

(defn current-rates-provider
  "Returns the exchange rate for the given currency pair from the ECB current rates."
  [from-currency to-currency & [query-date]]
  (if (= from-currency to-currency)
    1
    (let [{:keys [rates date]} (get-ecb-rates)
          base-currency :eur
          supported-currencies (keys rates)
          query-date (or query-date (LocalDate/now))
          query-date-str (str query-date)]
      (cond
        (not (contains? #{from-currency to-currency} base-currency))
        (throw (ex-info "The ECB provider only supports conversions from/to :eur"
                        {:requested-currencies [from-currency to-currency]
                         :provider :ecb}))

        (not= query-date-str date)
        (throw (ex-info (str "ECB exchange rates are only available for " date)
                        {:requested-date query-date-str
                         :provider :ecb}))

        (not-any? #{from-currency to-currency} supported-currencies)
        (throw (ex-info "ECB exchange rate not available for the requested currency pair"
                        {:requested-currencies [from-currency to-currency]
                         :provider :ecb}))

        :else
        (if (= from-currency base-currency)
          (rates to-currency)
          (/ 1 (rates from-currency)))))))

;;; ECB historical rates (last 90 days)

(defn get-ecb-hist90-rates
  "Returns the ECB historical rates."
  []
  (get-ecb-rates-from-url "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist-90d.xml"))

(defn historical-rates-provider
  "Returns the exchange rate for the given currency pair from the ECB historical rates."
  [from-currency to-currency & [query-date]]
  (if (= from-currency to-currency)
    1
    (let [rates-data (get-ecb-hist90-rates)
          base-currency :eur
          query-date (or query-date (LocalDate/now))
          query-date-str (str query-date)
          ecb-dates (map :date rates-data)
          supported-currencies (->> rates-data first :rates keys)]
      (cond
        (not (contains? #{from-currency to-currency} base-currency))
        (throw (ex-info "The ECB historical 90 days provider only supports conversions from/to :eur"
                        {:requested-currencies [from-currency to-currency]
                         :provider :ecb-hist90}))

        (not-any? #{query-date-str} ecb-dates)
        (throw (ex-info (str "ECB historical 90 days exchange rates are only available from "
                             (last ecb-dates)
                             " to "
                             (first ecb-dates))
                        {:requested-date query-date-str
                         :provider :ecb-hist90}))

        (not-any? #{from-currency to-currency} supported-currencies)
        (throw (ex-info "ECB historical 90 days exchange rate not available for the requested currency pair"
                        {:requested-currencies [from-currency to-currency]
                         :provider :ecb-hist90}))

        :else
        (let [rates (->> rates-data
                        (filter #(= query-date-str (:date %)))
                        first
                        :rates)]
          (if (= from-currency base-currency)
            (rates to-currency)
            (/ 1 (rates from-currency))))))))
