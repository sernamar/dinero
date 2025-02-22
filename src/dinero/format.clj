(ns dinero.format
  (:refer-clojure :exclude [format])
  (:require [dinero.core :as core]
            [dinero.currency :as currency]
            [dinero.utils :as utils]
            [clojure.string :as str])
  (:import [java.text DecimalFormat DecimalFormatSymbols]
           [java.util Currency Locale]))

(set! *warn-on-reflection* true)

(defn- make-formatter
  "Creates a new formatter for the given currency, locale, rounding mode, and decimal places."
  [currency locale decimal-places rounding-mode]
  (let [formatter (DecimalFormat/getCurrencyInstance locale)]
    (doto formatter
      (DecimalFormat/.setRoundingMode rounding-mode)
      (DecimalFormat/.setMinimumFractionDigits decimal-places)
      (DecimalFormat/.setMaximumFractionDigits decimal-places))
    (when (currency/iso-4217? currency)
      (DecimalFormat/.setCurrency formatter (Currency/getInstance ^String (currency/get-currency-code currency))))
    formatter))

(defn- format-amount
  "Formats the given amount with the given formatter, locale, and symbol style."
  [amount currency formatter locale symbol-style]
  (let [formatted-money (DecimalFormat/.format ^DecimalFormat formatter amount)
        locale-currency (Currency/getInstance ^Locale locale)
        locale-code (Currency/.getCurrencyCode locale-currency)
        locale-symbol (Currency/.getSymbol locale-currency locale)
        currency-code (currency/get-currency-code currency)
        currency-symbol (or (currency/get-symbol currency) currency-code)] ; default to code if symbol is not available
    (cond
      ;; ISO 4217 currency with symbol-style :symbol
      (and (currency/iso-4217? currency) (= :symbol symbol-style))
      formatted-money
      ;; ISO 4217 currency with symbol-style :code
      (and (currency/iso-4217? currency) (= :code symbol-style))
      (str/replace formatted-money locale-symbol locale-code)
      ;; Non-ISO 4217 currency with symbol-style :symbol
      (and (not (currency/iso-4217? currency)) (= :symbol symbol-style))
      (str/replace formatted-money locale-symbol currency-symbol)
      ;; Non-ISO 4217 currency with symbol-style :code
      (and (not (currency/iso-4217? currency)) (= :code symbol-style))
      (str/replace formatted-money locale-symbol currency-code)
      ;; Invalid symbol style
      :else (throw (ex-info "Invalid symbol style" {:symbol-style symbol-style})))))

(defn format-money
  "Formats the given monetary amount with the given options."
  [money & {:keys [locale decimal-places rounding-mode symbol-style] :as _options}]
  (let [amount (core/get-amount money)
        currency (core/get-currency money)
        locale (or locale (Locale/getDefault))
        decimal-places (or decimal-places (currency/get-minor-units currency) (BigDecimal/.scale amount))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode core/*default-rounding-mode* :half-even))
        symbol-style (or symbol-style :symbol)
        formatter (make-formatter currency locale decimal-places rounding-mode)]
    (format-amount amount currency formatter locale symbol-style)))

(defn format-money-with-pattern
  "Formats the given monetary amount with the given pattern and options."
  [money pattern & {:keys [locale rounding-mode] :as _options}]
  (let [amount (core/get-amount money)
        locale (or locale (Locale/getDefault))
        rounding-mode (utils/keyword->rounding-mode (or rounding-mode core/*default-rounding-mode* :half-even))
        symbols (DecimalFormatSymbols/getInstance locale)
        formatter (DecimalFormat. pattern symbols)]
    (DecimalFormat/.setRoundingMode formatter rounding-mode)
    (DecimalFormat/.format ^DecimalFormat formatter amount)))
