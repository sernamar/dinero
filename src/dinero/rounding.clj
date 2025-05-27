;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.rounding
  (:require [dinero.core :as core]
            [dinero.currency :as currency]
            [dinero.utils :as utils])
  (:import [java.math BigDecimal RoundingMode]))

(set! *warn-on-reflection* true)

(defn round
  "Rounds the given monetary amount"
  ([money]
   (let [minor-units (currency/get-minor-units (core/get-currency money))
         decimal-places (or minor-units (BigDecimal/.scale (core/get-amount money)))
         rounding-mode (or core/*default-rounding-mode* :half-even)]
     (round money decimal-places rounding-mode)))
  ([money rounding-fn]
   (rounding-fn money))
  ([money decimal-places rounding-mode]
   (let [amount (core/get-amount money)
         currency (core/get-currency money)
         rounding-mode-object (utils/keyword->rounding-mode rounding-mode)
         rounded (BigDecimal/.setScale ^BigDecimal amount ^int decimal-places ^RoundingMode rounding-mode-object)]
     (cond
       (core/money? money) (core/money-of rounded currency)
       (core/fast-money? money) (core/fast-money-of rounded currency)
       :else (core/rounded-money-of rounded currency decimal-places rounding-mode)))))

(defn chf-rounding-fn
  "Creates a rounding function for Swiss Francs."
  [money]
  (let [amount (core/get-amount money)
        currency (core/get-currency money)
        scale 0
        rounded (BigDecimal/.multiply
                 (BigDecimal/.divide ^BigDecimal amount (bigdec 0.05) scale RoundingMode/HALF_UP)
                 (bigdec 0.05))]
    (cond
      (core/money? money) (core/money-of rounded currency)
      (core/fast-money? money) (core/fast-money-of rounded currency)
      :else (core/rounded-money-of rounded currency 2 :half-up))))
