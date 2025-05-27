;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.rounding-test
  (:require [dinero.core :as core]
            [dinero.rounding :as sut]
            [clojure.test :as t]))

(t/deftest round
  (t/testing "Money"
    (let [money(core/money-of 1234.5678 :eur)]
      (t/is (= (core/money-of 1234.57 :eur) (sut/round money)))
      (t/is (= (core/money-of 1234.57 :eur) (sut/round money 2 :up)))
      (t/is (= (core/money-of 1234.56 :eur) (sut/round money 2 :down)))
      (t/is (= (core/money-of 1235 :eur) (sut/round money 0 :up)))
      (t/is (= (core/money-of 1234 :eur) (sut/round money 0 :down)))))
  (t/testing "Rounded Money"
    (let [money(core/rounded-money-of 1234.5678 :eur)]
      (t/is (= (core/rounded-money-of 1234.57 :eur) (sut/round money)))
      (t/is (= (core/rounded-money-of 1234.57 :eur 2 :up) (sut/round money 2 :up)))
      (t/is (= (core/rounded-money-of 1234.57 :eur 2 :down) (sut/round money 2 :down)))
      (t/is (= (core/rounded-money-of 1235 :eur 0 :up) (sut/round money 0 :up)))
      (t/is (= (core/rounded-money-of 1234 :eur 0 :down) (sut/round money 0 :down)))))
  (t/testing "Fast Money"
    (let[money (core/fast-money-of 1234.5678 :eur)]
      (t/is (= (core/fast-money-of 1234.57 :eur) (sut/round money)))
      (t/is (= (core/fast-money-of 1234.57 :eur) (sut/round money 2 :up)))
      (t/is (= (core/fast-money-of 1234.56 :eur) (sut/round money 2 :down)))
      (t/is (= (core/fast-money-of 1235 :eur) (sut/round money 0 :up)))
      (t/is (= (core/fast-money-of 1234 :eur) (sut/round money 0 :down)))))
  (t/testing "Currency with no minor units (`nil`)"
    (let [money (core/money-of 1.23 :xau)
          rounded-money (core/rounded-money-of 1.23 :xau)
          fast-money (core/fast-money-of 1.23 :xau)]
      (t/is (= 1.23M (core/get-amount (sut/round money))))
      (t/is (= 1.23M (core/get-amount (sut/round rounded-money))))
      (t/is (= 1.23M (core/get-amount (sut/round fast-money)))))))

(t/deftest round-chf
  (t/testing "Money"
    (let [m1 (core/money-of 0.975 :chf)
          m2 (core/money-of 1.024 :chf)
          m3 (core/money-of 1.025 :chf)
          m4 (core/money-of 1.074 :chf)
          m5 (core/money-of 1.075 :chf)
          m6 (core/money-of 1.124 :chf)]
      (t/is (= (core/money-of 1 :chf) (sut/round m1 sut/chf-rounding-fn)))
      (t/is (= (core/money-of 1 :chf) (sut/round m2 sut/chf-rounding-fn)))
      (t/is (= (core/money-of 1.05 :chf) (sut/round m3 sut/chf-rounding-fn)))
      (t/is (= (core/money-of 1.05 :chf) (sut/round m4 sut/chf-rounding-fn)))
      (t/is (= (core/money-of 1.10 :chf) (sut/round m5 sut/chf-rounding-fn)))
      (t/is (= (core/money-of 1.10 :chf) (sut/round m6 sut/chf-rounding-fn)))))
  (t/testing "Rounded Money"
    (let [m1 (core/rounded-money-of 0.975 :chf)
          m2 (core/rounded-money-of 1.024 :chf)
          m3 (core/rounded-money-of 1.025 :chf)
          m4 (core/rounded-money-of 1.074 :chf)
          m5 (core/rounded-money-of 1.075 :chf)
          m6 (core/rounded-money-of 1.124 :chf)]
      (t/is (= (core/rounded-money-of 1 :chf 2 :half-up) (sut/round m1 sut/chf-rounding-fn)))
      (t/is (= (core/rounded-money-of 1 :chf 2 :half-up) (sut/round m2 sut/chf-rounding-fn)))
      (t/is (= (core/rounded-money-of 1 :chf 2 :half-up) (sut/round m3 sut/chf-rounding-fn)))
      (t/is (= (core/rounded-money-of 1.05 :chf 2 :half-up) (sut/round m4 sut/chf-rounding-fn)))
      (t/is (= (core/rounded-money-of 1.10 :chf 2 :half-up) (sut/round m5 sut/chf-rounding-fn)))
      (t/is (= (core/rounded-money-of 1.10 :chf 2 :half-up) (sut/round m6 sut/chf-rounding-fn)))))
  (t/testing
      "Fast Money"
    (let [m1 (core/fast-money-of 0.975 :chf)
          m2 (core/fast-money-of 1.024 :chf)
          m3 (core/fast-money-of 1.025 :chf)
          m4 (core/fast-money-of 1.074 :chf)
          m5 (core/fast-money-of 1.075 :chf)
          m6 (core/fast-money-of 1.124 :chf)]
      (t/is (= (core/fast-money-of 1 :chf) (sut/round m1 sut/chf-rounding-fn)))
      (t/is (= (core/fast-money-of 1 :chf) (sut/round m2 sut/chf-rounding-fn)))
      (t/is (= (core/fast-money-of 1.05 :chf) (sut/round m3 sut/chf-rounding-fn)))
      (t/is (= (core/fast-money-of 1.05 :chf) (sut/round m4 sut/chf-rounding-fn)))
      (t/is (= (core/fast-money-of 1.10 :chf) (sut/round m5 sut/chf-rounding-fn)))
      (t/is (= (core/fast-money-of 1.10 :chf) (sut/round m6 sut/chf-rounding-fn))))))
