(ns dinero.rounding-test
  (:require [dinero.core :as core]
            [dinero.rounding :as sut]
            [clojure.test :as t]))

(t/deftest round
  ;; money
  (let [money(core/money-of 1234.5678 :eur)]
    (t/is (= (core/money-of 1234.57 :eur) (sut/round money)))
    (t/is (= (core/money-of 1234.57 :eur) (sut/round money 2 :up)))
    (t/is (= (core/money-of 1234.56 :eur) (sut/round money 2 :down)))
    (t/is (= (core/money-of 1235 :eur) (sut/round money 0 :up)))
    (t/is (= (core/money-of 1234 :eur) (sut/round money 0 :down))))
  ;; rounded money
  (let [money(core/rounded-money-of 1234.5678 :eur)]
    (t/is (= (core/rounded-money-of 1234.57 :eur) (sut/round money)))
    (t/is (= (core/rounded-money-of 1234.57 :eur 2 :up) (sut/round money 2 :up)))
    (t/is (= (core/rounded-money-of 1234.57 :eur 2 :down) (sut/round money 2 :down)))
    (t/is (= (core/rounded-money-of 1235 :eur 0 :up) (sut/round money 0 :up)))
    (t/is (= (core/rounded-money-of 1234 :eur 0 :down) (sut/round money 0 :down)))))

(t/deftest round-chf
  ;; money
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
    (t/is (= (core/money-of 1.10 :chf) (sut/round m6 sut/chf-rounding-fn))))
  ;; rounded money
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
