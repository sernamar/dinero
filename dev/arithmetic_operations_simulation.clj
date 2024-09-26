(ns dev.arithmetic-operations-simulation
  (:require [dinero.core :as core]))

(defn money-simulation
  [money]
  (-> money
      (core/add (core/money-of 1234567.3444 :eur))
      (core/subtract (core/money-of 232323 :eur))
      (core/multiply 3.4)
      (core/divide 5.456)))

(time
 (reduce (fn [acc _] (money-simulation acc)) (core/money-of 0 :eur) (range 1000000)))
;; => "Elapsed time: 3795.038377 msecs"
;; => {:amount 1657407.9625291828793774319066147859922178988326848249027237354085603112840466926070038910505836575875486381322957198443579766536964980544747081712062256809338521400778210116731517509727626459143968871595330739299610894941634241245136186770428015564202334630350194M, :currency :eur}

(defn rounded-money-simulation
  [money]
  (-> money
      (core/add (core/rounded-money-of 1234567.3444 :eur 2 :half-even))
      (core/subtract (core/rounded-money-of 232323 :eur 2 :half-even))
      (core/multiply 3.4)
      (core/divide 5.456)))

(time
 (reduce (fn [acc _] (rounded-money-simulation acc)) (core/rounded-money-of 0 :eur 2 :half-even) (range 1000000)))
;; => "Elapsed time: 7031.994195 msecs"
;; => {:amount 1657407.95M, :currency :eur, :scale 2, :rounding-mode :half-even}
