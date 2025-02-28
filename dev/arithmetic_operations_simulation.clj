(ns dev.arithmetic-operations-simulation
  (:require [criterium.core :as criterium]
            [dinero.core :as core]))

(let [addend (core/money-of 1234567.3444 :eur)
      subtrahend (core/money-of 232323 :eur)]
  (defn money-simulation
    [money]
    (-> money
        (core/add addend)
        (core/subtract subtrahend)
        (core/multiply 3.4)
        (core/divide 5.456))))

(comment
  (criterium/bench
      (reduce (fn [acc _] (money-simulation acc)) (core/money-of 0 :eur) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 2,184454 sec
;;     Execution time std-deviation : 10,434630 ms
;;    Execution time lower quantile : 2,158174 sec ( 2,5%)
;;    Execution time upper quantile : 2,203948 sec (97,5%)
;;                    Overhead used : 6,970274 ns
;;
;; Found 9 outliers in 60 samples (15,0000 %)
;; 	low-severe	 3 (5,0000 %)
;; 	low-mild	 2 (3,3333 %)
;; 	high-mild	 3 (5,0000 %)
;; 	high-severe	 1 (1,6667 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers

;; => {:amount 1657407.9625291828793774319066147859922178988326848249027237354085603112840466926070038910505836575875486381322957198443579766536964980544747081712062256809338521400778210116731517509727626459143968871595330739299610894941634241245136186770428015564202334630350194M, :currency :eur}

(let [addend (core/rounded-money-of 1234567.3444 :eur 2 :half-even)
      subtrahend (core/rounded-money-of 232323 :eur 2 :half-even)]
  (defn rounded-money-simulation
    [money]
    (-> money
        (core/add addend)
        (core/subtract subtrahend)
        (core/multiply 3.4)
        (core/divide 5.456))))

(comment
 (criterium/bench
     (reduce (fn [acc _] (rounded-money-simulation acc)) (core/rounded-money-of 0 :eur 2 :half-even) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 3,222852 sec
;;     Execution time std-deviation : 10,467930 ms
;;    Execution time lower quantile : 3,214198 sec ( 2,5%)
;;    Execution time upper quantile : 3,232669 sec (97,5%)
;;                    Overhead used : 6,521613 ns

;; Found 2 outliers in 60 samples (3,3333 %)
;; 	low-severe	 1 (1,6667 %)
;; 	low-mild	 1 (1,6667 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers

;; => {:amount 1657407.95M, :currency :eur, :scale 2, :rounding-mode :half-even}

(let [addend (core/fast-money-of 1234567.3444 :eur)
      subtrahend (core/fast-money-of 232323 :eur)]
  (defn fast-money-simulation
    [money]
    (-> money
        (core/add addend)
        (core/subtract subtrahend)
        (core/multiply 3.4)
        (core/divide 5.456))))

(comment
  (criterium/bench
      (reduce (fn [acc _] (fast-money-simulation acc)) (core/fast-money-of 0 :eur) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 1,338023 sec
;;     Execution time std-deviation : 24,957735 ms
;;    Execution time lower quantile : 1,314668 sec ( 2,5%)
;;    Execution time upper quantile : 1,395095 sec (97,5%)
;;                    Overhead used : 6,970274 ns
;;
;; Found 7 outliers in 60 samples (11,6667 %)
;; 	low-severe	 5 (8,3333 %)
;; 	low-mild	 2 (3,3333 %)
;;  Variance from outliers : 7,8135 % Variance is slightly inflated by outliers

;; => {:amount 1657407.96252, :currency :eur}
