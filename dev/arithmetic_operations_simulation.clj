(ns dev.arithmetic-operations-simulation
  (:require [criterium.core :as criterium]
            [dinero.core :as core]))

(def money-simulation
  (let [addend (core/money-of 1234567.3444 :eur)
        subtrahend (core/money-of 232323 :eur)]
    (fn [money]
      (-> money
          (core/add addend)
          (core/subtract subtrahend)
          (core/multiply 3.4)
          (core/divide 5.456)))))

(comment
  (criterium/bench
      (reduce (fn [acc _] (money-simulation acc)) (core/money-of 0 :eur) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 2,394855 sec
;;     Execution time std-deviation : 79,951502 ms
;;    Execution time lower quantile : 2,313923 sec ( 2,5%)
;;    Execution time upper quantile : 2,525123 sec (97,5%)
;;                    Overhead used : 6,546638 ns
;;
;; => {:amount 1657407.9625291828793774319066147859922178988326848249027237354085603112840466926070038910505836575875486381322957198443579766536964980544747081712062256809338521400778210116731517509727626459143968871595330739299610894941634241245136186770428015564202334630350194M, :currency :eur}

(def rounded-money-simulation
  (let [addend (core/rounded-money-of 1234567.3444 :eur 2 :half-even)
        subtrahend (core/rounded-money-of 232323 :eur 2 :half-even)]
    (fn [money]
      (-> money
          (core/add addend)
          (core/subtract subtrahend)
          (core/multiply 3.4)
          (core/divide 5.456)))))

(comment
 (criterium/bench
     (reduce (fn [acc _] (rounded-money-simulation acc)) (core/rounded-money-of 0 :eur 2 :half-even) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 3,843440 sec
;;     Execution time std-deviation : 6,455752 ms
;;    Execution time lower quantile : 3,832537 sec ( 2,5%)
;;    Execution time upper quantile : 3,854636 sec (97,5%)
;;                    Overhead used : 6,546638 ns
;; Found 1 outliers in 60 samples (1,6667 %)
;; 	low-severe	 1 (1,6667 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;;
;; => {:amount 1657407.95M, :currency :eur, :scale 2, :rounding-mode :half-even}

(def fast-money-simulation
  (let [addend (core/fast-money-of 1234567.3444 :eur)
        subtrahend (core/fast-money-of 232323 :eur)]
    (fn [money]
      (-> money
          (core/add addend)
          (core/subtract subtrahend)
          (core/multiply 3.4)
          (core/divide 5.456)))))

(comment
  (criterium/bench
      (reduce (fn [acc _] (fast-money-simulation acc)) (core/fast-money-of 0 :eur) (range 1000000))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 2,085946 sec
;;     Execution time std-deviation : 121,642628 ms
;;    Execution time lower quantile : 1,966039 sec ( 2,5%)
;;    Execution time upper quantile : 2,334722 sec (97,5%)
;;                    Overhead used : 6,546638 ns

;; Found 8 outliers in 60 samples (13,3333 %)
;; 	low-severe	 7 (11,6667 %)
;; 	low-mild	 1 (1,6667 %)
;;  Variance from outliers : 43,4493 % Variance is moderately inflated by outliers
;;
;; => {:amount 1657407.96252, :currency :eur}
