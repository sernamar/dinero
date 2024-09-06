(ns dinero.conversion-test
  (:require [dinero.conversion :as sut]
            [dinero.core :as core]
            [clojure.test :as t]))

(t/deftest convert-with-exchange-rate
  (let [money (core/money-of 1M :eur)
        term-currency :gbp
        exchange-rate 0.85
        converted (sut/convert-with-exchange-rate money term-currency exchange-rate)]
    (t/is (= 0.85M (core/get-amount converted)))
    (t/is (= :gbp (core/get-currency converted)))))
