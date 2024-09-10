(ns dinero.conversion.coinbase-test
  (:require [dinero.conversion.coinbase :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]))

(t/deftest bitcoin-rate-provider
  (with-redefs [sut/get-exchange-rates (fn [] {:eur 55000})]
    (t/is (= 1 (sut/bitcoin-rate-provider :btc :btc)))
    (t/is (= 55000 (sut/bitcoin-rate-provider :btc :eur)))
    (t/is (= 1/55000 (sut/bitcoin-rate-provider :eur :btc)))
    (t/is (thrown? ExceptionInfo (sut/bitcoin-rate-provider :eur :gbp)))
    (t/is (thrown? ExceptionInfo (sut/bitcoin-rate-provider :btc :unknown-currency)))))
