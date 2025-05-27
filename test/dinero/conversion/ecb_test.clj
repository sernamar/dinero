;; Copyright (c) 2025 Sergio Navarro
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns dinero.conversion.ecb-test
  (:require [dinero.conversion.ecb :as sut]
            [clojure.test :as t])
  (:import [clojure.lang ExceptionInfo]
           [java.time LocalDate]))

(t/deftest current-rates-provider
  (with-redefs [sut/get-ecb-rates (fn [] {:date "2024-09-10", :rates {:gbp 0.80}})]
    (t/is (= 1 (sut/current-rates-provider :eur :eur (LocalDate/of 2024 9 10))))
    (t/is (= 0.8 (sut/current-rates-provider :eur :gbp (LocalDate/of 2024 9 10))))
    (t/is (= 1.25 (sut/current-rates-provider :gbp :eur (LocalDate/of 2024 9 10))))
    (t/is (thrown? ExceptionInfo (sut/current-rates-provider :jpy :gbp (LocalDate/of 2024 9 10))))
    (t/is (thrown? ExceptionInfo (sut/current-rates-provider :eur :gbp (LocalDate/of 2000 1 1))))
    (t/is (thrown? ExceptionInfo (sut/current-rates-provider :eur :jpy (LocalDate/of 2024 9 10))))))

(t/deftest historical-rates-provider
  (with-redefs [sut/get-ecb-hist90-rates (fn [] [{:date "2024-09-10", :rates {:gbp 0.80}}
                                                 {:date "2024-09-09", :rates {:gbp 0.90}}])]
    (t/is (= 1 (sut/historical-rates-provider :eur :eur (LocalDate/of 2024 9 10))))
    (t/is (= 0.8 (sut/historical-rates-provider :eur :gbp (LocalDate/of 2024 9 10))))
    (t/is (= 1.25 (sut/historical-rates-provider :gbp :eur (LocalDate/of 2024 9 10))))
    (t/is (= 0.9 (sut/historical-rates-provider :eur :gbp (LocalDate/of 2024 9 9))))
    (t/is (= (/ 1 0.9) (sut/historical-rates-provider :gbp :eur (LocalDate/of 2024 9 9))))
    (t/is (thrown? ExceptionInfo (sut/historical-rates-provider :jpy :gbp (LocalDate/of 2024 9 10))))
    (t/is (thrown? ExceptionInfo (sut/historical-rates-provider :eur :gbp (LocalDate/of 2000 1 1))))
    (t/is (thrown? ExceptionInfo (sut/historical-rates-provider :eur :jpy (LocalDate/of 2024 9 10))))))
