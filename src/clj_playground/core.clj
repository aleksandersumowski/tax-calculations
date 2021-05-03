(ns clj-playground.core
  (:require [com.rpl.specter :refer [transform MAP-VALS]]
)
  (:require [clj-playground.io :as io]
            [clj-playground.preprocess :as prep]
            [clj-playground.algo :as algo]))

(def sell
  (->  "./data/sell.csv"
      io/read-data
      prep/preprocess))

(def buy
  (-> "./data/buy.csv"
      io/read-data
      prep/preprocess))

(def transactions
  (apply merge
         (for [key (keys sell)]
           {key {:buy (get buy key)
                 :sell (get sell key)}})))

(transform [MAP-VALS] (comp flatten
                            algo/process) transactions)


