(ns clj-playground.core
  (:require [com.rpl.specter :refer [transform MAP-VALS] :as s]
            [java-time :as dt])
  (:require [clj-playground.io :as io]
            [clj-playground.preprocess :as prep]
            [clj-playground.algo :as algo]))

(def sell
  (->  "./data/transactions/sell.csv"
      io/read-data
      prep/preprocess))

(def buy
  (-> "./data/transactions/buy.csv"
      io/read-data
      prep/preprocess))

(def exchange-rates
; {"2019-07-08" 4.7446,
;  "2016-04-13" 5.3654,
;  "2016-11-09" 4.8727,
;  "2019-05-16" 4.9112,
;  "2017-08-01" 4.7622,
;  "2018-05-18" 4.9117,
;  "2020-02-06" 5.0177,
;  "2020-01-21" 4.9919,
;  ...}
(->> "./data/exchange-rates/2016-2020.csv"
     io/read-data
     flatten 
     (apply hash-map)
     (transform [MAP-VALS] prep/parse-float)))

(def transactions
  (apply merge
         (for [key (keys sell)]
           {key {:buy (get buy key)
                 :sell (get sell key)}})))


(def data 
 {"B9MRHC2"
 '({:purchase-date "2018-10-18",
   :purchase-value 334.2356675304691,
   :units 11.178600311279297,
   :purchase-price 29.8996,
   :sale-date "2020-07-09",
   :sale-value 355.47999572753906,
   :sale-price 31.8})})

(defn flat-merge [[k v]]
  (map #(merge {:asset-code k} %) v))

(defn shift-day-back [date-string]
  (as-> date-string %
       (dt/local-date "yyyy-MM-dd" %)
       (dt/minus % (dt/days 1))
       (dt/format "yyyy-MM-dd" %)))

(defn lookup-exchange-rate [initial-date]
  "looks for previous working day"
  (loop [date initial-date]
    (if-let [result (get exchange-rates (shift-day-back date))]
      result
      (recur (shift-day-back date)))))

(defn lookup-exchange-rates [m]
  (let [purchase-exchange-rate (lookup-exchange-rate (:purchase-date m))
        sale-exchange-rate (lookup-exchange-rate (:sale-date m))
        purchase-value-pln (* purchase-exchange-rate (:purchase-value m))
        sale-value-pln (* sale-exchange-rate (:sale-value m))]
    (merge m
           {:purchase-exchange-rate purchase-exchange-rate
            :purchase-value-pln purchase-value-pln
            :sale-exchange-rate sale-exchange-rate
            :sale-value-pln sale-value-pln
            :profit-pln (-  sale-value-pln purchase-value-pln)})))


(def headers
  [:asset-code
   :purchase-date :sale-date :units
   :purchase-price :purchase-exchange-rate :purchase-value-pln
   :sale-price     :sale-exchange-rate     :sale-value-pln
   :profit-pln])

(def result
  (->> transactions
     (transform [MAP-VALS] (comp flatten
                            algo/process))
     (map flat-merge)
     flatten
     (map lookup-exchange-rates)))

(defn- result-key [t]
  (str (:date t) (:asset-code t)))

(def postprocessed
  (as-> result %
       (sort-by result-key %)
       (map (apply juxt headers) %)
       (conj % headers)))

(io/write-output "./data/output.csv" postprocessed)

(comment
  (def asset-code "BDFCGG9")

  (get transactions asset-code)

  (get result asset-code)
  )
