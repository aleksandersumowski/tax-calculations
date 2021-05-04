(ns clj-playground.preprocess
  (:require [clojure.string :as str]
            [java-time :as dt]
            [com.rpl.specter :refer [transform ALL MAP-VALS]]))

(defn- csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map str/lower-case)
            (map #(str/replace % " " "-"))
            (map #(str/replace % "#" "number"))
            (map #(str/replace % "(s)" ""))
            (map keyword) ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))

(defn- parse-date [d]
  (->> d
       (dt/local-date "M/d/yyyy")
       (dt/format "yyyy-MM-dd")))

(defn- parse-dates [transactions]
  (map #(update % :date parse-date) transactions))

(defn parse-float [f]
  (Float/parseFloat f))

(defn- abs [n]
  (Math/abs n))

(defn- parse-numbers [transactions]
  (->> transactions
       (map #(update % :price parse-float))
       (map #(update % :value parse-float))
       (map #(update % :units (comp abs
                                  parse-float)))))

(defn- transaction-key [t]
  (str (:date t) (:asset-code t) (:price t)))

(defn- merge-transactions [transactions]
  (reduce
    (fn [m1 m2]
      (merge (select-keys m1 [:date :asset-code :price])
             {:value (+
                      (:value m1)
                      (:value m2))
              :units (+
                      (:units m1)
                      (:units m2))}))
    transactions))

(defn preprocess [csv-data]
  (->> csv-data
       csv-data->maps 
       parse-dates
       parse-numbers
       (sort-by transaction-key)
       (partition-by transaction-key)
       (map merge-transactions)
       (group-by :asset-code)
       (transform [MAP-VALS ALL] #(dissoc % :wrapper :asset-code))))
