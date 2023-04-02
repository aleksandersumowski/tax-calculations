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
       (dt/local-date "yyyyMMdd")
       (dt/format "yyyy-MM-dd")))

(defn- parse-dates [transactions]
  (map #(update % :date parse-date) transactions))

(defn parse-float [f]
  (Float/parseFloat f))

(defn- abs [n]
  (Math/abs n))

(defn parse-number [n]
  (-> n
      parse-float
      abs))

(defn- parse-numbers [transactions]
  (->> transactions
       (map #(update % :price parse-number))
       (map #(update % :value parse-number))
       (map #(update % :units parse-number))))

(defn- transaction-key [t]
  (str (:date t) (:etf-ticker t) (:price t)))

(defn- merge-transactions [transactions]
  (reduce
    (fn [m1 m2]
      (merge (select-keys m1 [:date :etf-ticker :price])
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
       ; parse-dates
       ; parse-numbers
       ; (sort-by transaction-key)
       ; (partition-by transaction-key)
       ; (map merge-transactions)
       #_(group-by :etf-ticker)
       #_(transform [MAP-VALS ALL] #(dissoc % :wrapper :etf-ticker))))
