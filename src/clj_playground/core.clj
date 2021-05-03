(ns clj-playground.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [java-time :as dt]
            [clojure.data.csv :as csv]
            [com.rpl.specter :refer [transform select ALL MAP-VALS]]
            [clojure.test :refer [deftest is]]))


(defn read-data [file-name]
  (with-open [reader (io/reader file-name)]
    (doall
      (csv/read-csv reader))))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map str/lower-case)
            (map #(str/replace % " " "-"))
            (map #(str/replace % "#" "number"))
            (map #(str/replace % "(s)" ""))
            (map keyword) ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))

(defn first-key [m]
  (let [first-key (first (keys m))]
    (select-keys m [first-key])))

(defn parse-date [d]
  (->> d
       (dt/local-date "M/d/yyyy")
       (dt/format "yyyy-MM-dd")))

(defn parse-dates [transactions]
  (map #(update % :date parse-date) transactions))

(defn parse-float [f]
  (Float/parseFloat f))

(defn parse-numbers [transactions]
  (->> transactions
       (map #(update % :price parse-float))
       (map #(update % :value parse-float))
       (map #(-> %
                 (update :units parse-float)
                 Math/abs))))

(defn transaction-key [t]
  (str (:date t) (:asset-code t) (:price t)))

(defn merge-transactions [transactions]
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

(defn read-transactions [file]
  (->> (csv-data->maps (read-data file))
       parse-dates
       parse-numbers
       (sort-by transaction-key)
       (partition-by transaction-key)
       (map merge-transactions)
       (group-by :asset-code)
       (transform [MAP-VALS ALL] #(dissoc % :wrapper :asset-code))))

(def sell
  (read-transactions "sell.csv"))

(def buy
  (read-transactions "buy.csv"))

(def transactions
  (apply merge
         (for [key (keys sell)]
           {key {:buy (get buy key)
                 :sell (get sell key)}})))

(->> transactions
     (select [MAP-VALS :buy])
     (map count)
     (apply max))

(def transaction-history
  {:buy
   [{:date "2018-02-20",
     :price 100
     :value 500
     :units 5}
    {:date "2018-04-03"
     :price 150
     :units 3
     :value 450}]
   :sell
   [{:date "2020-10-08",
     :price 200
     :value 800
     :units 4}
    #_{:date "2020-10-08",
     :price 300
     :value 900
     :units 3}]})

(defn process-fn* [remaining-sale unconsumed-purchases]
  (loop [remaining-sale remaining-sale consumed-purchase [] unconsumed-purchases unconsumed-purchases current-purchase (first unconsumed-purchases)]
    (println remaining-sale consumed-purchase unconsumed-purchases current-purchase)
    (cond
      (=  remaining-sale (:units current-purchase))  {:consumed-purchases (conj (vec consumed-purchase) current-purchase)
                                                      :unconsumed-purchases (rest unconsumed-purchases)}
      (< remaining-sale (:units current-purchase))  {:consumed-purchase (conj (vec consumed-purchase)
                                                                              (assoc current-purchase :units remaining-sale))
                                                     :unconsumed-purchases (transform [first :units] #(- % remaining-sale) unconsumed-purchases)}
      :else                                         (recur (- remaining-sale (:units current-purchase))
                                                           (conj consumed-purchase current-purchase)
                                                           (rest unconsumed-purchases)
                                                           (first (rest unconsumed-purchases))))))

(process-fn* 3 [{:date "2018-02-20",
     :price 100
     :value 500
     :units 5}
    {:date "2018-04-03"
     :price 150
     :units 3
     :value 450}])
(conj [1 2 3]  2 22)


#_(deftest process-fn*-test
  (is (= (process-fn* {:unconsumed-sale-units 5
                       :sell-allocations []}
                      (-> transaction-history :sell first))
 )))

(defn process-fn [{:keys [unconsumed-purchases sell-allocations]} sale]
  (let [finall-acc (process-fn* sale unconsumed-purchases)])
      {:unconsumed-purchases (:unconsumed-purchases finall-acc)
       :sell-allocations (concat sell-allocations new-sell-allocations)})


(deftest process-fn-test
  (is (= (process-fn {:unconsumed-purchases (:buy transaction-history)
                      :sell-allocations []}
                     (-> transaction-history :sell first))
         {:unconsumed-purchases [{:date "2018-02-20",
                                  :price 100
                                  :value 100
                                  :units 1}
                                 {:date "2018-04-03", :price 111.556, :units 0.0019, :value 0.21}] 
          :sell-allocations [{:purchase-date "2018-02-20",
                              :purchase-value 400
                              :sale-date "2020-10-08"
                              :sale-value 800}]})))

(defn process [{:keys [buy sell]}]
  (-> (reduce process-fn {:unconsumed-purchases buy
                          :sell-allocations []} sell)
      :sell-allocations))

(deftest simple-purchase
  (is (= (process transaction-history)
         [{:purchase-date "2018-02-20",
           :purchase-value 400
           :sale-date "2020-10-08"
           :sale-value 800}])))
