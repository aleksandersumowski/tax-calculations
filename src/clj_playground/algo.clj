(ns clj-playground.algo
  (:require [com.rpl.specter :refer [transform FIRST]]
            [clojure.test :refer [deftest is]]))

(defn- process-fn* [remaining-sale unconsumed-purchases]
  (loop [remaining-sale remaining-sale consumed-purchase [] unconsumed-purchases unconsumed-purchases current-purchase (first unconsumed-purchases)]
    (cond
      (=  remaining-sale (:units current-purchase))  {:consumed-purchases (conj (vec consumed-purchase) current-purchase)
                                                      :unconsumed-purchases (rest unconsumed-purchases)}
      (< remaining-sale (:units current-purchase))  {:consumed-purchases (conj (vec consumed-purchase)
                                                                              (-> current-purchase
                                                                                (assoc :units remaining-sale)
                                                                                (assoc :value (* remaining-sale (:price current-purchase)))))
                                                     :unconsumed-purchases (transform [FIRST :units] #(- % remaining-sale) unconsumed-purchases)}
      :else                                         (recur (- remaining-sale (:units current-purchase))
                                                           (conj consumed-purchase current-purchase)
                                                           (rest unconsumed-purchases)
                                                           (first (rest unconsumed-purchases))))))

(defn- process-fn [{:keys [unconsumed-purchases sell-allocations]} sale]
  (let [finall-acc          (process-fn* (:units sale) unconsumed-purchases)
        ->sell-allocations  (fn [consumed-purchases]    (map (fn [purchase] 
                                                               {:purchase-date (:date purchase)
                                                                :purchase-value (:value purchase)
                                                                :units (:units purchase)
                                                                :purchase-price (:price purchase)
                                                                :sale-date (:date sale)
                                                                ;; taking :value from sale
                                                                ;; assumes always complete sale from a single purchase
                                                                :sale-value (* (:price sale)
                                                                               (:units purchase))
                                                                :sale-price (:price sale)})

                                                             consumed-purchases))]
    {:unconsumed-purchases (:unconsumed-purchases finall-acc)
     :sell-allocations (conj (vec sell-allocations) (vec (-> finall-acc
                                                               :consumed-purchases
                                                               ->sell-allocations)))}))


(defn process [{:keys [buy sell]}]
  (-> (reduce process-fn {:unconsumed-purchases buy
                          :sell-allocations []} sell)
      :sell-allocations))


(comment
  (process-fn* 6 [{:date "2018-02-20",
     :price 100
     :value 500
     :units 5}
    {:date "2018-04-03"
     :price 150
     :units 3
     :value 450}]))

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
      {:date "2020-10-08",
       :price 300
       :value 900
       :units 3}]})


(deftest simple-purchase
  (is (= (process transaction-history)
         [{:purchase-date "2018-02-20",
           :purchase-value 400
           :sale-date "2020-10-08"
           :sale-value 800}])))

(deftest process-fn*-test
  (is (= (process-fn* {:unconsumed-sale-units 5
                       :sell-allocations []}
                      (-> transaction-history :sell first)))))

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

