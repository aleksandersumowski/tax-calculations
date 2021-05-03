(ns clj-playground.io
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn read-data [file-name]
  (with-open [reader (io/reader file-name)]
    (doall
      (csv/read-csv reader))))

