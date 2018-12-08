(ns safety1.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main
  [& args]
    (with-open [rdr (clojure.java.io/reader (nth args 0))]
      (let [points (into {} (map-indexed #(vector %1 (str/split %2 #", ")) (line-seq rdr)))]
        (println points)
        (println (get points 0))
       )
  ))
