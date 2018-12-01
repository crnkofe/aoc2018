(ns calibration.core
  (:gen-class))

(defn calculate-frequency [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce + (map read-string (line-seq rdr)))
  )
)

(defn -main
  [& args]
  (println (calculate-frequency (nth args 0)))
)
