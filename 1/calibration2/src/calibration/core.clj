(ns calibration.core
  (:gen-class))

(defn find-duplicate-frequency-loop [raw-input]
  (let [input raw-input
        current-frequency 0
        start-idx 0
        known-frequencies {}]
    (loop [freq current-frequency 
           idx start-idx
           frequencies known-frequencies]
        (if (some? (get frequencies freq nil))
          [freq]
          (let [idxmod (mod idx (count input))
                element (nth input idxmod)]
           (recur (+ freq element) (inc idx) (assoc frequencies freq 1))))
    )
  ))

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader (nth args 0))]
    (let [input (map read-string (line-seq rdr))]
      (println (find-duplicate-frequency-loop input))
    )
  )
)
