(ns checksum1.core
  (:gen-class))

(defn has-char-frequency [word char-freq]
  (let [fx (frequencies (seq word))]
    (if (some #(= % char-freq) (vals fx)) 1 0)
  ))

(defn calc-checksum [words]
  (let [freq2 (map #(has-char-frequency % 2) words)
        freq3 (map #(has-char-frequency % 3) words)]
      (* (reduce + freq2)  (reduce + freq3))
    )
  )

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader (nth args 0))]
    (println (calc-checksum (line-seq rdr)))
    ))
