(ns guard1.core
  (:gen-class)
  (:require [clojure.pprint :as pprint])
  (:require [clojure.string :as str]))


(defn assoc-range [data ranges]
  (loop [current-ranges ranges
         current-data data]
    (if (empty? current-ranges)
      current-data
      (recur (rest current-ranges) 
             (assoc current-data (first current-ranges) (+ 1 (get current-data (first current-ranges) 0)))))))

(defn most-frequent-min [wakey-data]
  (loop [ranges (partition 2 wakey-data)
         frequency-map {}]
    (if (empty? ranges)
      frequency-map
      (let [first-range (first ranges)
            lst (range (Integer. (nth first-range 0)) (Integer. (nth first-range 1)))]
      (recur (rest ranges) (assoc-range frequency-map lst))))))

(defn guard-max-powernap [wakey-data]
  (apply max (map #(nth % 1) (partition 2 wakey-data))))
  ; (apply max (map #(- (nth % 1) (nth % 0)) (partition 2 wakey-data))))

(defn guard-sleep-time [wakey-data]
  (reduce + (map #(- (nth % 1) (nth % 0)) (partition 2 wakey-data)))
  )

(defn guards-sleep-time [guard-data]
  (reduce-kv #(assoc %1 %2 (guard-sleep-time %3)) {} guard-data)
  )

(defn parse [data]
  (loop [current-data data
         guard nil
         guard-map {}]
    (if (empty? current-data)
      guard-map
      (let [line (first current-data)
            command (subs (nth (str/split line #"]") 1) 1)
            parsed-date (subs (first (str/split line #"]")) 1)
            parsed-time (nth (str/split parsed-date #" ") 1)
            parsed-minutes (Integer. (nth (str/split parsed-time #":") 1))]
        (if (str/starts-with? command "Guard")
          (let [guard-num (subs (nth (str/split command #" ") 1) 1)]
            (recur (rest current-data) guard-num (assoc guard-map guard-num (get guard-map guard-num [])))
          )
          (recur (rest current-data) guard (assoc guard-map guard (conj (get guard-map guard) parsed-minutes)))
      )))))


(defn date-compare [line1 line2]
  (let [parsed-date1 (subs (first (str/split line1 #"]")) 1)
        parsed-date2 (subs (first (str/split line2 #"]")) 1)]
        (compare parsed-date1 parsed-date2)
        ))

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader (nth args 0))]
    (let [sorted-guard (sort #(date-compare %1 %2) (line-seq rdr))
          parsed-guard (parse sorted-guard) 
          guard-sleep-frequency (reduce-kv #(assoc %1 %2 (most-frequent-min %3)) {} parsed-guard)
          guard-max-frequency (reduce-kv #(assoc %1 %2 (apply max (vals %3)))
                          {} (into {} (filter #(not= 0 (count (nth % 1))) guard-sleep-frequency)))
          guard-slept-most-at-once (last (sort-by #(nth % 1) guard-max-frequency))
          minute-of (filter #(= (nth guard-slept-most-at-once 1) (nth % 1)) (get guard-sleep-frequency (first guard-slept-most-at-once) ))
          ]
      (println (* (Integer. (first guard-slept-most-at-once)) (first (first minute-of))))
  )))

