(ns sleigh1.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:gen-class))

(defn parse-graph [lines]
  (let [raw-split (map #(str/split % #" ") lines)
        points (map #(vector (nth % 1) (nth % 7)) raw-split)]
    points
    )
  )

(defn find-root [graph]
  (let [non-root (into #{} (map #(last %) graph))
        all-nodes (into #{} (map #(first %) graph))]
      (set/difference all-nodes non-root)
    )
  )


(defn is-node-fulfilled [graph node visited]
  (let [prerequisite-links (filter #(= (last %) node) graph)
        prerequisites (into #{} (map #(first %) prerequisite-links))] 
    (set/subset? prerequisites visited)))

(defn fulfilled-prerequisite [graph neighbours visited]
  (filter #(is-node-fulfilled graph % visited) neighbours))

(defn traverse [graph candidates all-candidates visited]
  (let [node (first candidates)
        neighbour-links (filter #(= node (first %)) graph)
        neighbours (sort (map #(last %) neighbour-links))
        remaining-candidates (into #{} (rest candidates))
        all-but (set/difference all-candidates #{node})
        allowed-neighbours (fulfilled-prerequisite graph (set/union all-but neighbours) (conj visited node))
        new-all-candidates (set/difference (set/union all-but (into #{} neighbours)) (conj visited node))]
    (if (empty? allowed-neighbours)
      (vector node)
      (conj (vector node) (traverse graph (sort allowed-neighbours) new-all-candidates (conj visited node))))
      )
    )

(defn -main
  [& args]
    (with-open [rdr (clojure.java.io/reader (nth args 0))]
      (let [graph (parse-graph (line-seq rdr))
            roots (find-root graph)]
        (println (str/join "" (flatten (traverse graph (sort roots) roots #{}))))
        )
     )
  )
