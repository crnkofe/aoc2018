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


(defn assign-free-workers [candidates workers timetable]
  (let [lcandidates (into [] candidates)
        lworkers (into [] workers)
        free-workers (set/difference (into #{} workers) (into #{} (keys timetable)))
        limit (min (count candidates) (count free-workers))]
    (loop [i 0
           current-timetable timetable]
      (if (>= i limit)
        current-timetable
        (recur (inc i) (assoc current-timetable (nth lworkers i) [(nth lcandidates i) 0]))
        )
      )
    )
  )

(defn up-time [nodetime]
  (assoc nodetime 1 (inc (nth nodetime 1)))
  )

(defn task-difficulty [node]
  (+ 60 (inc (- (int (nth node 0)) (int \A))))
  )

(defn find-free-workers [timetable all-workers]
  (let [entries (filter #(< (nth (second %) 1) (task-difficulty (nth (second %) 0))) timetable)]
    (if (empty? entries)
      []
      (into [] (set/difference (into #{} all-workers) (into #{} (keys entries)))))
    )
  )

(defn done-workers [timetable]
  (let [done-map (filter #(<= (task-difficulty (nth (second %) 0)) (nth (second %) 1)) timetable)]
    (map #(key %) done-map)
    )
  )

(defn done-tasks [timetable]
  (let [done-map (filter #(<= (task-difficulty (nth (second %) 0)) (nth (second %) 1)) timetable)]
    (map #(first (val %)) done-map)
    )
  )

(defn filter-done-timetable [timetable]
  (into {} (filter #(> (task-difficulty (nth (second %) 0)) (nth (second %) 1)) timetable))
  )

(defn fulfilled-prerequisites [done-nodes graph]
  (let [neighbour-links (filter #(contains? done-nodes (first %)) graph)
        neighbours (into #{} (map #(last %) neighbour-links))
        unvisited-neighbours (set/difference neighbours done-nodes)
        prerequisite-links (filter #(contains? unvisited-neighbours (second %)) graph)]
      (loop [eligible-nodes unvisited-neighbours
             links prerequisite-links
             ]
        (if (empty? links)
          eligible-nodes
          (let [prerequisite-link (first links)
                requirement (nth prerequisite-link 0)
                ]
            (if (contains? done-nodes requirement)
              (recur eligible-nodes (rest links))
              (recur (disj eligible-nodes (nth prerequisite-link 1)) (rest links))
              )
            )
          )
        )
    )
  )

(defn traverse-parallel [graph candidates workers timetable visited timelimit]
  (let [node (first candidates)]
      (loop [current-time 0
             current-candidates candidates
             current-visited visited
             current-timetable (assign-free-workers candidates workers timetable)]
        (if (>= current-time timelimit)
          current-timetable
          (let [step-timeline (reduce-kv #(assoc %1 %2 (up-time %3)) {} current-timetable)
                free-workers (find-free-workers current-timetable workers)
                new-timeline (into {} (filter #(not (contains? (into #{} free-workers) (first %))) step-timeline))
                new-visited (set/union visited (into #{} (map #(nth (get current-timetable %) 0) free-workers))) 
                neighbour-links (filter #(contains? new-visited (first %)) graph)
                neighbours (into #{} (map #(last %) neighbour-links))
                ; TODO: kle je problem
                new-candidates (set/difference (set/difference (set/union (into #{} current-candidates) neighbours) new-visited) (into #{} (map #(first %) (vals new-timeline))))
                ]
            (recur (inc current-time) new-candidates new-visited (assign-free-workers new-candidates free-workers new-timeline))
            )
          )
        )
      )
  )

;; Psevdokoda
;; - vsem rootom assignaš delavca na začetku (to še ni narjen)
;; - ko obdelaš po urniku en node(trenuten čas >= ord(X) - ord(A), 
;;   - ga je treba vržt ven iz urnika
;;   - poiskat nove root-e (nasledniki)


(defn traverse-parallel-fresh [graph candidates workers timetable visited timelimit]
  (let [node (first candidates)
        all-nodes (set/union (into #{} (map #(nth % 0) graph)) (into #{} (map #(nth % 1) graph)))]
      (loop [current-time 0
             current-candidates candidates
             current-visited visited
             current-timetable (assign-free-workers candidates workers timetable)
             current-free-workers (set/difference (into #{} workers) (into #{} (keys current-timetable)))
             ]
        (if (or (>= (count current-visited) (count all-nodes)) (>= current-time timelimit))
          current-time
          (let [step-timeline (reduce-kv #(assoc %1 %2 (up-time %3)) {} current-timetable)
                finished-tasks (into #{} (done-tasks step-timeline))
                new-timeline (filter-done-timetable step-timeline)
                in-processing-nodes (into #{} (map #(nth (second %) 0) new-timeline))
                free-workers (set/difference (into #{} workers) (into #{} (keys new-timeline)))
                new-visited (set/union current-visited finished-tasks)
                neighbour-links (filter #(contains? finished-tasks (first %)) graph)
                neighbours (into #{} (map #(last %) neighbour-links))
                ; new candidates are all nodes that become available due to worker finishing his job
                new-candidates (set/difference (set/difference (set/union (into #{} current-candidates) neighbours) new-visited) (into #{} (map #(first %) (vals new-timeline))))
                ; actual candidates are all nodes that are elligible out of new-candidates
                ; elligible node must have all of its prerequisites done
                actual-candidates (set/difference (fulfilled-prerequisites new-visited graph) in-processing-nodes)
                reassigned-timetable (assign-free-workers actual-candidates free-workers new-timeline)
                ]
            (recur (inc current-time) new-candidates new-visited reassigned-timetable free-workers)
            )
          )
        )
      )
  )

(defn -main
  [& args]
    (with-open [rdr (clojure.java.io/reader (nth args 0))]
      (let [graph (parse-graph (line-seq rdr))
            roots (find-root graph)
            plan (flatten (traverse graph (sort roots) roots #{}))]
        (println (traverse-parallel-fresh graph (into [] roots) ["a" "b" "c" "d" "e" ] {} #{} 5000))
      )
     )
  )
