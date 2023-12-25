(ns day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-edges [line]
  (let [[from tos] (str/split line #": ")]
    (for [to (str/split tos #" ")]
      [from to])))

(defn read-graph [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (mapcat parse-edges)
         (mapcat (fn [[from to]] (vector [from to] [to from])))
         (group-by first)
         (map (fn [[k v]] [k (vec (map second v))]))
         (into {}))))

(defn bfs
  ([graph] (bfs graph [(first (keys graph))] #{} {}))
  ([graph times-visited start] (bfs graph [start] #{} times-visited))
  ([graph [v & rest] seen? times-visited]
   (if (nil? v)
     times-visited
     (let [to-do (->> (graph v)
                      (remove seen?))
           edges (map #(set [v %]) to-do)]
       (recur graph
              (concat rest to-do)
              (into seen? to-do)
              (reduce #(update %1 %2 (fnil inc 0)) times-visited edges))))))

(defn top-visited-edges [graph]
  (->> (keys graph)
       (reduce
         #(bfs graph %1 %2)
         {})
       (sort-by last #(compare %2 %1))
       (map (comp vec first))))

(defn without-edge [graph [from to]]
  (-> graph
      (update to #(remove #{from} %))
      (update from #(remove #{to} %))))

(defn solve1 [input]
  (let [graph (read-graph input)
        n-vertices (count (keys graph))
        top-visited (top-visited-edges graph)
        top10 (take 10 top-visited)]
    (->> (for [e1 top10
               e2 top10
               e3 top10
               :when (every? neg? [(compare e1 e2) (compare e2 e3)])]
           [e1 e2 e3])
         (map #(reduce without-edge graph %))
         (map (comp vals bfs))
         (map #(reduce + %))
         (filter #(not= % n-vertices))
         (first)
         (#(* % (- n-vertices %))))))

(test/are [in out] (= out (time (solve1 in)))
                   "day25/test.txt" 54
                   "day25/input.txt" 601344)
