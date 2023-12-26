(ns day23
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map vec)
         (apply vector))))

(defn at [field [i j]]
  (-> field (get i) (get j)))

(defn all-adjacent [[x y]]
  (vector [(dec x) y \^] [(inc x) y \v] [x (dec y) \<] [x (inc y) \>]))

(defn can-go? [field [x y d]]
  (let [c (at field [x y])]
    (cond
      (= \. c) true
      (= \# c) false
      :else (= c d))))

(defn adjacent
  ([field p] (adjacent field can-go? p))
  ([field can-go? p]
   (->> (all-adjacent p)
        (filter #(can-go? field %))
        (map (fn [[x y _d]] [[x y] 1])))))

(defn find-longest-path [adjacent-f from to]
  (loop [max-path 0
         [[p l seen?] & queue] [[from 0 #{}]]]
    (cond
      (nil? p) max-path
      (= p to) (recur (max max-path l) queue)
      :else
      (let [to-visit (->> (adjacent-f p)
                          (remove #(seen? (first %)))
                          (map (fn [[v d]] [v (+ d l) (conj seen? v)])))]
        (recur max-path
               (into (or queue []) to-visit))))))

(defn first-space [line]
  (->> line (keep-indexed #(when (= %2 \.) %1)) (first)))

(defn solve1 [input]
  (let [field (read-field input)
        start [0 (->> (first field) (first-space))]
        finish [(dec (count field)) (->> (last field) (first-space))]]
    (find-longest-path #(adjacent field %) start finish)))

(test/are [in out] (= out (time (solve1 in)))
                   "day23/test.txt" 94
                   "day23/input.txt" 2222)

(defn can-go2? [field [x y & _d]]
  (not (contains? #{\# nil} (at field [x y]))))

(defn vertices [field adjacent-f]
  (for [x (range (count field))
        y (range (count (first field)))
        :when (and (can-go2? field [x y])
                   (> (count (adjacent-f [x y])) 2))]
    [x y]))

(defn add-connection [connections from to dist]
  (update connections from #(conj (or % []) [to dist])))

(defn vertex->connections [adjacent-f vertex? v]
  (let [vertex? (disj vertex? v)]
    (loop [connections {}
           seen? #{v}
           [[p l] & queue] [[v 0]]]
      (cond
        (nil? p) connections
        (vertex? p) (recur (add-connection connections v p l) seen? queue)
        :else
        (let [to-visit (->> (adjacent-f p)
                            (remove #(seen? (first %)))
                            (map (fn [[v d]] [v (+ d l)])))]
          (recur connections
                 (into seen? (map first to-visit))
                 (into (or queue []) to-visit)))))))

(defn solve2 [input]
  (let [field (read-field input)
        start [0 (->> (first field) (first-space))]
        finish [(dec (count field)) (->> (last field) (first-space))]

        adjacent-f (fn [v] (adjacent field can-go2? v))
        vertices (set (-> (vertices field adjacent-f)
                          (conj start)
                          (conj finish)))
        graph (->> vertices
                   (map #(vertex->connections adjacent-f vertices %))
                   (reduce merge))]
    (find-longest-path graph start finish)))

(test/are [in out] (= out (time (solve2 in)))
                   "day23/test.txt" 154
                   "day23/input.txt" 6590)
