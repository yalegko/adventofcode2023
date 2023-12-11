(ns day11
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :as test]))

(defn read-universe [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map vec)
         (apply vector))))

(defn at [m i j]
  (-> m (get i) (get j)))

(defn transpose [m]
  (apply mapv vector m))

(defn empty-row? [row]
  (every? #{\.} row))

(defn expand-rows [m]
  (reduce
    (fn [m row]
      (if-not (empty-row? row)
        (conj m row)
        (conj m row row)))
    []
    m))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn solve1 [input]
  (let [universe (read-universe input)
        universe (->> universe (expand-rows) (transpose) (expand-rows) (transpose))
        galaxies (for [i (range (count universe))
                       j (range (count (first universe)))
                       :when (= \# (at universe i j))]
                   [i j])
        pairs (->>
                (for [from galaxies to galaxies
                      :when (not= from to)]
                  (sort [from to]))
                (distinct))]

    (->> pairs
         (map #(apply distance %))
         (reduce +)
         )
    ))

(test/are [in out] (= out (solve1 in))
                   "day11/test.txt" 374
                   "day11/input.txt" 9795148)

(defn count-in-range [points a b]
  (->> (range (min a b) (max a b))
       (set)
       (set/intersection points)
       (count)))

(defn solve2 [input multiplier]
  (let [universe (read-universe input)
        galaxies (for [i (range (count universe))
                       j (range (count (first universe)))
                       :when (= \# (at universe i j))]
                   [i j])
        empty-rows (->> universe
                        (keep-indexed #(when (empty-row? %2) %1))
                        (set))
        empty-cols (->> universe
                        (transpose)
                        (keep-indexed #(when (empty-row? %2) %1))
                        (set))
        pairs (->> (for [from galaxies to galaxies
                         :when (not= from to)]
                     (sort [from to]))
                   (distinct))]
    (->> pairs
         (map (fn [[[x1 y1] [x2 y2]]]
                (+ (distance [x1 y1] [x2 y2])
                   (* (dec multiplier) (count-in-range empty-rows x1 x2))
                   (* (dec multiplier) (count-in-range empty-cols y1 y2)))))
         (reduce +))))

(test/are [in x out] (= out (solve2 in x))
                     "day11/test.txt" 10 1030
                     "day11/test.txt" 100 8410
                     "day11/input.txt" 1000000 650672493820)
