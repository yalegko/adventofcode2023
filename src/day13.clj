(ns day13
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :as test]))

(def test-vertical ["#.##..##."
                    "..#.##.#."
                    "##......#"
                    "##......#"
                    "..#.##.#."
                    "..##..##."
                    "#.#.##.#."])
(def test-horizontal ["#...##..#"
                      "#....#..#"
                      "..##..###"
                      "#####.##."                           ; <
                      "#####.##."                           ; <
                      "..##..###"
                      "#....#..#"])

(defn symmetric? [g1 g2]
  (->> (map vector (reverse g1) g2)
       (map #(apply = %))
       (every? true?)))

(test/are [g1 g2 out] (= out (symmetric? g1 g2))
                      (take 4 test-horizontal) (drop 4 test-horizontal) true
                      (take 3 test-horizontal) (drop 3 test-horizontal) false
                      "#.##." ".##." true
                      "#.##." ".###" false)

(defn find-symmetry
  ([col] (find-symmetry col 1))
  ([col start]
   (let [n (count col)]
     (loop [i start]
       (cond
         (= i n) nil
         (symmetric? (take i col) (drop i col)) i
         :else (recur (inc i)))))))

(test/are [matrix out] (= out (find-symmetry matrix))
                       test-horizontal 4
                       test-vertical nil)

(defn find-all-symmetries [line]
  (loop [start 1 res []]
    (let [i (find-symmetry line start)]
      (if (nil? i)
        res
        (recur (inc i) (conj res i))))))

(test/are [in out] (= out (find-all-symmetries in))
                   "#.##..##." [5 7])

(defn find-vertical-symmetry [matrix]
  (->> matrix
       (map find-all-symmetries)
       (map set)
       (reduce set/intersection)
       (first)))

(test/are [in out] (= out (find-vertical-symmetry in))
                   test-vertical 5
                   test-horizontal nil)

(defn parse-matrixes [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (partition-by empty?)                              ; Group lines by empty line.
         (remove #(= 1 (count %)))                          ; Drop empty groups.
         (map vec)
         (doall))))

(defn solve1 [input]
  (->> (parse-matrixes input)
       (map (juxt find-symmetry find-vertical-symmetry))
       (map (partial map #(or % 0)))
       (map (fn [[row col]] (+ (* row 100) col)))
       (reduce +)))

(test/are [in out] (= out (solve1 in))
                   "day13/test.txt" 405
                   "day13/input.txt" 29165)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn difference [l1 l2]
  (->> (map vector (str l1) (str l2))                       ; Input can be 2 chars or 2 strings
       (map (fn [[c1 c2]] (if (= c1 c2) 0 1)))
       (reduce +)))

(defn groups-difference [g1 g2]
  (->> (map vector (reverse g1) g2)
       (map #(apply difference %))
       (reduce +)))

(test/are [g1 g2 out] (= out (groups-difference g1 g2))
                      (take 4 test-horizontal) (drop 4 test-horizontal) 0
                      (take 3 test-horizontal) (drop 3 test-horizontal) 19
                      "#.##." ".##." 0
                      "#.##." ".###" 1
                      "....." "####" 4)

(defn find-fixable-symmetry [col]
  (let [n (count col)]
    (loop [i 1]
      (cond
        (= i n) nil
        (= 1 (groups-difference (take i col) (drop i col))) i
        :else (recur (inc i))))))

(test/are [in out] (= out (find-fixable-symmetry in))
                   test-vertical 3
                   test-horizontal 1)

(defn transpose [m]
  (apply mapv vector m))

(defn solve2 [input]
  (->> (parse-matrixes input)
       (map (juxt find-fixable-symmetry (comp find-fixable-symmetry transpose)))
       (map (partial map #(or % 0)))
       (map (fn [[row col]] (+ (* row 100) col)))
       (reduce +)))

(test/are [in out] (= out (solve2 in))
                   "day13/test.txt" 400
                   "day13/input.txt" 32192)
