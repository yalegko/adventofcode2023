(ns day06
  (:require [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-table [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(str/split % #"\s+"))
         (map rest)
         (map #(map parse-long %))
         (apply map vector)
         (map (fn [[t d]] (hash-map :time t :dst d))))))

(defn next-int ^Double [x]
  (let [next-float (Math/ceil x)]
    (if (= x next-float)
      (int (inc x))
      (int next-float))))

(test/are [in out] (= out (next-int in))
                   1.0 2
                   1.1 2
                   1.9 2
                   2.0 3)

(defn prev-int ^Double [x]
  (let [prev-float (Math/floor x)]
    (if (= x prev-float)
      (int (dec x))
      (int prev-float))))

(test/are [in out] (= out (prev-int in))
                   2.0 1
                   2.1 2
                   2.9 2
                   3.0 2)

(defn solve-eq [t d]
  (let [D (- (* t t) (* 4 d))
        sq-D (Math/sqrt D)]
    (->> [(- t sq-D) (+ t sq-D)]
         (map #(/ % 2))
         ((fn [[x1 x2]] [(next-int x1) (prev-int x2)]))
         )))
(test/are [t d res] (= res (solve-eq t d))
                    7 9 '(2 5)
                    15 40 '(4 11)
                    30 200 '(11 19))

(defn solve1 [input]
  (->> (parse-table input)
       (map #(apply solve-eq (map % [:time :dst])))
       (map (fn [[from to]] (- to from)))
       (map inc)
       (reduce *)))

(test/is (= 288 (solve1 "day06/test.txt")))
(test/is (= 293046 (solve1 "day06/input.txt")))

(defn parse-table-2 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(str/split % #"\s+"))
         (map rest)
         (map #(apply str %))
         (map parse-long)
         (apply vector))))

(defn solve2 [input]
  (->> (parse-table-2 input)
       (apply solve-eq)
       (reverse)
       (reduce -)
       (inc)))

(test/is (= 71503 (solve2 "day06/test.txt")))

(solve2 "day06/input.txt")
