(ns day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn sub-seq [s]
  (->> (map vector s (rest s))
       (map (fn [[x y]] (- y x)))))

(test/are [x y] (= x y)
                (sub-seq '(0 3 6 9 12 15)) '(3 3 3 3 3)
                (sub-seq '(3 3 3 3 3)) '(0 0 0 0))

(defn produce-seq [s]
  (loop [res [s]]
    (let [s (last res)]
      (if (every? zero? s)
        res
        (recur (conj res (sub-seq s)))))))

(test/is (= (produce-seq '(0 3 6 9 12 15))
            ['(0 3 6 9 12 15) '(3 3 3 3 3) '(0 0 0 0)]))

(defn predict-next [s]
  (->> s
       (produce-seq)
       (map last)
       (reduce +)))

(test/are [in out] (= out (predict-next in))
                   '(0 3 6 9 12 15) 18
                   '(1 3 6 10 15 21) 28)

(defn solve1 [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(str/split % #" "))
         (map #(map parse-long %))
         (map predict-next)
         (reduce +))))

(test/are [in out] (= out (solve1 in))
                   "day09/test.txt" 114
                   "day09/input.txt" 2005352194)

(defn predict-first [s]
  (->> s
       (produce-seq)
       (map first)
       (reverse)
       (reduce (fn [a x] (- x a)))))

(test/are [in out] (= out (predict-first in))
                   '(0 3 6 9 12 15) -3
                   '(1 3 6 10 15 21) 0
                   '(10 13 16 21 30 45) 5)

(defn solve2 [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(str/split % #" "))
         (map #(map parse-long %))
         (map predict-first)
         (reduce +))))

(test/are [in out] (= out (solve2 in))
                   "day09/test.txt" 2
                   "day09/input.txt" 1077)
