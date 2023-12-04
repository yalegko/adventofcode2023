(ns day04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn card-score [card-line]
  (->> card-line
       ; Drop "Game X".
       (drop-while #(not= \: %))
       ; Drop ": ".
       (drop 2)
       ; Cast lazy-seq back into string.
       (apply str)
       ; Split into [winning, draw].
       (#(str/split % #"\|"))
       ; Split each part into numbers.
       (map #(str/split % #" "))
       (map #(filter not-empty %))
       ; Count the intersection size.
       (map set)
       (apply set/intersection)
       (count)))

(defn solve1 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map card-score)
         (remove zero?)
         (map #(Math/pow 2 (dec %)))
         (map int)
         (reduce +))))

(assert (= 13 (solve1 "day04/test1.txt")))

(solve1 "day04/input.txt")

(defn calc-card-scores [input]
  ; Make a map of card scores.
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map card-score)
         (map vector (range))
         (into {})
         )))

(defn add-cards [cards i score]
  ; Weird cards duplication algorithm.
  (let [i+1 (inc i)]
    (reduce
      (fn [cards, j] (assoc cards j (+ (get cards j) (get cards i))))
      cards
      (range i+1 (+ i+1 score)))))

(defn solve2 [input]
  (let [scores (calc-card-scores input)
        cards (into {} (map #(vec [%, 1]) (range (count scores))))]
    (->>
      ; Loop over cards duplicating them.
      (loop [i 0
             scores scores
             cards cards]
        (if (nil? (cards i))
          cards
          (recur (inc i) scores (add-cards cards i (scores i)))))
      ; Sum the cards amount.
      (vals)
      (reduce +))))

(assert (= 30 (solve2 "day04/test1.txt")))

(solve2 "day04/input.txt")
