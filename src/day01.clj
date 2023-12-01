(ns day01
  (:require [clojure.string :as str]))

;(def fname "test.txt")
(def fname "input.txt")

(with-open [rdr (clojure.java.io/reader (str "assets/day01/" fname))]
  (->>
    (line-seq rdr)
    (map
      (fn [line] (re-seq #"\d" line)))
    (map
      (fn [digits] [(first digits) (last digits)]))
    (map
      (fn [[x, y]] (str x y)))
    (map (fn [num] (Integer/parseInt num)))
    (reduce + 0)
    (println)))


;(def fname2 "test2.txt")
(def fname2 "input.txt")


(def digits ["1", "2", "3", "4", "5", "6", "7", "8", "9"])
(def digit-words ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])

(defn words-to-digit-map [words]
  (->>
    words
    (map-indexed (fn [i, x] [x, (+ i 1)]))
    (into {})))


(def digit-regex
  (re-pattern
    (str/join "|"
              (concat digits digit-words))))


(def word-to-digit
  (merge
    (words-to-digit-map digits)
    (words-to-digit-map digit-words)))


(defn find-overlapping-matches [re s]
  ((fn step [ss]
     (let [m (re-matcher re ss)]
       (when (. m find)
         (let [rest (subs ss (+ 1 (. m start)))]
           (cons (. m group)
                 (lazy-seq (step rest))))))) s))

(assert
  (=
    (find-overlapping-matches digit-regex "eightwothree")
    '("eight", "two", "three")))

(with-open [rdr (clojure.java.io/reader (str "assets/day01/" fname2))]
  (->>
    (line-seq rdr)
    (map
      (fn [line]
        (find-overlapping-matches digit-regex line)))
    (map
      (fn [digits]
        [(first digits) (last digits)]))
    (map
      (fn [nums]
        (map word-to-digit nums)))
    (map
      (fn [[x,y]]
        (+ (* x 10) y)))
    (reduce + 0)
    (println)))
