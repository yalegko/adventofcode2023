(ns day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn file-lines [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (reduce conj [] (line-seq rdr))))

(defn parse-map [lines]
  (->> lines
       (map #(re-seq #"[A-Z]{3}" %))
       (map (fn [[src left right]] [src [left right]]))
       (into {})))

(defn find-path-len [from via route stop-f]
  (loop [i 0
         position from
         path (cycle route)]
    (if (stop-f position)
      i
      (let [[step & path] path
            [left, right] (via position)
            next-pos ({\L left \R right} step)]
        (recur (inc i) next-pos path)))))

(defn solve1 [input]
  (let [[directions _empty & rest] (file-lines input)
        routes (parse-map rest)]
    (find-path-len "AAA" routes directions #(= % "ZZZ"))))

(test/are [in out] (= out (solve1 in))
                   "day08/test.txt" 2
                   "day08/test2.txt" 6
                   "day08/input.txt" 22357)

(defn gcd [x y]
  (if (= 0 y) x (recur y (rem x y))))

(test/are [x y r] (= r (gcd x y))
                  2 3 1
                  6 3 3
                  6 12 6)

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn solve2 [input]
  (let [[directions _empty & rest] (file-lines input)
        routes (parse-map rest)
        ]
    (->> (keys routes)
         (filter #(str/ends-with? % "A"))
         (map (fn [start]
                (find-path-len start routes directions #(str/ends-with? % "Z"))))
         (reduce lcm))))

(test/are [in out] (= out (solve2 in))
                   "day08/test3.txt" 6)

(solve2 "day08/input.txt")
