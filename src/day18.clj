(ns day18
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn parse1 [line]
  (let [[_all d n] (re-find #"(\w) (\d+) \(#[0-9a-f]+\)" line)]
    [d (parse-long n)]))

(defn move [[x y] d n]
  (case d
    "U" [(- x n) y]
    "D" [(+ x n) y]
    "R" [x (+ y n)]
    "L" [x (- y n)]))

(defn read-polygon [parse input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse)
         (reduce
           (fn [[edges pos] [d n]]
             (let [new-pos (move pos d n)]
               [(conj edges [pos new-pos]) new-pos]))
           [[] [0 0]])
         (first))))

(defn polygon-area [edges]
  ; https://gamedev.stackexchange.com/questions/151034/how-to-compute-the-area-of-an-irregular-shape
  (->> edges
       (map
         (fn [[[x1 y1] [x2 y2]]]
           (-
             (* x1 y2)
             (* x2 y1))))
       (reduce +)
       (#(/ % 2))
       ; Result can be negative if we're moving CW instead of CCW. Don't care it's still same number.
       (abs)))

(defn polygon-perimeter [edges]
  (->> edges
       (map
         (fn [[[x1 y1] [x2 y2]]]
           (+
             (abs (- y1 y2))
             (abs (- x1 x2)))))
       (reduce +)))

(defn solve1
  ([input] (solve1 input parse1))
  ([input parse-f]
   (let [P (read-polygon parse-f input)]
     (+
       (polygon-area P)
       ; We've already counted half of the edges calculating an area (had to inc upper bounds?).
       (/ (polygon-perimeter P) 2)
       ; Plus starting point lost dividing perimeter? No idea + don't care.
       1))))

(test/are [in out] (= out (time (solve1 in)))
                   "day18/test.txt" 62
                   "day18/input.txt" 47675)

(defn parse2 [line]
  (let [[_all hex] (re-find #"#([0-9a-f]+)" line)
        d ({\0 "R" \1 "D" \2 "L" \3 "U"} (last hex))
        num (Integer/parseInt (apply str (butlast hex)) 16)]
    [d num]))

(defn solve2 [input]
  (solve1 input parse2))

(test/are [in out] (= out (time (solve2 in)))
                   "day18/test.txt" 952408144115
                   "day18/input.txt" 122103860427465)
