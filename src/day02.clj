(ns day02
  (:require [clojure.string :as str]))

(defn parse-balls [pull]
  (->> (str/split pull #", ")
       (map #(str/split % #" "))
       (reduce
         (fn [res, [num, color]]
           (assoc res color, (parse-long num)))
         {})))
(assert (=
          (parse-balls "1 red, 2 green, 6 blue")
          {"red" 1, "green" 2, "blue" 6}))

(defn parse-game [line]
  (let [[_, round, balls] (re-find #"Game (\d+): (.*)" line)]
    [
     (parse-long round),
     (->> (str/split balls #"; ")
          (map parse-balls)
          (reduce #(merge-with max %1 %2)))]
    ))
(assert (=
          (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
          [1, {"blue" 6, "red" 4, "green" 2}]))

(defn possible-game? [game]
  (and
    (<= (get game "red" 0) 12)
    (<= (get game "green" 0) 13)
    (<= (get game "blue" 0) 14)))

(assert (possible-game? {"blue" 6, "red" 4, "green" 2}))
(assert (not (possible-game? {"blue" 15, "red" 4, "green" 2})))


(defn solve1 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-game)
         (filter
           (fn [[_round, game]]
             (possible-game? game)))
         (map first)
         (reduce +))))

(assert (= 8 (solve1 "day02/test1.txt")))

(solve1 "day02/input.txt")

(defn solve2 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-game)
         (map last)                                         ; drop round number
         (map
           #(->> %
                 (vals)
                 (reduce *)))
         (reduce +))))

(assert (= 2286 (solve2 "day02/test1.txt")))

(solve2 "day02/input.txt")