(ns day17
  (:require [clojure.java.io :as io]
            [clojure.test :as test])
  (:import [java.util PriorityQueue]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(map (comp parse-long str) %))
         (map vec)
         (apply vector))))
(defn at [field, i, j]
  (-> field (get i) (get j)))

(test/is (= 2 (at (read-field "day17/test.txt") 0 0)))

(defn adjacent [x y d]
  (let [old-d (first d)
        res (case old-d
              \> [[x (inc y) (str d ">")] [(dec x) y "^"] [(inc x) y "v"]]
              \v [[(inc x) y (str d "v")] [x (dec y) "<"] [x (inc y) ">"]]
              \< [[x (dec y) (str d "<")] [(inc x) y "v"] [(dec x) y "^"]]
              \^ [[(dec x) y (str d "^")] [x (dec y) "<"] [x (inc y) ">"]])]
    (remove (fn [[_x' _y' d']] (= 4 (count d'))) res)))

(test/is (= 2 (count (adjacent 1 1 ">>>"))))

(defn get-dist [distances pos]
  (or (get distances pos) ##Inf))

(defn fix-priority [queue]
  (->> queue
       (sort-by last)
       (reduce
         (fn [[seen? queue] [x y dir dist]]
           (if (seen? [x y dir])
             [seen? queue]
             [(conj seen? [x y dir]) (conj queue [x y dir dist])]))
         [#{} []])
       (second)))

(test/are [in out] (= out (fix-priority in))
                   [[1 1 ">" 3] [1 1 ">" 2] [1 2 ">" 1]] [[1 2 ">" 1] [1 1 ">" 2]]
                   [[1 1 "^" 3] [1 1 ">" 2] [1 2 ">" 1]] [[1 2 ">" 1] [1 1 ">" 2] [1 1 "^" 3]])

(defn get-dest-distances [[x y] distances]
  (->> [">" "v"]
       (map #(get distances [x y %]))))

; PriorityQueue of [distance [x y] direction] (we keep distance first to define sort order).
(def queue (PriorityQueue. []))

(defn dijkstra
  ([field adjacent-f finished? start]
   (.clear queue)
   (doseq [[[x y] dir] start]
     (.add queue [0 [x y] dir]))
   (dijkstra [field adjacent-f finished?] {}))

  ([[field adjacent-f finished? :as task-descriptor] distances]
   ;(println (.size queue) (count distances))
   (let [[dist [x y] dir :as pos] (.poll queue)]
     (cond
       ; Out of hypothesis.
       (nil? pos) distances

       ; Found a way to get to the destination.
       (finished? [x y dir]) distances

       ; We've already been in [x y] with given direction earlier. No need to elaborate.
       (> dist (get-dist distances [x y dir]))
       (recur task-descriptor distances)

       :else
       (->> (adjacent-f x y dir)
            (map (fn [[x y dir]] [x y dir (at field x y)]))
            (remove #(nil? (last %)))
            (map (fn [[x y dir dist']] [x y dir (+ dist dist')]))
            (remove (fn [[x y dir dist]] (>= dist (get-dist distances [x y dir]))))
            (reduce (fn [distances' [x y dir dist]]
                      (.add queue [dist [x y] dir])
                      (assoc distances' [x y dir] dist))
                    distances)
            (recur task-descriptor))))))

(defn solve1 [input]
  (let [field (read-field input)
        max-x (dec (count field))
        max-y (dec (count (first field)))
        finished? (fn [[x y _d]] (= [x y] [max-x max-y]))]
    (->> (dijkstra field adjacent finished? [[[0 0] ">"] [[0 0] "v"]])
         (get-dest-distances [max-x max-y])
         (remove nil?)
         (reduce min))))

(test/are [in out] (= out (time (solve1 in)))
                   "day17/test.txt" 102
                   "day17/input.txt" 916)

(defn adjacent-large [x y [dir n]]
  (->> (case dir
         ">" [[x (inc y) [">" (inc n)]] [(dec x) y ["^" 1]] [(inc x) y ["v" 1]]]
         "v" [[(inc x) y ["v" (inc n)]] [x (dec y) ["<" 1]] [x (inc y) [">" 1]]]
         "<" [[x (dec y) ["<" (inc n)]] [(inc x) y ["v" 1]] [(dec x) y ["^" 1]]]
         "^" [[(dec x) y ["^" (inc n)]] [x (dec y) ["<" 1]] [x (inc y) [">" 1]]])
       (remove
         (fn [[_x' _y' [dir' _n']]]
           (cond
             (< n 4) (not= dir dir')                        ; Can't turn yet.
             (= n 10) (= dir dir')                          ; Must turn.
             :else false)))))

(test/are [in out] (= out (apply adjacent-large in))
                   [1 1 [">" 1]] [[1 2 [">" 2]]]
                   [1 1 [">" 4]] [[1 2 [">" 5]] [0 1 ["^" 1]] [2 1 ["v" 1]]]
                   [1 1 [">" 10]] [[0 1 ["^" 1]] [2 1 ["v" 1]]])

(defn get-final-distances-2 [[x y] distances]
  (for [d [">" "v"]
        n (range 4 11)]
    (get distances [x y [d n]])))

(defn solve2 [input]
  (let [field (read-field input)
        max-x (dec (count field))
        max-y (dec (count (first field)))
        finished? (fn [[x y [_d n]]]
                    (and (= [x y] [max-x max-y])
                         (>= n 4))
                    )]
    (->> (dijkstra field adjacent-large finished? [[[0 0] [">" 1]] [[0 0] ["v" 1]]])
         (get-final-distances-2 [max-x max-y])
         (remove nil?)
         (reduce min))))

(test/are [in out] (= out (time (solve2 in)))
                   "day17/test.txt" 94
                   "day17/test2.txt" 71
                   "day17/test3.txt" 19
                   "day17/input.txt" 1067)
