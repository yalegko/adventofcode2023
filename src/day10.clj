(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (apply vector (map vec (line-seq rdr)))))

(defn field-at [field i j]
  (-> field (get i) (get j)))

(defn find-s [field]
  (->> field
       (map-indexed #(vector %1 (.indexOf %2 \S)))
       (remove #(= -1 (second %)))
       (first)))
(defn adjacent [[x y]]
  (vector [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn connected [field [row col]]
  (case (field-at field row col)
    \- [[row (dec col)] [row (inc col)]]
    \| [[(dec row) col] [(inc row) col]]
    \L [[(dec row) col] [row (inc col)]]
    \J [[row (dec col)] [(dec row) col]]
    \7 [[row (dec col)] [(inc row) col]]
    \F [[row (inc col)] [(inc row) col]]
    (throw (ex-info "unexpected symbol" {:row row :col col}))))

(defn find-first-move [field start]
  (->> start
       (adjacent)
       (remove (fn [[x y]] (contains? #{nil \.} (field-at field x y))))
       (map #(vector % (connected field %)))
       (filter (fn [[_p connections]] (some #{start} connections)))
       (first)                                              ; Return only points.
       (first)))                                            ; Choose any from the 2 returned.

(defn move [field prev curr]
  (->> (connected field curr)
       (remove #{prev})
       (first)))

(defn find-main-loop [field start]
  (loop [path [start]
         curr (find-first-move field start)]
    (if (= curr start)
      path
      (recur (conj path curr) (move field (last path) curr)))))

(defn solve1 [input]
  (let [field (read-field input)
        start (find-s field)]
    (->> (find-main-loop field start)
         (count)
         (#(/ % 2)))))

(test/are [in out] (= out (solve1 in))
                   "day10/test1.txt" 4
                   "day10/test2.txt" 4
                   "day10/test3.txt" 8
                   "day10/input.txt" 6931)

(defn inflate-field [field]
  (let [n (count (first field))
        empty-row (vec (repeat (inc (* 2 n)) \,))]
    (->> field
         ; Add an empty column between every column pair.
         (map #(interleave (repeat \,) %))
         (map vec)
         ; Add an empty column at the end.
         (map #(conj % \,))
         ; Add an empty row between every 2 rows.
         (interleave (repeat empty-row))
         (apply vector)
         ; Append empty row at the end.
         (#(conj % empty-row)))))

(defn missing-point [[x1 y1] [x2 y2]]
  (assert (<= (abs (- y1 y2)) 2))
  (assert (<= (abs (- x1 x2)) 2))
  (cond
    (= x1 x2) [x1 (inc (min y1 y2))]
    (= y1 y2) [(inc (min x1 x2)) y1]
    :else (throw (ex-info "strange pair" {:a [x1 y1] :b [x2 y2]}))))

(test/are [p1 p2 out] (= out (missing-point p1 p2))
                      [3 3] [5 3] [4 3]
                      [3 5] [3 3] [3 4])

(defn rotate [n s]
  (lazy-cat (drop n s)
            (take n s)))

(test/is (= '(2 3 1) (rotate 1 '(1 2 3))))

(defn inflate-path [path]
  (->> path
       (map (fn [[x y]] [(inc (* 2 x)) (inc (* 2 y))]))
       (#(map vector % (rotate 1 %)))
       (mapcat (fn [[from to]] [from (missing-point from to)]))
       ))

(defn print-field [field]
  (->> field
       (map (partial str/join " "))
       (map println))
  field)

(defn update-field-at [field [i j] c]
  (update field i #(assoc % j c)))

(defn update-field [field points c]
  (reduce #(update-field-at %1 %2 c)
          field
          points))

(defn bfs
  ([field]
   (bfs field [[0 0]] #{}))

  ([field queue visited]
   (if (empty? queue)
     visited
     (let [[p & queue] queue
           to-visit (->> p
                         (adjacent)
                         (remove visited)
                         (remove #(contains? #{nil \x} (apply field-at field %))))]
       (recur field (apply conj queue to-visit) (conj visited p))))))


(defn solve2 [input]
  (let [field (read-field input)
        start (find-s field)
        path (find-main-loop field start)]
    (as-> field F
          ; Add empty row between every pair of rows and column between every pair of cols.
          (inflate-field F)
          ; Mark all points related to the (inflated) path as \x.
          (update-field F (inflate-path path) \x)
          ; Mark all points accessible from the (0 0) point as \o.
          (update-field F (bfs F) \o)
          ; Count all not-visited and not-additional cells.
          (map #(count (remove #{\x \o \  \,} %)) F)
          (reduce + F))))

(test/are [in out] (= out (solve2 in))
                   "day10/test1.txt" 1
                   "day10/test2.txt" 1
                   "day10/test3.txt" 1
                   "day10/test4.txt" 4
                   "day10/test5.txt" 4
                   "day10/test6.txt" 8
                   "day10/test7.txt" 10
                   "day10/input.txt" 357)
