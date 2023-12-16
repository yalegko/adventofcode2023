(ns day16
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map vec)
         (apply vector))))

(defn at [field, i, j]
  (-> field (get i) (get j)))

(defn move [x y direction]
  (case direction
    \^ [(dec x) y direction]
    \> [x (inc y) direction]
    \v [(inc x) y direction]
    \< [x (dec y) direction]))

(defn do-step [field [x y direction]]
  (let [on (at field x y)]
    (cond
      (nil? on) []
      (= \. on) [(move x y direction)]

      (and (= \/ on) (= \^ direction)) [(move x y \>)]
      (and (= \/ on) (= \> direction)) [(move x y \^)]
      (and (= \/ on) (= \v direction)) [(move x y \<)]
      (and (= \/ on) (= \< direction)) [(move x y \v)]

      (and (= \\ on) (= \^ direction)) [(move x y \<)]
      (and (= \\ on) (= \> direction)) [(move x y \v)]
      (and (= \\ on) (= \v direction)) [(move x y \>)]
      (and (= \\ on) (= \< direction)) [(move x y \^)]

      (and (= \| on) (#{\^ \v} direction)) [(move x y direction)]
      (and (= \| on) (#{\< \>} direction)) [(move x y \^) (move x y \v)]

      (and (= \- on) (#{\^ \v} direction)) [(move x y \<) (move x y \>)]
      (and (= \- on) (#{\< \>} direction)) [(move x y direction)]

      :else (throw (ex-info "unexpected step" {:dir direction :at [x y on]})))))

(defn flood [field [pos & queue] visited?]
  (if (nil? pos)
    (remove (fn [[x y _d]] (nil? (at field x y))) visited?)
    (if (visited? pos)
      (recur field queue visited?)
      (let [next-steps (do-step field pos)]
        (recur field (apply conj queue next-steps) (conj visited? pos))))))

;(defn print-field
;  ([field visited] (print-field field visited nil))
;  ([field visited c] (->>
;                       (reduce
;                         (fn [field [x y d]]
;                           (assoc-in field [x y] (or c d)))
;                         ;(if (= \. (at field x y)) (assoc-in field [x y] (or c d)) field))
;                         field
;                         visited)
;                       (map #(apply str %))
;                       (map println))))

(defn energy-level [visited]
  (->> visited
       (map (fn [[x y _d]] [x y]))
       (distinct)
       (count)))

(defn solve1 [input]
  (let [field (read-field input)
        visited (flood field [[0 0 \>]] #{})]
    (energy-level visited)))

(test/are [in out] (= out (time (solve1 in)))
                   "day16/test.txt" 46
                   "day16/input.txt" 7543)

(defn solve2 [input]
  (let [field (read-field input)
        n (count field)
        m (count (first field))]
    (->> (concat
           (for [x (range n)] [x 0 \>])
           (for [x (range n)] [x (dec m) \<])
           (for [y (range m)] [0 y \v])
           (for [y (range m)] [(dec n) y \^]))
         (map #(energy-level (flood field [%] #{})))
         (reduce max))))

(test/are [in out] (= out (time (solve2 in)))
                   "day16/test.txt" 51
                   "day16/input.txt" 8231)
