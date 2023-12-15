(ns day15
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn read-inputs [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (slurp rdr)
         (partition-by #{\,})
         (remove #(= 1 (count %))))))

(defn HASH [line]
  (->> line
       (reduce #(-> (int %2)
                    (+ %1)
                    (* 17)
                    (rem 256))
               0)))

(test/are [in out] (= out (HASH in))
                   "HASH" 52
                   "cm-" 253)

(defn solve1 [input]
  (->> (read-inputs input)
       (map HASH)
       (reduce +)))

(test/are [in out] (= out (HASH in))
                   "day15/test.txt" 1320
                   "day15/input.txt" 511343)

(defn index-of [col pred]
  (->> col
       (keep-indexed #(when (pred %2) %1))
       (first)))

(test/are [col val res] (= res (index-of col #{val}))
                        [1 2 3] 1 0
                        [1 2 3] 2 1
                        [1 2 3] 4 nil)

(defn lens->box [box [name op num]]
  (case op
    "-" (vec (remove #(= name (first %)) box))
    "=" (if-let [i (index-of box #(= name (first %)))]
          (assoc box i [name num])
          (conj box [name num]))))

(test/are [in out] (= out (reduce lens->box [] in))
                   '(("rn" "=" "1") ("cm" "=" "2")) [["rn" "1"] ["cm" "2"]]
                   '(("rn" "=" "1") ("cm" "=" "2") ("rn" "-" nil)) [["cm" "2"]]
                   '(("rn" "=" "1") ("rn" "=" "2")) [["rn" "2"]])

(defn solve2 [input]
  (->> (read-inputs input)
       (map #(apply str %))
       (map #(rest (re-find #"(\w+)([-=])(\d+)?" %)))
       (group-by #(HASH (first %)))
       (map (fn [[k v]] [k (reduce lens->box [] v)]))
       (remove #(empty? (second %)))
       (map (fn [[k v]] [k (map second v)]))
       (map (fn [[k v]] [k (map parse-long v)]))
       (map (fn [[box lenses]] (* (inc box)
                                  (reduce + (map-indexed #(* (inc %1) %2) lenses)))))
       (reduce +)))

(test/are [in out] (= out (solve2 in))
                   "day15/test.txt" 145
                   "day15/input.txt" 294474)
