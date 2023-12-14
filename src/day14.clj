(ns day14
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map vec)
         (apply vector))))

(defn unzip [input]
  (for [iter (iterate (partial map rest) input)
        :while (every? seq iter)]
    (map first iter)))

(defn step-up [top bot]
  (->> (map vector top bot)
       (map (fn [[c1 c2]]
              (if (and (= c1 \.) (= c2 \O))
                [\O \.]
                [c1 c2])))
       (unzip)
       (map #(apply str %))))

(test/are [l1 l2 res] (= res (step-up l1 l2))
                      "O..#.#...."
                      "O.OO#...O#"
                      '("O.O#.#..O."
                         "O..O#....#"))

(defn lift-row [field i]
  (if (= 0 i)
    field
    (let [prev (dec i)
          [l1 l2] (step-up (get field prev) (get field i))
          field (-> field (assoc prev l1) (assoc i l2))]
      (recur field prev))))

(test/are [field res] (= res (lift-row field 2))
                      ["O..#.#...."
                       "OO.O#...O#"
                       ".O.....O#."]
                      ["OO.#.#.OO."
                       "O..O#....#"
                       ".O......#."])

(def lift-all
  (memoize
    (fn [field]
      (->> (range 1 (count field))
           (reduce
             (fn [F i] (lift-row F i))
             field)))))

(defn solve1 [input]
  (->> (read-field input)
       (lift-all)
       (reverse)
       (map-indexed
         (fn [i line] (* (inc i) (count (filter #{\O} line)))))
       (reduce +)))

(test/are [in out] (= out (time (solve1 in)))
                   "day14/test.txt" 136
                   "day14/input.txt" 108813)

(defn rotate [m]
  (vec (map (comp vec reverse) (apply map vector m))))

(defn do-spin [field]
  (reduce (fn [F _i] (rotate (lift-all F)))
          field
          (range 4)))

(def N 1000000000)

(defn find-cycle [field]
  (->> (range N)
       (reduce
         (fn [[F seen] i]
           (if (contains? seen F)
             (reduced [F i (- i (seen F))])
             [(do-spin F) (assoc seen F i)]))
         [field {}])))

(defn solve2 [input]
  (let [[field i cycle-len] (find-cycle (read-field input))
        raw-repeats (rem (- N i) cycle-len)]
    (println "Found a cycle at" i "of len" cycle-len)
    (->> (range raw-repeats)
         (reduce
           (fn [F _i] (do-spin F))
           field)
         (reverse)
         (map-indexed
           (fn [i line] (* (inc i) (count (filter #{\O} line)))))
         (reduce +))))

(test/are [in out] (= out (time (solve2 in)))
                   "day14/test.txt" 64
                   "day14/input.txt" 104533)
