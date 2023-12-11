(ns day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn file-lines [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (reduce conj [] (line-seq rdr))))

(defn parse-seeds [seeds-line]
  (->> seeds-line
       (drop (count "seeds: "))
       (apply str)
       (#(str/split % #" "))
       (map parse-long)))

(defn make-interval [start step]
  [start (+ start step)])

(defn parse-range [range-line]
  (->> range-line
       (#(str/split % #" "))
       (map parse-long)
       ((fn [[out in step]]
          [(make-interval in step)
           (make-interval out step)]))))

(test/are
  [in out] (= out (parse-range in))
           "50 98 2" [[98 100] [50 52]]
           "3071831978 1031969734 125782276" [[1031969734 1157752010] [3071831978 3197614254]])

(defn parse-mapping [[_header & lines]]
  (->> lines
       (map parse-range)))

(test/is (=
           (parse-mapping '("soil-to-fertilizer map:" "0 15 37" "37 52 2" "39 0 15"))
           [[[15 52] [00 37]]
            [[52 54] [37 39]]
            [[00 15] [39 54]]]))

(defn map-number [n map]
  (->> map
       (some
         (fn [[[in-from in-to] [out-from _out-to]]]
           (when (and (<= in-from n) (<= n in-to))
             (+ out-from (- n in-from)))))
       (#(or % n))))

(let [m [(parse-range "50 98 2") (parse-range "52 50 48")]]
  (test/are
    [in out] (= out (map-number in m))
             98 50
             99 51
             53 55))

(defn solve1 [input]
  (let [[seeds-line _empty & others] (file-lines input)
        seeds (parse-seeds seeds-line)
        mappings (->> others
                      (partition-by empty?)
                      (remove #(= 1 (count %)))
                      (map parse-mapping))]
    (->> seeds
         (map #(reduce map-number % mappings))
         (reduce min))))

(test/are [in res] (= res (solve1 in))
                   "day05/test1.txt" 35
                   "day05/input.txt" 26273516)

(defn map-interval [rng stage]
  (let [[res _] (reduce
                  (fn [[mapped [a b]] [[from-a from-b] _to :as m]]
                    (cond
                      ; Mapped all the interval. Enough.
                      (= a b nil) (reduced [mapped nil])

                      ; Mapping starts too late. As stage is sorted -- no luck further.
                      (<= a b from-a from-b) (reduced [mapped nil])

                      ; Does not touch the mapping. Try next one.
                      (<= from-a from-b a b) [mapped [a b]]

                      ; Interval is mapped as a whole.
                      (<= from-a a b from-b) [
                                              (conj mapped
                                                    [(map-number a [m]) (map-number b [m])])
                                              nil
                                              ]

                      ; Mapping maps some part of the interval.
                      (<= from-a a from-b b) [
                                              (conj mapped
                                                    [(map-number a [m]) (map-number from-b [m])])
                                              [from-b b]
                                              ]
                      (<= a from-a b from-b) [
                                              (conj mapped
                                                    [a from-a] ; Early part. Won't be mapped anymore.
                                                    [(map-number from-a [m]) (map-number b [m])])
                                              nil
                                              ]

                      :else (throw (ex-info "Unexpected mapping" {:int [a b] :mapping-in [from-a from-b]}))
                      ))
                  [[] rng]
                  stage)]
    (if (empty? res) [rng] res)))

(let [stage '([[50 98] [52 100]] [[98 100] [50 52]])]
  (test/are [in out] (= out (map-interval in stage))
                     [55 99] [[57 100] [50 51]]
                     [50 98] [[52 100]]
                     [10 20] [[10 20]]))

(defn solve2 [input]
  (let [[seeds-line _empty & others] (file-lines input)
        seed-intervals (->> seeds-line
                            (parse-seeds)
                            (partition 2)
                            (map #(apply make-interval %)))
        mapping-stages (->> others
                            (partition-by empty?)           ; Group lines by empty line
                            (remove #(= 1 (count %)))       ; Drop empty line group
                            (map parse-mapping)             ; Parse each as mapping
                            (map sort))]                    ; Sort by input interval
    (->> mapping-stages
         (reduce
           (fn [rngs stage] (mapcat #(map-interval % stage) rngs))
           seed-intervals)
         (map (fn [[a _b]] a))
         (reduce min))))

(test/are [in res] (= res (solve2 in))
                   "day05/test1.txt" 46
                   "day05/input.txt" 34039469)
