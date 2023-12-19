(ns day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-pipeline [line]
  (let [[_ name cond-str] (re-find #"(\w+)\{(.+)}" line)
        conditions (->> (str/split cond-str #",")
                        (map #(str/split % #":"))
                        (map (fn [[cond dst]]
                               (if (nil? dst)
                                 [nil cond]
                                 (let [[_ x op v] (re-find #"(.+)([<>])(.+)" cond)]
                                   [[x op (parse-long v)] dst]))))
                        (vec))]
    [name conditions]))

(defn parse-part [line]
  (->> (subs line 1 (dec (count line)))
       (#(str/split % #","))
       (map #(str/split % #"="))
       (map (fn [[x v]] [x (parse-long v)]))
       (into {})))

(defn eval-pipeline [pipe vals]
  (reduce
    (fn [vals [[x op v :as cnd] dst]]
      (cond
        (nil? cnd) (reduced dst)
        (= op ">") (if (> (vals x) v) (reduced dst) vals)
        (= op "<") (if (< (vals x) v) (reduced dst) vals)
        :else (throw (ex-info "unexpected op" {:condition [x op v "->" dst]}))
        ))
    vals
    pipe))

(test/are [pipe vals res]
  (= res (eval-pipeline (second (parse-pipeline pipe)) vals))
  "px{a<2006:qkq,m>2090:A,rfg}" {"a" 2005} "qkq"
  "px{a<2006:qkq,m>2090:A,rfg}" {"a" 2006 "m" 3000} "A"
  "px{a<2006:qkq,m>2090:A,rfg}" {"a" 2006 "m" 0} "rfg")

(defn process-part [pipes stage part]
  (let [next (eval-pipeline (get pipes stage) part)]
    (if (#{"A" "R"} next)
      next
      (recur pipes next part))))

(defn read-input [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (let [[pipes-lines parts-lines] (->> (line-seq rdr)
                                         (partition-by empty?)
                                         (remove #{'("")}))
          pipes (->> pipes-lines
                     (map parse-pipeline)
                     (into {}))
          parts (->> parts-lines
                     (map parse-part)
                     (into []))]
      [pipes parts])))

(defn solve1 [input]
  (let [[pipes parts] (read-input input)]
    (->> parts
         (map #(vector % (process-part pipes "in" %)))
         (filter #(#{"A"} (last %)))
         (map first)
         (map #(reduce-kv (fn [acc _k v] (+ acc v)) 0 %))
         (reduce +))))

(test/are [in out] (= out (time (solve1 in)))
                   "day19/test.txt" 19114
                   "day19/input.txt" 383682)

(defn map-rng
  "Returns a pair of @ranges (a b) (both sides not included) map as [mapped, unmapped]"
  [[x op v] ranges]
  (let [[a b] (ranges x)]
    (cond
      (nil? x) [ranges, nil]                                ; Last clause.

      (and (= op ">") (<= b v)) [nil ranges]                ; a b v
      (and (= op ">") (<= v a)) [ranges nil]                ; v a b
      (and (= op ">") (< a v b))                            ; a v b
      [(assoc ranges x [v b]) (assoc ranges x [a (inc v)])]

      (and (= op "<") (<= b v)) [ranges nil]                ; a b v
      (and (= op "<") (<= v a)) [nil ranges]                ; v a b
      (and (= op "<") (< a v b))                            ; a v b
      [(assoc ranges x [a v]) (assoc ranges x [(dec v) b])]

      :else (throw (ex-info "unexpected" {:rng [a b] :map [x op v]})))))

(test/are [rng map out] (= out (map-rng map rng))
                        {"a" [0 4000]} nil [{"a" [0 4000]} nil]

                        {"a" [0 4000]} ["a" "<" 2006] [{"a" [0 2006]} {"a" [2005 4000]}]
                        {"a" [0 1000]} ["a" "<" 2006] [{"a" [0 1000]} nil]
                        {"a" [0 2006]} ["a" "<" 2006] [{"a" [0 2006]} nil]
                        {"a" [4000 5000]} ["a" "<" 2006] [nil {"a" [4000 5000]}]
                        {"a" [2006 5000]} ["a" "<" 2006] [nil {"a" [2006 5000]}]

                        {"a" [0 4000]} ["a" ">" 2006] [{"a" [2006 4000]} {"a" [0 2007]}]
                        {"a" [0 2000]} ["a" ">" 2006] [nil {"a" [0 2000]}]
                        {"a" [0 2006]} ["a" ">" 2006] [nil {"a" [0 2006]}]
                        {"a" [4000 5000]} ["a" ">" 2006] [{"a" [4000 5000]} nil]
                        {"a" [2006 3000]} ["a" ">" 2006] [{"a" [2006 3000]} nil])

(def conj-non-nil ((remove nil?) conj))

(defn map-multiple-ranges
  "Processes sequence of ranges {x: [a b]} through pipe stage [[x op v] dsp]"
  [[mapping dst] ranges]
  (reduce
    (fn [[mapped unmapped] rng]
      (let [[m un-m] (map-rng mapping rng)]
        [(conj-non-nil mapped [dst m]) (conj-non-nil unmapped un-m)]))
    [[] []]
    ranges))

(defn pipe-ranges
  "Processes sequence of ranges {x: [a b]} through a single pipeline"
  [pipe ranges]
  (->>
    (reduce
      (fn [[res unmapped] stage]
        (let [[mapped unmapped] (map-multiple-ranges stage unmapped)]
          [(apply conj res mapped) unmapped]))
      [[] ranges]
      pipe)
    ; Take the result (everything should be mapped).
    ((fn [[res unmapped]]
       (assert (empty? unmapped))
       res))
    ; Group by target stage.
    (reduce
      (fn [res [dst rng]] (update res dst #(vec (conj % rng))))
      {})))

(test/are [pipe ranges res]
  (= res (pipe-ranges (second (parse-pipeline pipe)) ranges))
  "px{a<2006:qkq,m>2090:A,rfg}"

  [{"a" [0 4000] "m" [0 4000]}]

  {"qkq" [{"a" [0 2006], "m" [0 4000]}],
   "A"   [{"a" [2005 4000], "m" [2090 4000]}],
   "rfg" [{"a" [2005 4000], "m" [0 2091]}]})

(defn process-ranges [pipelines res [[stage ranges] & queue]]
  (if (nil? stage)
    res
    (let [pipe (pipelines stage)
          mapped (pipe-ranges pipe ranges)
          res (apply conj res (mapped "A"))
          todo (dissoc mapped "A" "R")]
      (recur pipelines res (concat queue todo)))))

(defn solve2 [input]
  (let [[pipes _] (read-input input)]
    (->> [
          ; Not includes both sides of the interval as (a b).
          ["in" [{"x" [0 4001] "m" [0 4001] "a" [0 4001] "s" [0 4001]}]]
          ]
         (process-ranges pipes [])
         (map #(map (fn [[a b]] (- b a 1)) (vals %)))
         (map #(reduce * %))
         (reduce +))))

(test/are [in out] (= out (time (solve2 in)))
                   "day19/test.txt" 167409079868000
                   "day19/input.txt" 117954800808317)
