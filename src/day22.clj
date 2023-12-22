(ns day22
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-figure [line]
  (let [[[x1 y1 z1]
         [x2 y2 z2]] (->> (str/split line #"~")
                          (map #(str/split % #","))
                          (map #(map parse-long %)))]
    (set
      (for [x (range x1 (inc x2))
            y (range y1 (inc y2))
            z (range z1 (inc z2))]
        [x y z]))))

(test/are [in out] (= out (count (parse-figure in)))
                   "2,2,2~2,2,2" 1
                   "0,0,10~1,0,10" 2
                   "0,0,10~0,1,10" 2
                   "0,0,1~0,0,10" 10)

(defn min-z [figure]
  (->> figure (map last) (reduce (partial min ##Inf))))

(defn max-z [figure]
  (->> figure (map last) (reduce (partial max ##-Inf))))

(defn read-figures [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-figure)
         (sort-by min-z)
         (apply vector))))

(defn drop-one-level [figure]
  (set (map (fn [[x y z]] [x y (dec z)]) figure)))

(defn drop-figure [figures figure]
  (let [occupied? (set/difference (reduce set/union figures) figure)]
    (loop [figure figure]
      (let [dropped (drop-one-level figure)]
        (cond
          (= 1 (min-z figure)) figure
          (some occupied? dropped) figure
          :else (recur dropped))))))

(defn fall
  ([figures] (fall figures 0 0))
  ([figures i felt]
   (let [figure (get figures i)
         lowest-part (min-z figure)
         figs-below (take i figures)]
     (cond
       (nil? figure) [figures felt]
       (= 1 lowest-part) (recur figures (inc i) felt)
       :else
       (let [dropped (drop-figure figs-below figure)]
         (if (= dropped figure)
           (recur figures (inc i) felt)
           (recur (assoc figures i dropped) (inc i) (inc felt))))))))

(defn vec-remove [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn figures-above [figures f]
  (let [row-above (inc (max-z f))]
    (->> figures
         (filter #(= (min-z %) row-above)))))

(defn solve1 [input]
  (let [figures (first (->> input (read-figures) (fall)))]
    (count
      (for [i (range (count figures))
            :let [figs-above (figures-above figures (get figures i))
                  without-f (vec-remove figures i)]
            :when (every? #(= % (drop-figure without-f %)) figs-above)]
        1))))

(test/are [in out] (= out (time (solve1 in)))
                   "day22/test.txt" 5
                   "day22/input.txt" 509)

(defn solve2 [input]
  (let [figures (first (->> input (read-figures) (fall)))]
    (->> (range (count figures))
         (pmap
           (fn [i]
             (second (fall (vec-remove figures i)))))
         (reduce +))))

(test/are [in out] (= out (time (solve2 in)))
                   "day22/test.txt" 7
                   "day22/input.txt" 102770)
