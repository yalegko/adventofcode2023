(ns day21
  (:require [clojure.java.io :as io]
            [clojure.test :as test]))

(defn read-field [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map vec)
         (apply vector))))

(defn at [field [i j]]
  (-> field (get i) (get j)))

(defn adjacent [[x y]]
  (vector [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn bfs
  ([field start steps]
   (bfs field [[start steps]] #{} :recur))

  ([field [[p steps] & queue] visited _dummy]
   (cond
     (nil? p) visited
     (zero? steps) (recur field queue visited _dummy)

     :else
     (let [to-visit (->> p
                         (adjacent)
                         (remove visited)
                         (remove #(contains? #{nil \#} (at field %))))]
       (recur field
              (concat queue (map #(vector % (dec steps)) to-visit))
              (disj (reduce conj visited to-visit) p)
              _dummy)))))

(defn solve1 [input steps]
  (let [field (read-field input)
        n (count field)
        m (count (first field))
        start (first
                (for [i (range n) j (range m)
                      :when (= \S (at field [i j]))] [i j]))]
    (count (bfs field start steps))))

(test/are [in steps out] (= out (time (solve1 in steps)))
                         "day21/test.txt" 6 16
                         "day21/input.txt" 64 3617)

(defn count-visited [field start]
  (loop [visited {}
         [[p steps] & queue] [[start 0]]]
    (cond
      (nil? p) visited
      (contains? visited p) (recur visited queue)
      :else
      (let [to-visit (->> p
                          (adjacent)
                          (remove visited)
                          (remove #(contains? #{nil \#} (at field %)))
                          (map #(vector % (inc steps))))]
        (recur (assoc visited p steps)
               (concat queue to-visit))))))

(defn solve2 [input]
  (let [field (read-field input)
        size (count field)
        start (first
                (for [i (range size) j (range size)
                      :when (= \S (at field [i j]))] [i j]))
        ; So
        ;   1) We can freely traverse up/down and to the sides to the maximum distance of
        ;      (steps/(size/2)) forming a corner-standing square with visited cells.
        ;   2) There are some weird odd-even pattern we can observe in an input saturated
        ;      with steps, e.g. checkout
        ;      `(map-indexed #(vector %1 (solve1 "day21/test.txt" %2)) (range 20))`
        ;
        ;  So the final shape should be something like a following:
        ;
        ;    ^
        ;  /oeo\
        ; <oeoeo>
        ;  \oeo/
        ;    v
        ;
        ; So now we need to count saturated and not-saturated fields in the middle and
        ; on the edges and try to produce some equation.
        ;
        ; After a couple of handwritten pages it looks like
        ;
        ;   (n+1)2*odd-middles + (n)2*even-middles - (n+1)*odd-edges + n*even-edges
        ;
        n (/ (- 26501365 (int (/ size 2))) size)
        times-visited (vals (count-visited field start))
        odd-middles (->> times-visited (filter odd?) (count))
        even-middles (->> times-visited (filter even?) (count))
        odd-edges (->> times-visited (filter #(> % 65)) (filter odd?) (count))
        even-edges (->> times-visited (filter #(> % 65)) (filter even?) (count))]
    (+ (* (* (inc n) (inc n)) odd-middles)
       (* (* n n) even-middles)
       (* -1 (* (inc n) odd-edges))
       (* n even-edges))))

(test/is (= 596857397104703 (time (solve2 "day21/input.txt"))))
