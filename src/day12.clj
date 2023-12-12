(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-line [line]
  (let [[pattern nums] (str/split line #" ")]
    [pattern
     (vec (map parse-long (str/split nums #",")))]))

(defn valid-line? [pattern nums]
  (let [real-nums (->> pattern
                       (partition-by #{\. \?})
                       (remove #(#{\. \?} (first %)))
                       (map count)
                       (remove zero?))]
    (= nums real-nums)))

(test/are [l n out] (= out (time (valid-line? l n)))
                    ".#.###.#.######" [1, 3, 1, 6] true
                    "#....######..#####." [1, 6, 5] true
                    "#....######...#####" [1, 6, 5] true
                    "##...######..#####." [1, 6, 5] false)

(defn solve-line [pattern nums]
  (loop [queue [pattern]
         num-valid 0]
    (let [[line & rest] queue]
      (cond
        ; Done.
        (empty? queue) num-valid

        ; Enough for this line.
        (nil? (str/index-of line "?"))
        (recur rest
               (if (valid-line? line nums) (inc num-valid) num-valid))

        ; Guess.
        :else
        (let [queue (conj rest
                          (str/replace-first line "?" "#")
                          (str/replace-first line "?" "."))]
          (recur queue num-valid))))))

(test/are [l n out] (= out (time (solve-line l n)))
                    "???.###" [1, 1, 3] 1
                    "?#?#?#?#?#?#?#?" [1, 3, 1, 6] 1
                    "????.######..#####." [1, 6, 5] 4)

(defn solve1 [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-line)
         (map #(apply solve-line %))
         (reduce +))))

(test/are [in out] (= out (time (solve1 in)))
                   "day12/test.txt" 21
                   "day12/input.txt" 6852)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unfold-line [pattern nums]
  [(str/join "?" (repeat 5 pattern)) (vec (mapcat identity (repeat 5 nums)))])

(test/are [l n out] (= out (unfold-line l n))
                    ".#" [1]
                    [".#?.#?.#?.#?.#" [1, 1, 1, 1, 1]]

                    "???.###" [1, 1, 3]
                    ["???.###????.###????.###????.###????.###" [1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3]])

(def solve-line-3
  (memoize
    (fn
      ([line nums] (solve-line-3 line -1 nums))
      ([[c & rest-line] broken [n & rest-nums :as nums]]
       (case c
         ; Processed all the string.
         nil (cond
               (and (nil? n) (= -1 broken)) 1               ; Consumed all groups.
               (and (nil? rest-nums) (= n broken)) 1        ; Last group matched.
               :else 0)                                     ; Left something unmatched.

         ; Facing damaged spring.
         \# (cond
              ; Just starting a damaged group.
              (= -1 broken) (solve-line-3 rest-line 1 nums)
              ; We already have damaged group going -- inc broken counter.
              :else (solve-line-3 rest-line (inc broken) nums))

         ; Facing working spring.
         \. (cond
              ; Damaged group is not started yet -- just consume working and go further.
              (= -1 broken) (solve-line-3 rest-line -1 nums)
              ; We closed a group of damaged springs and need to start new one. Check if we can.
              (= n broken) (solve-line-3 rest-line -1 rest-nums)
              :else 0
              )

         ; Facing unknown spring.
         \? (cond
              ; If we haven't any group going we could guess between both variants.
              (= -1 broken)
              (+ (solve-line-3 rest-line 1 nums)            ; ? -> #
                 (solve-line-3 rest-line -1 nums))          ; ? -> .

              ; If it's time to close damaged group -- the choice is obvious.
              (= n broken)
              (solve-line-3 rest-line -1 rest-nums)         ; ? -> .

              ; Otherwise we have damaged group going and the only choice is to proceed with group.
              :else
              (solve-line-3 rest-line (inc broken) nums))))))) ; ? -> #


(test/are [l n out] (= out (time (solve-line-3 l n)))
                    "???.###" [1, 1, 3] 1
                    "?#?#?#?#?#?#?#?" [1, 3, 1, 6] 1
                    "????.######..#####." [1, 6, 5] 4
                    ".??..??...?##.?.??..??...?##." [1, 1, 3, 1, 1, 3] 32)

(test/are [l n out] (= out (time (apply solve-line-3 (unfold-line l n))))
                    "????.######..#####." [1, 6, 5] 2500
                    ".??..??...?##." [1, 1, 3] 16384
                    "?###????????" [3, 2, 1] 506250)

(defn solve2 [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-line)
         (map #(apply unfold-line %))
         (map #(apply solve-line-3 %))
         (reduce +))))

(test/are [in out] (= out (time (solve2 in)))
                   "day12/test.txt" 525152
                   "day12/input.txt" 8475948826693)
