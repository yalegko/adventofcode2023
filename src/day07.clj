(ns day07
  (:require [clojure.string :as str]
            [clojure.test :as test]))

(def card->order
  (ref {\T \a
        \J \b
        \Q \c
        \K \d
        \A \e}))

(defn count-cards [draw]
  (->> draw
       (group-by identity)
       (map (fn [[_k v]] (count v)))
       (sort >)))

(defn draw->rank [draw]
  (let [[c1 c2 & _others] (count-cards draw)]
    (cond
      (= c1 5) 7                                            ; Five of a kind
      (= c1 4) 6                                            ; Four of a kind
      (and (= c1 3) (= c2 2)) 5                             ; Full house
      (= c1 3) 4                                            ; Three of a kind
      (and (= c1 2) (= c2 2)) 3                             ; Two pair
      (= c1 2) 2                                            ; One pair
      :otherwise 1)))

(test/are [in out] (= out (draw->rank in))
                   "AAAAA" 7
                   "AA8AA" 6
                   "23332" 5
                   "TTT98" 4
                   "23432" 3
                   "A23A4" 2
                   "23456" 1)

(defn draw->hex [draw]
  (->> draw
       (map #(or (card->order %) %))
       (apply str)
       (#(Integer/parseUnsignedInt % 16))))

(test/are [x f y] (f (draw->hex x) (draw->hex y))
                  "33332" > "2AAAA"
                  "77888" > "77788"
                  "KK677" > "KTJJT"
                  "QQQJA" > "T55J5")

(defn parse-draw [rank-f line]
  (let [[draw bid] (str/split line #" ")
        rank (rank-f draw)
        value (draw->hex draw)]
    [rank value (parse-long bid)]))

(defn solve1 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(parse-draw draw->rank %))
         (sort-by (juxt first second))
         (map-indexed (fn [idx, [_rank, _val, bid]] (* (inc idx) bid)))
         (reduce +))))

(test/is (= 6440 (solve1 "day07/test.txt")))
(test/is (= 252295678 (solve1 "day07/input.txt")))

; Change Joker value to the lowest.
(dosync
  (alter card->order assoc \J \1))

(test/are [x f y] (f (draw->hex x) (draw->hex y))
                  "JKKK2" < "QQQQ2"
                  "T55J5" < "QQQJA"
                  "QQQJA" < "KTJJT")

(defn best-joker-rank
  ([draw]
   (let [cards (->> draw
                    (group-by identity)
                    (map (fn [[k v]] [k (count v)]))
                    (into {})
                    )]
     (best-joker-rank draw cards)))

  ([draw cards]
   (let [rank (draw->rank draw)]
     (cond
       (= 7 rank) 7                                         ; Highest possible.
       (= 0 (cards \J 0)) rank
       :else (let [new-cards (update cards \J dec)]
               (->> (for [c "AKQT98765432"]
                      (let [new-draw (str/replace-first draw \J c)]
                        (best-joker-rank new-draw new-cards)))
                    (reduce max)
                    )
               )

       ))))

(test/are [draw rank] (= rank (best-joker-rank draw))
                      "AAAAA" 7 "AJJJJ" 7
                      "QJJQ2" 6 "T55J5" 6
                      "AQAQJ" 5)

(defn solve2 [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map #(parse-draw best-joker-rank %))
         (sort-by (juxt first second))
         (map-indexed (fn [idx, [_rank, _val, bid]] (* (inc idx) bid)))
         (reduce +)
         )))

(test/is (= 5905 (solve2 "day07/test.txt")))
(test/is (= 250577259 (solve2 "day07/input.txt")))