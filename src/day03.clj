(ns day03)

(defn make-field [input]
  (with-open [rdr (clojure.java.io/reader (str "assets/" input))]
    (vec (line-seq rdr))))

(defn field-at [field, i, j]
  (-> field
      (get i)
      (get j)))

(let [field (make-field "day03/test1.txt")]
  (assert (= \4 (field-at field 0 0)))
  (assert (= \. (field-at field 0 9)))
  (assert (= \6 (field-at field 9 1))))

(def digit? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn some-sign? [chr]
  (and
    (not (digit? chr))
    (not (= chr \.))))

(assert (some-sign? \+))
(assert (not (some-sign? \.)))
(assert (not (some-sign? \1)))

(defn find-marked-digits [field]
  (let [n (count field)]
    (->>
      (for [i (range n)
            j (range n)
            :when (some-sign? (field-at field i j))]
        (for [ii [i (+ i 1) (- i 1)]
              jj [j (+ j 1) (- j 1)]
              :when (digit? (field-at field ii jj))]
          [ii, jj]))
      (apply concat)
      (set))))

(defn find-digits [line]
  (->> line
       (map vector (range))
       (partition-by (fn [[_i, c]] (not (digit? c))))
       (filter (fn [group]
                 (let [[_i, c] (first group)]
                   (digit? c))))
       (map vec)))
(assert (= (vec (find-digits "467..114.."))
           [[[0 \4] [1 \6] [2 \7]] [[5 \1] [6 \1] [7 \4]]]))
(assert (empty? (find-digits "...*......")))

(defn reduce->number [digit-group]
  (->> digit-group
       (reduce
         (fn [[cords, number], [pos, digit]]
           [(conj cords pos)
            (+ (* 10 number)
               (Character/digit digit 10))])
         [[], 0])))

(assert (= [[0 1 2] 467] (reduce->number [[0 \4] [1 \6] [2 \7]])))

(defn find-numbers [field]
  (->> field
       (map find-digits)                                    ; Parse to (y, dig)
       (map vector (range))                                 ; Enumerate
       (filter #(not-empty (last %)))                       ; Drop lines w/o nums
       (mapcat                                              ; ([(x,y)], num)
         (fn [[i, num-groups]]
           (->> num-groups
                (map reduce->number)                        ; Transalte digits to num
                (map (fn [[idxs num]]
                       (vec [
                             (map #(vector i, %) idxs)      ; Add row to digit columns
                             num
                             ]))))))))

(defn solve1 [input]
  (let [field (make-field input)
        marked? (find-marked-digits field)]
    (->> field
         (find-numbers)
         (filter (fn [[cords, _num]] (some marked? cords)))
         (map (fn [[_cords, num]] num))
         (reduce +))))

(assert (= 4361 (solve1 "day03/test1.txt")))
(assert (= 521515 (solve1 "day03/input.txt")))

(defn find-on-field [pred? field]
  (let [n (count field)]
    (set
      (for [i (range n)
            j (range n)
            :when (pred? (field-at field i j))]
        [i, j]))))

(defn adjacent [[x, y]]
  (for [i [x (+ x 1) (- x 1)]
        j [y (+ y 1) (- y 1)]
        :when #(not= [i, j] [x, y])]
    [i, j]))

(defn solve2 [input]
  (let [field (make-field input)
        ; Find numbers as before, but convert cords to set.
        numbers (->> field
                     (find-numbers)
                     (map (fn [[cords, num]] [(set cords), num])))]
    (->>
      ; Find gears adjacent to some numbers.  
      (for [gear (find-on-field #(= % \*) field)
            [cords, _num] numbers]
        (when (some #(cords %) (adjacent gear))
          (lazy-seq [gear, _num])))
      ; Somehow it yields nils too, so clean them.
      (remove nil?)
      ; Group by the gear coordinate.
      (group-by #(first %))
      ; Filter those with 2 adjacent numbers.
      (filter (fn [[_gear, adj]] (= 2 (count adj))))
      ; Compute gear ratios multiplying numbers.
      (map (fn [[_gear, nums]]
             (reduce #(* %1 (last %2)) 1 nums)))
      (reduce +)
      )
    ))

(assert (= 467835 (solve2 "day03/test1.txt")))

(solve2 "day03/input.txt")