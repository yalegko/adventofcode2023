(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn parse-line [line]
  (let [[name conn] (str/split line #" -> " 2)
        connections (str/split conn #", ")
        type ({\% :flop \& :conj} (first name))
        state ({:flop 0 :conj {}} type)
        name (if (#{\% \&} (first name)) (subs name 1) name)]
    {:name name, :type type, :state state :connections connections}))

(test/are [in out]
  (= out (parse-line in))
  "broadcaster -> a, b, c" {:name "broadcaster", :type nil, :state nil, :connections ["a" "b" "c"]}
  "%a -> inv, con" {:name "a", :type :flop, :state 0, :connections ["inv" "con"]}
  "&inv -> a" {:name "inv", :type :conj, :state {}, :connections ["a"]})

(defn read-machine [input]
  (let [machine (with-open [rdr (io/reader (str "assets/" input))]
                  (->> (line-seq rdr)
                       (map parse-line)
                       (reduce #(assoc %1 (:name %2) %2) {})))
        conj-names (set (keep (fn [[k v]] (when (= :conj (:type v)) k)) machine))]
    ; Set initial conj states.
    (reduce
      (fn [machine name]
        (let [conj-input-names
              (keep (fn [[k v]] (when (some #{name} (:connections v)) k)) machine)
              conj-states
              (reduce #(assoc %1 %2 0) {} conj-input-names)]
          (update machine name #(assoc % :state conj-states))))
      machine
      conj-names)))

(defn flip-bit [b] (bit-xor 1 b))

(defmulti eval-gate (fn [gate _input _s] (:type gate)))
(defmethod eval-gate :flop [gate _input s]
  (case s
    1 [gate nil]
    0 (let [new-state (flip-bit (:state gate))]
        [(assoc gate :state new-state) new-state])))
(defmethod eval-gate :conj [gate input s]
  (let [gate (assoc-in gate [:state input] s)
        and-states (reduce-kv #(bit-and %1 %3) 1 (:state gate))]
    [gate (flip-bit and-states)]))
(defmethod eval-gate nil [gate _input s] [gate s])

(test/are [gate from s out]
  (= out (eval-gate gate from s))
  {:type :flop, :state 0} "q" 1 [{:type :flop, :state 0} nil]
  {:type :flop, :state 0} "q" 0 [{:type :flop, :state 1} 1]
  {:type :flop, :state 1} "q" 0 [{:type :flop, :state 0} 0]

  {:type :conj, :state {"q" 0}} "q" 1 [{:type :conj, :state {"q" 1}} 0]
  {:type :conj, :state {"q" 0}} "q" 0 [{:type :conj, :state {"q" 0}} 1]
  {:type :conj, :state {"q" 0 "e" 0}} "q" 1 [{:type :conj, :state {"q" 1 "e" 0}} 1]
  {:type :conj, :state {"q" 1 "e" 0}} "e" 1 [{:type :conj, :state {"q" 1 "e" 1}} 0]
  {:type :conj, :state {"q" 1 "e" 1}} "e" 0 [{:type :conj, :state {"q" 1 "e" 0}} 1]

  {:name "broadcaster", :type nil} "q" 1 [{:name "broadcaster", :type nil} 1]
  {:name "broadcaster", :type nil} "q" 0 [{:name "broadcaster", :type nil} 0])


(defn trigger
  ([machine signals] (trigger machine
                              (update signals 0 inc)
                              [["button" "broadcaster" 0]]))

  ([machine signals [[from to s] & queue]]
   (if (nil? to)
     [machine signals]
     (let [[gate s] (eval-gate (machine to) from s)
           todo (when (some? s) (map #(vector to % s) (:connections gate)))]
       (recur (assoc machine to gate)
              (if (nil? s) signals (update signals s (partial + (count todo))))
              (concat queue todo))))))

(def times 1000)

(defn solve1 [input]
  (let [machine (read-machine input)]
    (->> (range times)
         (reduce (fn [acc _i] (apply trigger acc))
                 [machine {0 0 1 0}])
         (second)
         (reduce-kv #(* %1 %3) 1))))

(test/are [in out] (= out (solve1 in))
                   "day20/test.txt" 32000000
                   "day20/test2.txt" 11687500
                   "day20/input.txt" 896998430)


(defn find-activations [machine targets]
  (loop [i 1 machine machine targets targets]
    (cond
      (every? (fn [[_k v]] (not= 0 v)) targets) targets

      :else
      ; Push button
      (let [[q found] (loop [machine machine
                             targets targets
                             [[from s to] & queue] [["button" 0 "broadcaster"]]]
                        (cond
                          (nil? from) [machine targets]
                          :else
                          (let [targets (if (and (= 1 s) (= 0 (targets from)))
                                          (assoc targets from i)
                                          targets)
                                [gate s] (eval-gate (machine to) from s)
                                todo (when (some? s)
                                       (map #(vector to s %) (:connections gate)))]
                            (recur (assoc machine to gate) targets (concat queue todo)))))]
        (recur (inc i) q found)))))

(defn gcd [x y]
  (if (= 0 y) x (recur y (rem x y))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn solve2 [input]
  (let [machine (read-machine input)]
    (->>
      (find-activations machine {"jf" 0 "bh" 0 "sh" 0 "mz" 0})
      (map second)
      (reduce lcm))))

(test/are [in out] (= out (time (solve2 in)))
                   "day20/input.txt" 236095992539963)

