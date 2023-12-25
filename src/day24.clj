(ns day24
  (:import [com.microsoft.z3 ArithExpr])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
            [clj-z3.context :as z3]
            [clj-z3.solver :as z3-solver]))

;19, 13, 30 @ -2,  1, -2
(defn parse-trajectory [line]
  (let [[cords vels] (->> (str/split line #" @ ")
                          (map #(str/split % #","))
                          (map #(map str/trim %))
                          (map #(map parse-long %)))
        [x0 y0 z0] cords
        [kx ky kz] vels]
    {:x0 x0 :y0 y0 :z0 z0 :kx kx :ky ky :kz kz}))

(defn at [trj t]
  (let [x (+ (:x0 trj) (* (:kx trj) t))
        y (+ (:y0 trj) (* (:ky trj) t))
        z (+ (:z0 trj) (* (:kz trj) t))]
    [x y z]))

(defn points->2d-line [[[x1 y1 & _z] [x2 y2 & _z]]]
  (let [m (/ (- y2 y1) (- x2 x1))]
    {:a m
     :b (- y1 (* m x1))}))

(defn intercept-2d [tr1 tr2]
  (let [[line1 line2] (->> [tr1 tr2]
                           (map #(map (partial at %) [0 1]))
                           (map points->2d-line))
        d (- (:a line2) (:a line1))]
    (when (not= d 0)
      (let [x (/ (- (:b line1) (:b line2)) d)
            y (+ (* (:a line1) x) (:b line1))]
        [x y]))))

(test/are [l1 l2 res]
  (= res (intercept-2d (parse-trajectory l1) (parse-trajectory l2)))
  "19, 13, 30 @ -2, 1, -2" "18, 19, 22 @ -1, -1, -2" [43/3 46/3]
  "18, 19, 22 @ -1, -1, -2" "12, 31, 28 @ -1, -2, -1" [-6 -5]
  "18, 19, 22 @ -1, -1, -2" "20, 25, 34 @ -2, -2, -4" nil)

(defn read-trajectories [input]
  (with-open [rdr (io/reader (str "assets/" input))]
    (->> (line-seq rdr)
         (map parse-trajectory)
         (vec))))

(defn in-the-past? [tr [x y]]
  (letfn [(cmp [k] (if (pos? (k tr)) < >))]
    (or ((cmp :kx) x (:x0 tr))
        ((cmp :ky) y (:y0 tr)))))

(defn solve1 [input [min max]]
  (let [trajectories (read-trajectories input)]
    (->> (for [t1 trajectories
               t2 trajectories
               :when (neg? (compare (vec (vals t1)) (vec (vals t2))))]
           [t1 t2])
         (map #(conj % (apply intercept-2d %)))
         (remove #(nil? (last %)))
         (remove (fn [[tr1 tr2 [x y]]]
                   (or (in-the-past? tr1 [x y])
                       (in-the-past? tr2 [x y]))))
         (map last)
         (filter (fn [[x y]]
                   (and (<= min x max)
                        (<= min y max))))
         (count))))

(test/are [in area out] (= out (solve1 in area))
                        "day24/test.txt" [7 27] 2
                        "day24/input.txt" [200000000000000 400000000000000] 16665)

(def ^:dynamic *context* nil)

(defn z3= [a b]
  (z3/mkEq *context* a b))

(defn z3+ [a b]
  (z3/mkBVAdd *context* a b))

(defn z3* [a b]
  (z3/mkBVMul *context* a b))

(defn z3> [a b]
  (z3/mkBVUGE *context* a b))

(defn z3i [x]
  (if (string? x)
    (z3/mkBVConst *context* x 64)
    (z3/mkBV *context* x 64)))

(defn z3-solve [& constraints]
  (let [solver (z3/mkSolver *context*)
        status (z3-solver/check solver constraints)]
    (case (.name status)
      "SATISFIABLE" (z3-solver/getModel solver)
      "UNSATISFIABLE" (throw (ex-info "UNSAT" {:core (vec (.getUnsatCore solver))}))
      "UNKNOWN" (throw (ex-info "UNKNOWN" {:reason (.getUnknownReason solver)})))))

(defn z3-eval [model s]
  (.getBigInteger (.eval model s false)))

(defn make-equation [l1 l2]
  (let [t (z3i (str "t-" (random-uuid)))]
    [(z3> t (z3i 0))
     (z3= (z3+ (:x0 l1) (z3* (:kx l1) t)) (z3+ (:x0 l2) (z3* (:kx l2) t)))
     (z3= (z3+ (:y0 l1) (z3* (:ky l1) t)) (z3+ (:y0 l2) (z3* (:ky l2) t)))
     (z3= (z3+ (:z0 l1) (z3* (:kz l1) t)) (z3+ (:z0 l2) (z3* (:kz l2) t)))]))

(defn solve2 [input skip get]
  (let [trajectories (read-trajectories input)]
    (with-bindings {#'*context* (z3/context)}
      (let [rock (->> [:kx :ky :kz :x0 :y0 :z0]
                      (map #(vector % (z3i (name %))))
                      (into {}))
            model (->> trajectories
                       ; TODO We need to find some lines which forms 2 intersecting planes
                       ;  too exhausted to do it algorithmically.
                       (drop skip)
                       (take get)
                       (map #(update-vals % z3i))
                       (mapcat #(make-equation rock %))
                       (apply z3-solve))]
        (->> (select-keys rock [:x0 :y0 :z0])
             (vals)
             (map #(z3-eval model %))
             (reduce +))))))

(test/are [in s t out] (= out (time (solve2 in s t)))
                       "day24/test.txt" 0 5 47
                       "day24/input.txt" 3 3 769840447420960)
