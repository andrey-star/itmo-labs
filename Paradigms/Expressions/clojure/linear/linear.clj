(defn size-equ? [vs] (apply = (map count vs)))
(defn number-seq? [v] (every? number? v))
(defn test-seq? [ss test] (and (not-empty ss) (every? test ss) (size-equ? ss)))
(defn vector-test [v test] (and (vector? v) (test v)))

(defn num-vector? [v] (vector-test v number-seq?))
(defn vector-seq? [vs] (test-seq? vs num-vector?))

(defn num-matrix? [m] (vector-test m vector-seq?))
(defn matrix-seq? [ms] (test-seq? ms num-matrix?))

(defn tensor-shape [t]
  (if (number? t)
    []
    (if (and (not-empty t) (vector? t))
      (let [shapes (map tensor-shape t)]
        (if (and (apply = shapes) (first shapes))
          (vec (cons (count t) (first shapes)))))
      nil)))
(defn num-tensor? [t] (boolean (tensor-shape t)))
(defn tensor-seq? [ts] (and (not-empty ts) (apply = (map tensor-shape ts)) (num-tensor? (first ts))))
(defn op-test [op test]
  (fn [& vs]
    {:pre [(test vs)]}
    (apply mapv op vs)))

(defn vector-op [op]
  (op-test op vector-seq?))

(def v+ (vector-op +))
(def v- (vector-op -))
(def v* (vector-op *))

(defn v*s ([v & ss]
           {:pre [(num-vector? v) (every? number? ss)]}
           (mapv (partial * (reduce * ss)) v)))
(defn s*v [s v] (v*s v s))

(defn scalar [a b]
  {:pre [(vector-seq? [a b])]}
  (reduce + (v* a b)))

(defn vect2
  ([a b]
   [(- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
    (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
    (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))
    ]))
(defn vect
  [& vs]
  {:pre [(and (vector-seq? vs) (= (count (first vs)) 3))]}
  (reduce vect2 vs))

(defn matrix-op [op]
  (op-test op matrix-seq?))

(def m+ (matrix-op v+))
(def m- (matrix-op v-))
(def m* (matrix-op v*))

(defn m*s [m & ss]
  {:pre [(and (num-matrix? m) (every? number? ss))]}
  (mapv (partial s*v (reduce * ss)) m))

(defn m*v [m v]
  {:pre [(num-matrix? m)]}
  (mapv (partial scalar v) m))
(defn transpose [m] (apply mapv vector m))


(defn matrix-shape [m]
  {:pre [(num-matrix? m)]}
  [(count m) (count (first m))])

(defn m*m2 [a b]
  {:pre [= (last (matrix-shape a)) (first (matrix-shape b))]}
  (transpose (mapv (partial m*v a) (transpose b))))
(defn m*m [& ms]
  {:pre [(and (not-empty ms) (every? num-matrix? ms))]}
  (reduce m*m2 ms))

(defn tensor-op [op]
  (fn [& ts]
    {:pre [(tensor-seq? ts)]}
    (if (number? (first ts)) (apply op ts) (apply mapv (tensor-op op) ts))))

(def t+ (tensor-op +))
(def t- (tensor-op -))
(def t* (tensor-op *))


(defn can-broadcast? [t1 t2]
  (let [s1 (tensor-shape t1) s2 (tensor-shape t2)]
    (and (<= (count s1) (count s2)) (= (subvec s2 (- (count s2) (count s1))) s1))))

(defn broadcast [t1 t2]
  {:pre [(can-broadcast? t1 t2)]}
  (let [s1 (tensor-shape t1) s2 (tensor-shape t2)]
    (if (= (count s1) (count s2))
      t1
      (broadcast (vec (repeat (nth s2 (- (count s2) (count s1) 1)) t1)) t2))))

(defn broadcast-op [op]
  (let [tenzor-op (tensor-op op)]
    (fn operation
      ([a] {:pre [(tensor-shape a)]} (tenzor-op a))
      ([a b] {:pre [(every? tensor-shape [a b])]} (cond
         (can-broadcast? a b) (tenzor-op (broadcast a b) b)
         (can-broadcast? b a) (tenzor-op a (broadcast b a))))
      ([a b & ts] (apply operation (operation a b) ts)))))
(def b+ (broadcast-op +))
(def b- (broadcast-op -))
(def b* (broadcast-op *))
(println (m*m [[1 2][1 2 3]] [[2 2][2 2]]))
