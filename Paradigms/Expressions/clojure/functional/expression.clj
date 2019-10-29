(defn constant [x] (fn [args] x))
(defn variable [name] (fn [args] (args name)))

(defn operation [f]
  (fn [& args]
    (fn [vars]
      (apply f (map (fn [x] (x vars)) args)))))

(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation (fn [x y] (/ x (double y)))))
(def negate (operation -))

(defn reduce-op [op] (operation (fn [& args] (reduce op args))))
(def min (reduce-op (fn [a b] (Math/min a b))))
(def max (reduce-op (fn [a b] (Math/max a b))))

(def operations {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 'min min, 'max max})

(defn parseExpression [expression]
  (cond
    (seq? expression) (apply (operations (first expression)) (map parseExpression (rest expression)))
    (number? expression) (constant expression)
    :else (variable (str expression))))

(def parseFunction (comp parseExpression read-string))
(defn parseFunction [s] (parseExpression (read-string s)))
