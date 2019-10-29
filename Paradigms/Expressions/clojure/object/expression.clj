(defn constructor [ctor methods]
  (fn [& args] (apply (partial ctor {:methods methods}) args)))

(defn evaluate [this args] (((this :methods) :evaluate) this args))
(defn toString [this] (((this :methods) :toString) this))
(defn diff [this var] (((this :methods) :diffImpl) this var))

(defn ConstConstructor [this value]
  (assoc this :value value))

(defn VarConstructor [this name]
  (assoc this :name name))

(def CONSTANTS)
(def Constant (constructor ConstConstructor
                           {:evaluate (fn [this _] (this :value))
                            :toString (fn [this] (str (format "%.1f" (this :value))))
                            :diffImpl (fn [_ _] (CONSTANTS :ZERO))}))

(def CONSTANTS {:ZERO (Constant 0.0)
                :ONE  (Constant 1.0)
                :TWO  (Constant 2.0)})

(def Variable (constructor VarConstructor
                           {:evaluate (fn [this args] (args (this :name)))
                            :toString (fn [this] (this :name))
                            :diffImpl (fn [this var]
                                        (if (= (this :name) var)
                                          (CONSTANTS :ONE)
                                          (CONSTANTS :ZERO)))}))

(defn OperationConstructor [this & operands]
  (assoc this :operands operands))

(defn createOperator [f name diff]
  (constructor OperationConstructor
               {:evaluate (fn [this args] (apply f (map (fn [x] (evaluate x args)) (this :operands))))
                :toString (fn [this] (str
                                       "("
                                       (clojure.string/join
                                         " "
                                         (cons name (map (fn [x] (toString x)) (this :operands))))
                                       ")"))
                :diffImpl (fn [this var] (diff (this :operands) var))}))

(def Add
  (createOperator
    +
    "+"
    (fn [exp var] (apply Add (map (fn [x] (diff x var)) exp)))))

(def Subtract
  (createOperator
    -
    "-"
    (fn [exp var] (apply Subtract (map (fn [x] (diff x var)) exp))))
  )

(def Multiply
  (createOperator
    *
    "*"
    (fn [exp var]
      (cond
        (= (count exp) 1) (diff (first exp) var)
        :else (reduce (fn [a b] (Add (Multiply (diff a var) b) (Multiply a (diff b var)))) exp)))))

(def Divide
  (createOperator
    (fn [x & ys] (/ x (double (reduce * ys))))
    "/"
    (fn [exp var]
      (cond
        (= (count exp) 1) (diff (first exp) var)
        :else (Divide
                (Subtract
                  (Multiply
                    (diff (first exp) var)
                    (second exp))
                  (Multiply
                    (first exp)
                    (diff (apply Multiply (rest exp)) var)))
                (apply Multiply (concat (rest exp) (rest exp))))))))

(def Negate
  (createOperator
    -
    "negate"
    (fn [exp var] (Negate (diff (first exp) var)))))

(def Square
  (createOperator
    (fn [x] (* x x))
    "square"
    (fn [exp var] (diff (Multiply (first exp) (first exp)) var))))

(def Sign
  (createOperator
    (fn [x] (Math/signum (double x)))
    "signum"
    (fn [_ _] (CONSTANTS :ZERO))))

(def Sqrt
  (createOperator
    (fn [x] (Math/sqrt (Math/abs (double x))))
    "sqrt"
    (fn [exp var]
      (let [arg (first exp)]
        (Multiply
          (Divide
            (CONSTANTS :ONE)
            (Multiply
              (CONSTANTS :TWO)
              (Sqrt arg)))
          (diff arg var)
          (Sign arg))))))

(def OPERATORS {'+ Add '- Subtract '* Multiply '/ Divide 'negate Negate 'square Square 'sqrt Sqrt})

(defn parse-exp [expression]
  (cond
    (number? expression) (Constant expression)
    (symbol? expression) (Variable (str expression))
    (list? expression) (apply (OPERATORS (first expression)) (map parse-exp (rest expression)))))

(def parseObject (comp parse-exp read-string))