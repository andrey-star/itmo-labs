(defn constructor [ctor methods]
  (fn [& args] (apply (partial ctor {:methods methods}) args)))

(defn evaluate [this args] (((this :methods) :evaluate) this args))
(defn toString [this] (((this :methods) :toString) this))
(defn toStringSuffix [this] (((this :methods) :toStringSuffix) this))
(defn toStringInfix [this] (((this :methods) :toStringInfix) this))
(defn diff [this var] (((this :methods) :diffImpl) this var))

(defn ConstConstructor [this value]
  (assoc this :value value))

(defn VarConstructor [this name]
  (assoc this :name name))

(def CONSTANTS)
(def Constant (constructor ConstConstructor
                           {:evaluate       (fn [this args] (this :value))
                            :toString       (fn [this] (str (format "%.1f" (this :value))))
                            :toStringSuffix toString
                            :toStringInfix  toString
                            :diffImpl       (fn [this var] (Constant 0.0))}))
(def CONSTANTS {:ZERO (Constant 0.0)
                :ONE  (Constant 1.0)
                :TWO  (Constant 2.0)})

(def Variable (constructor VarConstructor
                           {:evaluate       (fn [this args] (args (this :name)))
                            :toString       (fn [this] (this :name))
                            :toStringSuffix toString
                            :toStringInfix toString
                            :diffImpl (fn [this var]
                                        (if (= (this :name) var)
                                          (CONSTANTS :ONE)
                                          (CONSTANTS :ZERO)))}))

(defn Operation [this & operands]
  (assoc this :operands operands))

(defn createOperator [f name diff]
  (constructor Operation
               {:evaluate       (fn [this args] (apply f (map (fn [x] (evaluate x args)) (this :operands))))
                :toString       (fn [this] (str
                                             "("
                                             (clojure.string/join
                                               " "
                                               (cons name (map (fn [x] (toString x)) (this :operands))))
                                             ")"))
                :toStringSuffix (fn [this] (str
                                             "("
                                             (clojure.string/join
                                               " "
                                               (map (fn [x] (toStringSuffix x)) (this :operands)))
                                             " "
                                             name
                                             ")"))
                :toStringInfix  (fn [this]
                                  (if
                                    (= (count (this :operands)) 1)
                                    (str name "(" (toStringInfix (first (this :operands))) ")")
                                    (str
                                      "("
                                      (toStringInfix (first (this :operands)))
                                      " "
                                      name
                                      " "
                                      (toStringInfix (second (this :operands)))
                                      ")")))
                :diffImpl       (fn [this var] (diff (this :operands) var))}))

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

(def Ln
  (createOperator
    #(Math/log (Math/abs %))
    "ln"
    (fn [x] (Divide (Sign x) x))))

(def Pow
  (createOperator
    #(Math/pow %1 %2)
    "**"
    (fn [a b]
      [(Multiply b (Pow a (Subtract b (CONSTANTS :ONE))))
       (Multiply (Pow a b) (Ln a))])))

(def Log
  (createOperator
    #(/ (Math/log (Math/abs %2)) (Math/log (Math/abs %1)))
    "//"
    (fn [a b]
      [(Divide (CONSTANTS :ONE) (Multiply a (Log b)))
       (Divide (Negate (Ln a)) (Multiply b (Multiply (Log b) (Log b))))])))

(def OPERATIONS {"+" Add "-" Subtract "**" Pow "*" Multiply "negate" Negate "//" Log "/" Divide})

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
          ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))
(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (_map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps]
  (reduce (partial _either) p ps))
(defn +opt [p val]
  (+or p (_empty val)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))
(defn +collect [defs]
  (cond
    (empty? defs) ()
    (seq? (first defs)) (let [[[key args body] & tail] defs]
                          (cons
                            {:key key :args args :body body}
                            (+collect tail)))
    :else (let [[key body & tail] defs]
            (cons
              {:key key :args [] :synth true :body body}
              (+collect tail)))))
(defmacro grammar [& defs]
  (let [collected (+collect defs)
        keys (set (map :key (filter :synth collected)))]
    (letfn [(rule [r] `(~(:key r) ~(:args r) ~(convert (:body r))))
            (convert [value]
              (cond
                (seq? value) (map convert value)
                (char? value) `(+char ~(str value))
                (keys value) `(~value)
                :else value))]
      `(letfn ~(mapv rule collected) (+parser (~(:key (last collected))))))))
;

(def +exact-char (comp +char str))
(defn +exact-string [s] (apply +seqf str (map +exact-char s)))
(defn +string-from [coll] (apply +or (map +exact-string coll)))
(defn +item-getter [dict] (+map dict (+string-from (keys dict))))

(def VARIABLE-NAMES ["x" "y" "z"])

(def VARIABLES
  (zipmap
    VARIABLE-NAMES
    (map Variable VARIABLE-NAMES)))

(def +variable (+item-getter VARIABLES))

(def all-chars (mapv char (range 128)))
(def +digit (+char (apply str (filter #(Character/isDigit %) all-chars))))
(def +space (+char (apply str (filter #(Character/isWhitespace %) all-chars))))
(def +ws (+ignore (+star +space)))

(def +pos-integer (+str (+plus +digit)))
(def +pos-double (+seqf str +pos-integer (+exact-char \.) +pos-integer))
(def +number (+seqf str (+opt (+exact-char \-) "") (+or +pos-double +pos-integer)))
(def +constant (+map (comp Constant read-string) +number))

(def parseObjectSuffix
  (grammar
    (*make-op [[args op]] (apply op args))
    *op (+map *make-op (+seq (+ignore \() (+star (delay *ws-expr)) +ws (+item-getter OPERATIONS) +ws (+ignore \))))
    *expr (+or +constant +variable *op)
    *ws-expr (+seqn 0 +ws *expr +ws)))

(def parseObjectInfix
  (grammar
    (*make-unary [[op x]] (op x))
    (*make-binary [[a op b]] (op a b))
    (*collect-left-assoc
      [[x args]]
      (reduce (comp *make-binary cons) x args))
    (*left-assoc [ops prev] (+map *collect-left-assoc (+seq prev (+star (+seq +ws (+item-getter ops) +ws prev)))))
    (*right-assoc
      [ops prev]
      (+or
        (+map *make-binary (+seq prev +ws (+item-getter ops) +ws (delay (*right-assoc ops prev))))
        prev))
    *unary (+or
             (+seqn 1 \( (delay *ws-expr) \))
             (+map *make-unary (+seq (+item-getter {"negate" Negate}) +ws (delay *unary)))
             +variable
             +constant)
    *pow-log (*right-assoc {"//" Log "**" Pow} *unary)
    *mul-div (*left-assoc {"*" Multiply "/" Divide} *pow-log)
    *add-sub (*left-assoc {"+" Add "-" Subtract} *mul-div)
    *ws-expr (+seqn 0 +ws *add-sub +ws)))