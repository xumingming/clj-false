(ns false.core
  (:import [clojure.lang RT]))

;; ===== FALSE Reader related =====
(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))

(defprotocol Reader
  (read-char [this] "read the next char."))

(deftype FalseReader [program ^:unsynchronized-mutable idx]
  Reader
  (read-char [this]
    (assert (< idx (count program)))
    (let [ret (char (nth program idx))]
      (update! idx inc)
      ret)))

(defn reader [program]
  (FalseReader. program 0))

(defn read-error [reader msg]
  (throw (RuntimeException. (str msg ", idx: " (:idx reader)))))

(defn escape-char [&_]
  \")

(defn read-string*
  [reader]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (case ch
      nil (read-error reader "EOF while reading string")
      \\ (recur (doto sb (.append (escape-char sb reader)))
                (read-char reader))
      \" (str sb)
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn read-number
  [reader]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (cond
     (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} ch)
     (recur (.append sb ch) (read-char reader))

     :else (Integer/valueOf (.toString sb)))))

(defn func [name pcnt func & {:keys [custom?]}]
  {:name name
   :pcnt pcnt
   :func func
   :custom? (boolean custom?)})

(defn func?
  "whether x is a FALSE function"
  [x]
  (map? x))

(defn custom-func?
  "whether x is a FALSE function"
  [x]
  (and (func? x) (:custom? x)))

(defn same-fn?
  "Whether x and y are the same fn?"
  [x y]
  (= (:name x) (:name y)))

(defn get-custom-func-pcnt [commands]
  (let [[func idx] (loop [idx 0]
                     (if (func? (nth commands idx))
                       [(nth commands idx) idx]
                       (recur (inc idx))))]
    (- (:pcnt func) idx)))

(declare execute*)
(defn mk-custom-func
  "Makes the custom function."
  [commands]
  (fn [context & params]
    (let [commands (concat params commands)]
      (execute* commands (:vars context)))))

(defn custom-func [commands]
  (let [name (str "cf_" (RT/nextID))
        pcnt (get-custom-func-pcnt commands)
        afn (mk-custom-func commands)]
    (func name pcnt afn :custom? true)))

(def ^:const TRUE -1)
(def ^:const FALSE 0)
;; 'and' in FALSE
(defn- __and [a b]
  (= a b TRUE))

;; 'or' in FALSE
(defn- __or [a b]
  (or (= a TRUE) (= b TRUE)))

;; 'not' in FALSE
(defn- __not [a]
  (if (= a TRUE) FALSE TRUE))

(defn __if [context iftest action]
  (let [iftest (if (custom-func? iftest)
                 (first (:stacks ((:func iftest) (:vars context))))
                 iftest)
        ret (when (= iftest TRUE)
              ((:func action) (:vars context)))]
    (update-in context [:stacks] #(vec (concat % (:stacks ret))))))

(defn assign-var [context v n]
  (assoc-in context [:vars n] v))

(defn read-var [context name]
  (assert (contains? (:vars context) name))
  (update-in context [:stacks] conj ((:vars context) name)))

(defn print-int [i]
  (print i))

(defn pop-n-stack
  "Pops n stack frames from top.

  e.g. (pop-n-stack [1 2 3] 2) => [[1] [2 3]]"
  [stacks n]
  (loop [stacks stacks poped-stacks []]
    (if (< (count poped-stacks) n)
      (do
        (recur (pop stacks) (conj poped-stacks (peek stacks))))
      [stacks (reverse poped-stacks)])))

(defn pop-one-stack
  "Pops the stack top.

  e.g. (pop-one-stack [1 2 3]) => [[1 2] 3]"
  [stacks]
  (let [[stacks [stack]] (pop-n-stack stacks 1)]
    [stacks stack]))

(defn dup-top-stack
  "Duplicates stack top.

  e.g. (dup-top-stack [1 2 3]) => [1 2 3 3]"
  [context]
  (update-in context [:stacks] conj (peek (:stacks context))))

(defn del-top-stack
  "Deletes stack top.

  e.g. (del-top-stack [1 2 3]) => [1 2]"
  [context]
  (update-in context [:stacks] #(vec (drop-last %))))

(defn rotate-3rd-stack
  "Rotate third stack frame to stack top

  e.g. (rotate-3rd-stack [1 2 3]) => [2 3 1]"
  [context]
  (assert (> (count (:stacks context)) 2))
  (let [stacks (:stacks context)
        [stacks poped-stacks] (pop-n-stack stacks 3)
        stacks (conj stacks (second poped-stacks)
                     (last poped-stacks)
                     (first poped-stacks))]
    {:stacks stacks
     :vars (:vars context)}))

(defn copy-nth-stack
  "Copy the nth stack frame to stack's top

  e.g. (copy-nth-stack [1 2 3] 1) => [1 2 3 2]
  "
  [context n]
  (assert (> (count (:stacks context)) n))
  (let [stacks (:stacks context)
        copy-stack (nth stacks (- (count stacks) n 1))]
    (update-in context [:stacks] conj copy-stack)))

(defn- __add [context x y]
  (update-in context [:stacks] conj (+ x y)))

(defn- __substract [context x y]
  (update-in context [:stacks] conj (- x y)))

(defn- __multiply [context x y]
  (update-in context [:stacks] conj (* x y)))

(defn- __devide [context x y]
  (update-in context [:stacks] conj (/ x y)))

(defn- __minus [context x]
  (update-in context [:stacks] conj (- x)))

(defn- __eq? [context x y]
  (update-in context [:stacks] conj (if (= x y) TRUE FALSE)))

(defn- __gt? [context x y]
  (update-in context [:stacks] conj (if (> x y) TRUE FALSE)))

(defn- __and? [context x y]
  (update-in context [:stacks] conj (if (__and x y) TRUE FALSE)))

(defn- __or? [context x y]
  (update-in context [:stacks] conj (if (__or x y) TRUE FALSE)))

(defn- __not? [context x]
  (update-in context [:stacks] conj (if (__not x) TRUE FALSE)))

;; all the functions in FALSE
(def ^:const ADD (func "+" 2 __add))
(def ^:const SUBSTRACT (func "-" 2 __substract))
(def ^:const MULTIPLY (func "*" 2 __multiply))
(def ^:const DEVIDE (func "/" 2 __devide))
(def ^:const MINUS (func "_" 1 __minus))
;; -1 means true, 0 means false
(def ^:const EQ? (func "=" 2 __eq?))
(def ^:const GT? (func ">" 2 __gt?))
(def ^:const AND? (func "&" 2 __and?))
(def ^:const OR? (func "|" 2 __or?))
(def ^:const NOT? (func "|" 1 __not?))

(def ^:const DUP (func "$" 0 dup-top-stack))
(def ^:const DEL (func "%" 0 del-top-stack))
(def ^:const ROTATE (func "@" 0 rotate-3rd-stack))
(def ^:const COPYN (func "ø" 1 copy-nth-stack))
(def ^:const ASSIGNVAR (func ":" 2 assign-var))
(def ^:const READVAR (func ";" 1 read-var))
(def ^:const IF (func "?" 2 __if))
;; APPLY is just a skeleton: pcnt and func are nil, because
;; the real function is the function applied
(def ^:const APPLY (func "!" nil nil))

(defn parse [program]
  (loop [commands []]))


;; ===== stack-based commands execution ======

(defn execute-func
  "Executes the specified function"
  [func context]
  (let [{:keys [stacks vars]} context
        pcnt (if (same-fn? func APPLY)
               (:pcnt (peek stacks))
               (:pcnt func))
        stacks-to-pop (if (= func APPLY)
                        (inc pcnt)
                        pcnt)
        [stacks params] (pop-n-stack stacks stacks-to-pop)
        [real-func params] (if (same-fn? func APPLY)
                             [(last params) (drop-last params)]
                             [func params])
        real-func (if (same-fn? func APPLY)
                    (update-in real-func [:func] (fn [orig-fn]
                                                   (fn [context & params]
                                                     (let [ret (apply orig-fn (cons context params))
                                                           sub-stacks (:stacks ret)]
                                                       (assoc-in context [:stacks] (vec (concat (:stacks context) sub-stacks)))))))
                    real-func)]
    (apply (:func real-func) (cons {:stacks stacks :vars vars}
                                   params))))

(defn execute*
  "Executes commands"
  ([commands]
     (execute* commands {}))
  ([commands vars]
     (loop [commands commands
            stacks []
            vars vars]
       (if (seq commands)
         (let [command (first commands)
               commands (rest commands)
               [stacks vars] (if  (and (func? command)
                                            (not (:custom? command)))
                                    (let [ret (execute-func command {:stacks stacks :vars vars})]
                                      [(:stacks ret) (:vars ret)])
                                    [(conj stacks command) vars])]
           (recur commands stacks vars))
         {:stacks stacks :vars vars}))))


(defn execute [commands]
  (let [ret (execute* commands)]
    (first (:stacks ret))))

