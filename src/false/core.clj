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

(defn func [name pcnt func & {:keys [commands]}]
  {:name name
   :pcnt pcnt
   :func func
   :commands (vec commands)})

(defn custom-func [commands]
  (let [name (str "cf_" (RT/nextID))]
    (func name nil nil :commands commands)))

(defn func?
  "whether x is a FALSE function"
  [x]
  (map? x))

(defn custom-func?
  "whether x is a FALSE function"
  [x]
  (and (func? x) (seq (:commands x))))

(defn same-fn?
  "Whether x and y are the same fn?"
  [x y]
  (= (:name x) (:name y)))

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

(declare execute-custom-func execute*)
(defn __if [context iftest action]
  (let [context (if (custom-func? iftest)
                  (execute-custom-func context iftest)
                  context)
        iftest (if (custom-func? iftest)
                 (peek (:stacks context))
                 iftest)
        context (update-in context [:stacks] #(vec (drop-last %)))]
    (if (= iftest TRUE)
      (execute-custom-func context action)
      context)))

(defn __while [context whiletest action]
  (let [context (if (custom-func? whiletest)
                  (execute-custom-func context whiletest)
                  context)
        evaled-whiletest (if (custom-func? whiletest)
                           (peek (:stacks context))
                           whiletest)
        context (update-in context [:stacks] #(vec (drop-last %)))]
    (if (= evaled-whiletest TRUE)
      (recur (execute-custom-func context action) whiletest action)
      context)))


(defn assign-var [context v n]
  (assoc-in context [:vars n] v))

(defn read-var [context name]
  (assert (contains? (:vars context) name))
  (update-in context [:stacks] conj ((:vars context) name)))

(defn print-int [context i]
  (print i)
  context)

(defn print-char [context ch]
  (print (char ch))
  context)

(defn __read-char [context]
  (let [ch (.read System/in)
        context (update-in context [:stacks] conj ch)]
    context))

(defn __flush [context]
  (flush)
  context)

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
(def ^:const WHILE (func "#" 2 __while))
;; APPLY is just a skeleton: pcnt and func are nil, because
;; the real function is the function applied
(def ^:const APPLY (func "!" 1 nil))
(def ^:const PRINT-INT (func "." 1 print-int))
(def ^:const PRINT-CHAR (func "," 1 print-char))
(def ^:const READ-CHAR (func "^" 0 __read-char))

(defn parse [program]
  (loop [commands []]))


;; ===== stack-based commands execution ======

(defn execute-custom-func
  [context cfn]
  (let [commands (:commands cfn)
        stacks (:stacks context)
        vars (:vars context)]
    (execute* commands stacks vars)))

(defn execute-func
  "Executes the specified function"
  [func context]
  (let [{:keys [stacks vars]} context
        pcnt (:pcnt func)
        [stacks params] (pop-n-stack stacks pcnt)
        [real-func params] (if (same-fn? func APPLY)
                             [(first params) params]
                             [func params])]
    (if (same-fn? func APPLY)
      (execute-custom-func {:stacks stacks :vars vars} real-func)
      (apply (:func real-func) (cons {:stacks stacks :vars vars}
                                     params)))))


(defn execute*
  "Executes commands"
  ([commands]
     (execute* commands [] {}))
  ([commands stacks]
     (execute* commands stacks {}))
  ([commands stacks vars]
     (loop [commands commands
            stacks stacks
            vars vars]
       (if (seq commands)
         (let [command (first commands)
               commands (rest commands)
               [stacks vars] (if  (and (func? command)
                                       (not (custom-func? command)))
                               (let [ret (execute-func command {:stacks stacks :vars vars})]
                                 [(:stacks ret) (:vars ret)])
                               [(conj stacks command) vars])]
           (recur commands stacks vars))
         {:stacks stacks :vars vars}))))


(defn execute [commands]
  (let [ret (execute* commands)]
    (first (:stacks ret))))

