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

(defn func [name pcnt func & {:keys [stack-func? custom?]}]
  {:name name
   :pcnt pcnt
   :func func
   :stack-func? (boolean stack-func?)
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
  (fn [vars & params]
    (let [commands (concat params commands)]
      (execute* commands vars))))

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

(defn- __if [context iftest action]
  (let [iftest (if (custom-func? iftest)
                 (first (:stacks ((:func iftest) (:vars context))))
                 iftest)
        ret (when (= iftest TRUE)
              ((:func action) (:vars context)))]
    (:stacks ret)))

(defn assign-var [vars n v]
  (assoc vars n v))

(defn read-var [vars name]
  (assert (contains? vars name))
  (vars name))

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
  [stacks]
  (conj stacks (peek stacks)))

(defn del-top-stack
  "Deletes stack top.

  e.g. (del-top-stack [1 2 3]) => [1 2]"
  [stacks]
  (vec (drop-last stacks)))

(defn rotate-3rd-stack
  "Rotate third stack frame to stack top

  e.g. (rotate-3rd-stack [1 2 3]) => [2 3 1]"
  [stacks]
  (assert (> (count stacks) 2))
  (let [[stacks poped-stacks] (pop-n-stack stacks 3)
        stacks (conj stacks (second poped-stacks)
                     (last poped-stacks)
                     (first poped-stacks))]
    stacks))

(defn copy-nth-stack
  "Copy the nth stack frame to stack's top

  e.g. (copy-nth-stack [1 2 3] 1) => [1 2 3 2]
  "
  [stacks n]
  (assert (> (count stacks) n))
  (let [copy-stack (nth stacks (- (count stacks) n 1))]
    (conj stacks copy-stack)))

;; all the functions in FALSE
(def ^:const ADD (func "+" 2 +))
(def ^:const SUBSTRACT (func "-" 2 -))
(def ^:const MULTIPLY (func "*" 2 *))
(def ^:const DEVIDE (func "/" 2 /))
(def ^:const MINUS (func "_" 1 -))
;; -1 means true, 0 means false
(def ^:const EQ? (func "=" 2 #(if (= % %2) -1 0)))
(def ^:const GT? (func ">" 2 #(if (> % %2) -1 0)))
(def ^:const AND? (func "&" 2 #(if (__and % %2) -1 0)))
(def ^:const OR? (func "|" 2 #(if (__or % %2) -1 0)))
(def ^:const NOT? (func "|" 1 #(if (__not %) -1 0)))
(def ^:const DUP (func "$" 0 dup-top-stack :stack-func? true))
(def ^:const DEL (func "%" 0 del-top-stack :stack-func? true))
(def ^:const ROTATE (func "@" 0 rotate-3rd-stack :stack-func? true))
(def ^:const COPYN (func "ø" 1 copy-nth-stack :stack-func? true))
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
        stacks (cond
                (same-fn? func ASSIGNVAR) stacks
                (same-fn? func READVAR) (conj stacks (apply read-var (cons vars params)))
                (same-fn? func IF) (vec (concat stacks (apply __if (cons context params))))
                (same-fn? func APPLY)
                (let [real-func (last params)
                      params (drop-last params)
                      ret (apply (:func real-func) (cons vars params))
                      sub-stacks (:stacks ret)]
                  (vec (concat stacks sub-stacks)))

                (:stack-func? func) (apply (:func func) (cons stacks params))
                
                :else (conj stacks (apply (:func func) params)))
        vars (if (same-fn? func ASSIGNVAR)
                    (apply assign-var (cons vars (reverse params)))
                    vars)]
    {:stacks stacks :vars vars}))

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

