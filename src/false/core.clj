(ns false.core)

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

;; represents all the FALSE functions
(defrecord Func [name pcnt func stack-func?])

(defn func [name pcnt func & {:keys [stack-func?]}]
  (Func. name pcnt func (boolean stack-func?)))

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

(defn assign-var [variables n v]
  (assoc variables n v))

(defn read-var [variables name]
  (assert (contains? variables name))
  (variables name))

(defn print-int [i]
  (print i))

(defn mk-custom-func [commands]
  (fn [variables & params]
    (let [commands (concat params commands)]
      (execute* commands variables))))

(defn apply-custom-func [func params variables]
  (apply func (cons variables params)))

(declare dup-top-stack del-top-stack
         rotate-3rd-stack copy-nth-stack)
(def ^:const ADD (func "+" 2 +))
(def ^:const SUBSTRACT (func "-" 2 -))
(def ^:const MULTIPLY (func "*" 2 *))
(def ^:const DEVIDE (func "/" 2 /))
(def ^:const MINUS (func "_" 1 -))
;; -1 means true, 0 means false
(def ^:const EQ? (func "=" 2 #(if (= % %2) -1 0)))
(def ^:const NOT-EQ? (func "=~" 2 #(if (not= % %2) -1 0)))
(def ^:const GT? (func ">" 2 #(if (> % %2) -1 0)))
(def ^:const NOT-GT? (func ">~" 2 #(if-not (> % %2) -1 0)))
(def ^:const AND? (func "&" 2 #(if (__and % %2) -1 0)))
(def ^:const OR? (func "|" 2 #(if (__or % %2) -1 0)))
(def ^:const NOT? (func "|" 1 #(if (__not %) -1 0)))
(def ^:const DUP (func "$" 0 dup-top-stack :stack-func? true))
(def ^:const DEL (func "%" 0 del-top-stack :stack-func? true))
(def ^:const ROTATE (func "@" 0 rotate-3rd-stack :stack-func? true))
(def ^:const COPYN (func "ø" 1 copy-nth-stack :stack-func? true))
(def ^:const ASSIGN (func ":" 2 assign-var))
(def ^:const READVAR (func ";" 1 read-var))
(def ^:const APPLY (func "!" 2 apply-custom-func))


;; ====== parse the FALSE code into commands =====
(defn parse [program]
  (loop [commands []]))


;; ===== stack-based commands execution ======
(defn pop-n-stack [stacks n]
  (loop [stacks stacks poped-stacks []]
    (if (< (count poped-stacks) n)
      (do
        (println "hello: " (pop stacks) (conj poped-stacks (peek stacks)))
        (recur (pop stacks) (conj poped-stacks (peek stacks))))
      [stacks (reverse poped-stacks)])))

(defn pop-one-stack
  [stacks]
  (let [[stacks [stack]] (pop-n-stack stacks 1)]
    [stacks stack]))

(defn dup-top-stack
  [stacks]
  (conj stacks (peek stacks)))

(defn del-top-stack
  [stacks]
  (vec (drop-last stacks)))

(defn rotate-3rd-stack
  [stacks]
  (assert (> (count stacks) 2))
  (let [[stacks poped-stacks] (pop-n-stack stacks 3)
        stacks (conj stacks (last poped-stacks)
                     (first poped-stacks)
                     (second poped-stacks))]
    stacks))

(defn copy-nth-stack
  [stacks n]
  (assert (> (count stacks) n))
  (let [copy-stack (nth stacks (- (count stacks) n 1))]
    (conj stacks copy-stack)))

(defn execute-func [func stacks-and-variables]
  (println "execute-func...")
  (let [{:keys [stacks variables]} stacks-and-variables
        [stacks params] (pop-n-stack stacks (:pcnt func))
        _ (println "params: " params)
        stacks (cond
                (= ":" (:name func)) stacks
                (= ";" (:name func)) (conj stacks (apply read-var (cons variables params)))
                (= "!" (:name func))
                (let [params (into [] (for [param params]
                                        (if (and (= Character (type param))
                                                 (>= (int param) 97)
                                                 (<= (int param) 122))
                                          (variables param)
                                          param)))]
                  (conj stacks (apply-custom-func (last params) (drop-last params) variables)))
                (:stack-func? func) (apply (:func func) (cons stacks params))
                :else (conj stacks (apply (:func func) params)))
        variables (if (= ":" (:name func))
                    (apply assign-var (cons variables (reverse params)))
                    variables)]
    {:stacks stacks :variables variables}))

(defn execute*
  "Executes commands"
  ([commands]
     (execute* commands {}))
  ([commands variables]
     (loop [commands commands
            stacks []
            variables variables]
       (if (seq commands)
         (let [command (first commands)
               commands (rest commands)
               {:keys [stacks variables]} (if (= Func (type command))
                                            (execute-func command {:stacks stacks :variables variables})
                                            {:stacks (conj stacks command)
                                             :variables variables})]
           (recur commands stacks variables))
         {:stacks stacks :varaibles variables}))))


(defn execute [commands]
  (let [ret (execute* commands)]
    (first (:stacks ret))))

(comment
  (read-string* (reader "hello\"adlfjl"))
(read-number (reader "123"))
(pop-n-stack [1 2 (:add valid-functions)] 2)
(execute [1 2 ADD])
(execute [1 2 SUBSTRACT])
(execute [2 2 MULTIPLY])
(execute [4 2 DEVIDE])
(execute [4 MINUS])
(execute [1 2 ADD 4 DEVIDE])
(execute [4 2 GT?])
(execute [4 2 NOT-GT?])
(execute [4 2 EQ?])
(execute [1 2 EQ?])
(execute [4 2 NOT-EQ?])
(execute [4 2 GT? 3 2 GT? AND?])
(execute [4 2 GT? 1 2 GT? AND?])
(execute [4 2 GT? 1 2 GT? OR?])
(execute* [1 DUP])
(execute* [1 2 DEL])
(execute* [1 2 3 4 ROTATE])
(execute* [1 2 3 4 2 COPYN])
(execute* [1 \a ASSIGN])
(execute* [1 \a ASSIGN \a READVAR])
(execute* [1 \a ASSIGN \a READVAR 3 ADD])
(mk-custom-func [1 2 ADD] {})
((mk-custom-func [1 ADD] {}) 1)
(execute [1 (mk-custom-func [1 ADD]) APPLY])
(execute [1 \a ASSIGN \a (mk-custom-func [1 ADD]) APPLY])
)
