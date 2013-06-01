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
(defrecord Func [name pcnt func])

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

(declare dup-top-stack del-top-stack)
(def ^:const ADD (Func. "+" 2 +))
(def ^:const SUBSTRACT (Func. "-" 2 -))
(def ^:const MULTIPLY (Func. "*" 2 *))
(def ^:const DEVIDE (Func. "/" 2 /))
(def ^:const MINUS (Func. "_" 1 -))
;; -1 means true, 0 means false
(def ^:const EQ? (Func. "=" 2 #(if (= % %2) -1 0)))
(def ^:const NOT-EQ? (Func. "=~" 2 #(if (not= % %2) -1 0)))
(def ^:const GT? (Func. ">" 2 #(if (> % %2) -1 0)))
(def ^:const NOT-GT? (Func. ">~" 2 #(if-not (> % %2) -1 0)))
(def ^:const AND? (Func. "&" 2 #(if (__and % %2) -1 0)))
(def ^:const OR? (Func. "|" 2 #(if (__or % %2) -1 0)))
(def ^:const NOT? (Func. "|" 1 #(if (__not %) -1 0)))
(def ^:const DUP (Func. "$" 0 dup-top-stack))
(def ^:const DEL (Func. "%" 0 del-top-stack))



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

(defn execute-func [func stacks]
  (let [[stacks params] (pop-n-stack stacks (:pcnt func))
        stacks (if (seq params)
                 (conj stacks (apply (:func func) params))
                 ((:func func) stacks))]
    stacks))

(defn execute*
  "Executes commands"
  [commands]
  (println "before execute: " commands)
  (loop [commands commands stacks []]
    (if (seq commands)
      (let [command (first commands)
            commands (rest commands) 
            stacks (if (= Func (type command))
                     (execute-func command stacks)
                     (conj stacks command))]
        (recur commands stacks))
      stacks)))

(defn execute [commands]
  (let [stacks (execute* commands)]
    (first stacks)))

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
)
