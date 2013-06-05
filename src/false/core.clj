(ns false.core
  (:import [clojure.lang RT])
  (:require [clojure.string :as str]))

;; ===== FALSE Reader related =====
(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))

(defprotocol Reader
  (read-char [this] "read the next char.")
  (unread-char [this ch] "unread the char")
  (get-program [this] "return the program")
  (get-pos [this] "current pos"))

(def EOF ::EOF)
(defn eof?
  [ch]
  (= EOF ch))

(deftype FalseReader [program ^:unsynchronized-mutable pos buf
                      ^:unsynchronized-mutable buf-pos]
  Reader
  (read-char [this]
    (if (>= buf-pos 0)
      (let [ret (char (aget buf buf-pos))]
        (update! buf-pos dec)
        ret)
      (if (< pos (count program))
        (let [ret (char (nth program pos))]
          (update! pos inc)
          ret)
        EOF)))
  (unread-char [this ch]
    (update! buf-pos inc)
    (aset buf buf-pos ch))
  (get-pos [this] pos)
  (get-program [this] program))

(defn false-reader [program]
  (FalseReader. program 0 (object-array 10) -1))


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
  (boolean (and (func? x) (seq (:commands x)))))

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

(defn print-string [context str]
  (print str)
  context)

(defn read-char-from-stdin [context]
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

(defn swap-top-2-stack
  "Deletes stack top.

  e.g. (swap-top-2-stack [1 2 3]) => [1 3 2]"
  [context]
  (let [stacks (:stacks context)
        [stacks poped-stacks] (pop-n-stack stacks 2)
        stacks (conj stacks (second poped-stacks)
                     (first poped-stacks))]
    {:stacks stacks
     :vars (:vars context)}))

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
(def ADD (func "+" 2 __add))
(def SUBSTRACT (func "-" 2 __substract))
(def MULTIPLY (func "*" 2 __multiply))
(def DEVIDE (func "/" 2 __devide))
(def MINUS (func "_" 1 __minus))
;; -1 means true, 0 means false
(def EQ? (func "=" 2 __eq?))
(def GT? (func ">" 2 __gt?))
(def AND? (func "&" 2 __and?))
(def OR? (func "|" 2 __or?))
(def NOT? (func "~" 1 __not?))

(def DUP (func "$" 0 dup-top-stack))
(def DEL (func "%" 0 del-top-stack))
(def SWAP (func "\\" 0 swap-top-2-stack))
(def ROTATE (func "@" 0 rotate-3rd-stack))
(def COPYN (func "ø" 1 copy-nth-stack))
(def ASSIGN-VAR (func ":" 2 assign-var))
(def READ-VAR (func ";" 1 read-var))
(def IF (func "?" 2 __if))
(def WHILE (func "#" 2 __while))
;; APPLY is just a skeleton: pcnt and func are nil, because
;; the real function is the function applied
(def APPLY (func "!" 1 nil))
(def PRINT-INT (func "." 1 print-int))
(def PRINT-CHAR (func "," 1 print-char))
(def READ-CHAR (func "^" 0 read-char-from-stdin))
(def SYS-SYMBOLS
  {\+ ADD
   \- SUBSTRACT
   \* MULTIPLY
   \/ DEVIDE
   \_ MINUS
   \= EQ?
   \> GT?
   \& AND?
   \| OR?
   \~ NOT?
   \$ DUP
   \% DEL
   \\ SWAP
   \@ ROTATE
   \ø COPYN
   \: ASSIGN-VAR
   \; READ-VAR
   \? IF
   \# WHILE
   \! APPLY
   \. PRINT-INT
   \, PRINT-CHAR
   \^ READ-CHAR})

(defn- whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (Character/isWhitespace ^Character ch)))

(defn digit?
  [ch]
  (Character/isDigit ch))

(defn- low-letter?
  [ch]
  (let [int-ch (int ch)]
    (and (>= int-ch 97)
         (<= int-ch 122))))

(defn read-error [reader msg]
  (throw (RuntimeException. (str msg ", program: \"" (get-program reader) "\", pos:" (get-pos reader)))))

(defn read-false-char-as-int [reader _]
  (int (read-char reader)))

(defn read-delimited
  [reader end-del]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (condp = ch
      EOF (read-error reader (str "EOF while reading delimited, end-del: " end-del ))
      end-del (str sb)
      (recur (doto sb (.append ch)) (read-char reader)))))

(defn read-string*
  [reader _]
  (read-delimited reader \"))

(defn read-comments
  [reader _]
  (read-delimited reader \}))

(declare parse*)
(defn read-subroutine
  [reader _]
  (let [sub-commands (parse* reader \])]
    (custom-func sub-commands)))

(defn read-number
  [reader initch]
  (loop [sb (StringBuilder. (str initch))
         ch (read-char reader)]
    (if (eof? ch)
      (Integer/valueOf (.toString sb))
      (if (digit? ch)
        (recur (.append sb ch) (read-char reader))
        (do (unread-char reader ch)
          (Integer/valueOf (.toString sb)))))))

(defn parse*
  [reader end-del]
  (loop [commands []
         ch (read-char reader)]
    (if (= end-del ch)
      commands
      (if (not (nil? (SYS-SYMBOLS ch)))
        (recur (conj commands (SYS-SYMBOLS ch))
               (read-char reader))
        (cond
         (= \" ch)
         (recur (conj commands (read-string* reader ch))
                (read-char reader))

         (= \{ ch)
         (do
           (read-comments reader ch)
           (recur commands
                  (read-char reader)))

         (= \' ch)
         (recur (conj commands (read-false-char-as-int reader ch))
                (read-char reader))

         (low-letter? ch)
         (recur (conj commands ch)
                (read-char reader))
           
         (= \[ ch)
         (recur (conj commands (read-subroutine reader ch))
                (read-char reader))

         (whitespace? ch)
         (recur commands
                (read-char reader))

         (digit? ch)
         (recur (conj commands (read-number reader ch))
                (read-char reader))
         )))))

(defn parse [program]
  (parse* (false-reader program) EOF))

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
               {:keys [stacks vars]} (cond
                                      (and (func? command)
                                           (not (custom-func? command)))
                                      (let [ret (execute-func command {:stacks stacks :vars vars})]
                                        {:stacks (:stacks ret)
                                         :vars (:vars ret)})
                               
                                      (string? command)
                                      (print-string {:stacks stacks :vars vars} command)
                               
                                      :else
                                      {:stacks (conj stacks command)
                                       :vars vars})]
           (recur commands stacks vars))
         {:stacks stacks :vars vars}))))


(defn execute [commands]
  (let [ret (execute* commands)]
    (first (:stacks ret))))


;; ===== compose parser and executor together
(defn run [program]
  (let [commands (parse program)
        result (execute commands)]
    result))


(defn -main [file-path]
  (run (str/trim (slurp file-path))))