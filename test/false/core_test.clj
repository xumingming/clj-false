(ns false.core-test
  (:use clojure.test
        false.core)
  (import java.io.StringWriter))

(deftest test-parser
  (testing "test basic parsing"
    (is (= (parse "1 2 +") [1 2 ADD]))
    (is (= (parse "1 2+") [1 2 ADD])))

  (testing "test parse string"
    (is (= (parse "\"hello\"") ["hello"])))

  (testing "test parse comments"
    (is (= (parse "{this is comments}1 2 +") [1 2 ADD])))

  (testing "test parse char"
    (is (= (parse "'A") [65])))

  (testing "test parse var"
    (is (= (parse "a") [\a])))

  (testing "test parse var"
    (is (= (parse "abc") [\a \b \c])))

  (testing "test parse var"
    (is (= (parse "1 a:") [1 \a ASSIGN-VAR])))

  (testing "test parse subroutine"
    (let [commands (parse "[1 2+]")]
      (is (= 1 (count commands)))
      (is (map? (first commands)))
      (let [sub-commands (:commands (first commands))]
        (is (= 3 (count sub-commands)))
        (is (= 1 (first sub-commands)))
        (is (= 2 (second sub-commands)))
        (is (map? (last sub-commands)))))))

(deftest test-executor
  (testing "testing +"
    (is (= (execute* [1 2 ADD]) {:stacks [3] :vars {}})))

  (testing "testing -"
    (is (= (execute* [1 2 SUBSTRACT]) {:stacks [-1] :vars {}})))

  (testing "testing *"
    (is (= (execute* [2 2 MULTIPLY]) {:stacks [4] :vars {}})))

  (testing "testing /"
    (is (= (execute* [4 2 DEVIDE]) {:stacks [2] :vars {}})))

  (testing "testing _"
    (is (= ((execute* [4 MINUS]) {:stacks [-4] :vars {}}))))

  (testing "testing composed + /"
    (is (= (execute* [1 7 ADD 4 DEVIDE]) {:stacks [2] :vars {}})))

  (testing "testing >"
    (is (= (execute* [4 2 GT?]) {:stacks [TRUE] :vars {}})))

  (testing "testing ="
    (is (= (execute* [4 2 EQ?]) {:stacks [FALSE] :vars {}})))

  (testing "testing &"
    (is (= (execute* [4 2 GT? 3 2 GT? AND?]) {:stacks [TRUE] :vars {}})))

  (testing "testing & again"
    (is (= (execute* [4 2 GT? 1 2 GT? AND?]) {:stacks [FALSE] :vars {}})))

  (testing "testing |"
    (is (= (execute* [4 2 GT? 1 2 GT? OR?]) {:stacks [TRUE] :vars {}})))

  (testing "testing $"
    (is (= (execute* [1 DUP]) {:stacks [1 1] :vars {}})))

  (testing "testing \\"
    (is (= (execute* [1 2 3 SWAP]) {:stacks [1 3 2] :vars {}})))
 
  (testing "testing %"
    (is (= (execute* [1 2 DEL]) {:stacks [1] :vars {}})))

  (testing "testing @"
    (is (= (execute* [1 2 3 ROTATE]) {:stacks [2 3 1] :vars {}})))

  (testing "testing ø"
    (is (= (execute* [1 2 3 4 2 COPYN]) {:stacks [1 2 3 4 2] :vars {}})))

  (testing "testing :"
    (is (= (execute* [1 \a ASSIGN-VAR]) {:stacks [] :vars {\a 1}})))

  (testing "testing ;"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR]) {:stacks [1] :vars {\a 1}})))
  
  (testing "testing : ; +"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR 3 ADD]) {:stacks [4] :vars {\a 1}})))

  (testing "testing !"
    (is (= (execute* [(custom-func [1 2 ADD]) APPLY]) {:stacks [3] :vars {}})))

  (testing "testing ! again"
    (is (= (execute* [1 (custom-func [1 ADD]) APPLY]) {:stacks [2] :vars {}})))

  (testing "testing ! again again"
    (is (= (execute* [1 (custom-func [1 ADD 100 MINUS ADD]) APPLY]) {:stacks [-98] :vars {}})))

  (testing "testing ! throws Exception when there is not enough value on stack for subroutine"
    (is (thrown? Exception (execute* [1 (custom-func [1 ADD ADD]) APPLY]))))

  (testing "testing ! runs ok when there is enough value on stack"
    (is (= (execute* [1 2 (custom-func [1 ADD ADD]) APPLY])) {:stacks [4] :vars {}}))
  
  (testing "testing : ; !"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR (custom-func [1 ADD]) APPLY]) {:stacks [2] :vars {\a 1}})))

  (testing "testing ?"
    (is (= (execute* [1 1 EQ? (custom-func [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))

  (testing "testing ? with constant true"
    (is (= (execute* [TRUE (custom-func [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))
  
  (testing "testing ? with constant true"
    (is (= (execute* [FALSE (custom-func [1 2 ADD]) IF])
           {:stacks [] :vars {}})))
  
  (testing "testing ? with assign-var in action"
    (is (= (execute* [1 1 EQ? (custom-func [1 \a ASSIGN-VAR \a READ-VAR 2 ADD]) IF])
           {:stacks [3] :vars {\a 1}})))
  
  (testing "testing ? again"
    (is (= (execute* [1 2 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [] :vars {}})))  

  (testing "testing : ; ?"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR 1 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [3] :vars {\a 1}})))

  (testing "testing ? again"
    (is (= (execute* [(custom-func [1 1 EQ?]) (custom-func [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))

  (testing "testing #"
    (is (= (execute* [2 \a ASSIGN-VAR (custom-func [\a READ-VAR 0 GT?]) (custom-func [\a READ-VAR 1 SUBSTRACT \a ASSIGN-VAR]) WHILE])
           {:stacks [] :vars {\a 0}})))

  (testing "testing #"
    (is (= (execute* [1 1 \a ASSIGN-VAR (custom-func [\a READ-VAR 0 GT?]) (custom-func [2 ADD \a READ-VAR 1 SUBSTRACT \a ASSIGN-VAR]) WHILE])
           {:stacks [3] :vars {\a 0}})))

  (testing "testing ."
    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (execute* [65 PRINT-INT]))
      (is (= "65" (str wr)))))

  (testing "testing ."
    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (execute* [65 PRINT-CHAR]))
      (is (= "A" (str wr)))))  
  )


(deftest test-run
  (testing "test basic run"
    (is (= (run "1 2 +") 3))
    (is (= (run "1 2+") 3))

    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (run "\"hello\""))
      (is (= "hello" (str wr))))))