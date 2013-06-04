(ns false.core-test
  (:use clojure.test
        false.core)
  (import java.io.StringWriter))

(deftest test-reader
  (testing "test basic parsing"
    (is (= (parse "1 2 +") [1 2 ADD])))

  (testing "test basic parsing"
   (is (= (parse "\"hello\"") [1 2 ADD]))))

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

  (testing "testing %"
    (is (= (execute* [1 2 DEL]) {:stacks [1] :vars {}})))

  (testing "testing @"
    (is (= (execute* [1 2 3 ROTATE]) {:stacks [2 3 1] :vars {}})))

  (testing "testing ø"
    (is (= (execute* [1 2 3 4 2 COPYN]) {:stacks [1 2 3 4 2] :vars {}})))

  (testing "testing :"
    (is (= (execute* [1 \a ASSIGNVAR]) {:stacks [] :vars {\a 1}})))

  (testing "testing ;"
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR]) {:stacks [1] :vars {\a 1}})))
  
  (testing "testing : ; +"
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR 3 ADD]) {:stacks [4] :vars {\a 1}})))

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
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR (custom-func [1 ADD]) APPLY]) {:stacks [2] :vars {\a 1}})))

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
    (is (= (execute* [1 1 EQ? (custom-func [1 \a ASSIGNVAR \a READVAR 2 ADD]) IF])
           {:stacks [3] :vars {\a 1}})))
  
  (testing "testing ? again"
    (is (= (execute* [1 2 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [] :vars {}})))  

  (testing "testing : ; ?"
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR 1 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [3] :vars {\a 1}})))

  (testing "testing ? again"
    (is (= (execute* [(custom-func [1 1 EQ?]) (custom-func [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))

  (testing "testing #"
    (is (= (execute* [2 \a ASSIGNVAR (custom-func [\a READVAR 0 GT?]) (custom-func [\a READVAR 1 SUBSTRACT \a ASSIGNVAR]) WHILE])
           {:stacks [] :vars {\a 0}})))

  (testing "testing #"
    (is (= (execute* [1 1 \a ASSIGNVAR (custom-func [\a READVAR 0 GT?]) (custom-func [2 ADD \a READVAR 1 SUBSTRACT \a ASSIGNVAR]) WHILE])
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
        (run "hello"))
      (is (= "hello" (str wr))))))