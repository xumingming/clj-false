(ns false.core-test
  (:use clojure.test
        false.core))

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

  (testing "testing : ; !"
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR (custom-func [1 ADD]) APPLY]) {:stacks [2] :vars {\a 1}})))

  (testing "testing ?"
    (is (= (execute* [1 1 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [3] :vars {}})))

  (testing "testing ? again"
    (is (= (execute* [1 2 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [] :vars {}})))  

  (testing "testing : ; ?"
    (is (= (execute* [1 \a ASSIGNVAR \a READVAR 1 EQ? (custom-func [1 2 ADD]) IF]) {:stacks [3] :vars {\a 1}})))

  (testing "testing if again"
    (is (= (execute* [(custom-func [1 1 EQ?]) (custom-func [1 2 ADD]) IF]) {:stacks [3] :vars {}}))))