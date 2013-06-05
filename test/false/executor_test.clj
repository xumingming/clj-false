(ns false.executor-test
  (use clojure.test
       false.core)
  (import java.io.StringWriter))

;; ===== test executor =====
(deftest test-add
  (testing "testing +"
    (is (= (execute* [1 2 ADD]) {:stacks [3] :vars {}}))))

(deftest test-substract
  (testing "testing -"
    (is (= (execute* [1 2 SUBSTRACT]) {:stacks [-1] :vars {}}))))

(deftest test-multiply
  (testing "testing *"
    (is (= (execute* [2 2 MULTIPLY]) {:stacks [4] :vars {}}))))

(deftest test-devide
  (testing "testing /"
    (is (= (execute* [4 2 DEVIDE]) {:stacks [2] :vars {}}))))

(deftest test-minus
  (testing "testing _"
    (is (= ((execute* [4 MINUS]) {:stacks [-4] :vars {}})))))

(deftest test-composed-add-devide
  (testing "testing composed + /"
    (is (= (execute* [1 7 ADD 4 DEVIDE]) {:stacks [2] :vars {}}))))

(deftest test-gt?
  (testing "testing >"
    (is (= (execute* [4 2 GT?]) {:stacks [TRUE] :vars {}}))))

(deftest test-eq?
  (testing "testing ="
    (is (= (execute* [4 2 EQ?]) {:stacks [FALSE] :vars {}}))))

(deftest test-logic-and
  (testing "testing &"
    (is (= (execute* [4 2 GT? 3 2 GT? AND?]) {:stacks [TRUE] :vars {}}))
    (is (= (execute* [4 2 GT? 1 2 GT? AND?]) {:stacks [FALSE] :vars {}}))))

(deftest test-logic-or
  (testing "testing |"
    (is (= (execute* [4 2 GT? 1 2 GT? OR?]) {:stacks [TRUE] :vars {}}))))

(deftest test-dup
  (testing "testing $"
    (is (= (execute* [1 DUP]) {:stacks [1 1] :vars {}}))))

(deftest test-swap
  (testing "testing \\"
    (is (= (execute* [1 2 3 SWAP]) {:stacks [1 3 2] :vars {}}))))

(deftest test-DEL
  (testing "testing %"
    (is (= (execute* [1 2 DEL]) {:stacks [1] :vars {}}))))

(deftest test-rotate
  (testing "testing @"
    (is (= (execute* [1 2 3 ROTATE]) {:stacks [2 3 1] :vars {}}))))

(deftest test-copyn
  (testing "testing ø"
    (is (= (execute* [1 2 3 4 2 COPYN]) {:stacks [1 2 3 4 2] :vars {}}))))

(deftest test-assign-var
  (testing "testing :"
    (is (= (execute* [1 \a ASSIGN-VAR]) {:stacks [] :vars {\a 1}}))))

(deftest test-read-var
  (testing "testing ;"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR]) {:stacks [1] :vars {\a 1}}))))

(deftest test-assign-read-add
  (testing "testing : ; +"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR 3 ADD]) {:stacks [4] :vars {\a 1}}))))

(deftest test-apply
  (testing "testing !"
    (is (= (execute* [(subroutine [1 2 ADD]) APPLY]) {:stacks [3] :vars {}}))
    (is (= (execute* [1 (subroutine [1 ADD]) APPLY]) {:stacks [2] :vars {}}))
    (is (= (execute* [1 (subroutine [1 ADD 100 MINUS ADD]) APPLY]) {:stacks [-98] :vars {}}))
    (is (thrown? Exception (execute* [1 (subroutine [1 ADD ADD]) APPLY])))
    (is (= (execute* [1 2 (subroutine [1 ADD ADD]) APPLY])) {:stacks [4] :vars {}})
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR (subroutine [1 ADD]) APPLY]) {:stacks [2] :vars {\a 1}}))))

(deftest test-if
  (testing "testing ?"
    (is (= (execute* [1 1 EQ? (subroutine [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))

  (testing "testing ? with constant true"
    (is (= (execute* [TRUE (subroutine [1 2 ADD]) IF])
           {:stacks [3] :vars {}})))
  
  (testing "testing ? with constant false"
    (is (= (execute* [FALSE (subroutine [1 2 ADD]) IF])
           {:stacks [] :vars {}})))

  (testing "testing ? with assign-var in action"
    (is (= (execute* [1 1 EQ? (subroutine [1 \a ASSIGN-VAR \a READ-VAR 2 ADD]) IF])
           {:stacks [3] :vars {\a 1}})))
  (testing "testing ? again"
    (is (= (execute* [1 2 EQ? (subroutine [1 2 ADD]) IF]) {:stacks [] :vars {}})))

  (testing "testing : ; ?"
    (is (= (execute* [1 \a ASSIGN-VAR \a READ-VAR 1 EQ? (subroutine [1 2 ADD]) IF]) {:stacks [3] :vars {\a 1}})))

  (testing "testing ? again"
    (is (= (execute* [(subroutine [1 1 EQ?]) (subroutine [1 2 ADD]) IF])
           {:stacks [3] :vars {}}))))

(deftest test-while
  (testing "testing #"
    (is (= (execute* [2 \a ASSIGN-VAR (subroutine [\a READ-VAR 0 GT?]) (subroutine [\a READ-VAR 1 SUBSTRACT \a ASSIGN-VAR]) WHILE])
           {:stacks [] :vars {\a 0}})))
  (testing "testing #"
    (is (= (execute* [1 1 \a ASSIGN-VAR (subroutine [\a READ-VAR 0 GT?]) (subroutine [2 ADD \a READ-VAR 1 SUBSTRACT \a ASSIGN-VAR]) WHILE])
           {:stacks [3] :vars {\a 0}}))))

(deftest test-print-int
  (testing "testing ."
    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (execute* [65 PRINT-INT]))
      (is (= "65" (str wr))))))

(deftest test-print-char
  (testing "testing ."
    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (execute* [65 PRINT-CHAR]))
      (is (= "A" (str wr))))))
