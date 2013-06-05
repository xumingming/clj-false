(ns false.parser-test
  (use clojure.test
       false.core))

(deftest test-number
    (testing "test parse number"
      (is (= (parse "1") [1]))))

(deftest test-add
  (testing "test basic parsing"
    (is (= (parse "1 2 +") [1 2 ADD]))
    (is (= (parse "1 2+") [1 2 ADD]))))

(deftest test-string
  (testing "test parse string"
    (is (= (parse "\"hello\"") ["hello"]))
    (is (= (parse "\"\\\"hello\"") ["\"hello"]))))

(deftest test-comments
  (testing "test parse comments"
    (is (= (parse "{this is comments}1 2 +") [1 2 ADD]))))

(deftest test-char
  (testing "test parse char"
    (is (= (parse "'A") [65]))))

(deftest test-false-var
  (testing "test parse var"
    (is (= (parse "a") [\a]))
    (is (= (parse "abc") [\a \b \c]))
    (is (= (parse "1 a:") [1 \a ASSIGN-VAR]))))

(deftest test-subroutine
  (testing "test parse subroutine"
    (let [commands (parse "[1 2+]")]
      (is (= 1 (count commands)))
      (is (map? (first commands)))
      (let [sub-commands (:commands (first commands))]
        (is (= 3 (count sub-commands)))
        (is (= 1 (first sub-commands)))
        (is (= 2 (second sub-commands)))
        (is (map? (last sub-commands)))))
    ;; ttest [[]]
    ))
