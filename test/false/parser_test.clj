(ns false.parser-test
  (use clojure.test
       false.core))
(declare false-eq?)
(defn false-subroutine-eq?
  [x y]
  (and (custom-func? x)
       (custom-func? y)
       (false-eq? (:commands x) (:commands y))))

(defn false-eq?
  "Whether two FALSE program equals"
  [x y]
  (let [comp-fn (fn [x y]
                  (if (custom-func? x)
                    (false-subroutine-eq? x y)
                    (= x y)))]
    (and (= (count x) (count y))
         (loop [x x y y]
           (if (seq x)
             (if-not (comp-fn (first x) (first y))
               false
               (recur (rest x) (rest y)))
             true)))))

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
    (is (false-eq? (parse "[1 2+]") [(custom-func [1 2 ADD])]))
    (is (false-eq? (parse "[1 2 [1 2+] +]")
                   [(custom-func [1 2
                                  (custom-func [1 2 ADD])
                                  ADD])]))))
