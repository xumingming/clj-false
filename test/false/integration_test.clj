(ns false.integration-test
  (use clojure.test
       false.core)
  (import java.io.StringWriter))

(deftest test-basic-run
  (testing "test basic run"
    (is (= (run "1 2 +") 3))
    (is (= (run "1 2+") 3))

    (let [wr (StringWriter.)]
      (binding [*out* wr]
        (run "\"hello\""))
      (is (= "hello" (str wr))))))

(deftest test-assign-read-var-subroutine-if
  (testing "test assign var, read var subroutine, if"
    (is (= 4 (run "1a:a;1=[3b:]?1b;+")))))

(deftest test-prime
  (testing "testing prime"
    (is (= 1 (run "99 9[1-$][\\@$@$@$@\\/*=[1-$$[%\\1-$@]?0=[\\$.' ,\\]?]?]#")))))