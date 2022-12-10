(ns day7.core-test
  (:require [clojure.test :refer :all]
            [day7.core :refer :all]))


(deftest test-get-command-from-line
  (testing "Extract from valid line"
    (is (= "ls" (get-command-from-line "ls /"))))

  (testing "Extract from line with no argument"
    (is (= "ls" (get-command-from-line "ls"))))

  (testing "Extract from line with content"
    (is (= "ls" (get-command-from-line "ls /\nhello world"))))
  )


(deftest test-get-arguments-from-line
  (testing "Extract single argument"
    (is (= "/a/b/c/" (get-arguments-from-line "ls /a/b/c/"))))

  (testing "Extract multiple arguments"
    (is (= "a/ b/" (get-arguments-from-line "ls a/ b/"))))

  (testing "Extract argument from multi-line string"
    (is (= "a/b/" (get-arguments-from-line "ls a/b/\na thing\n another thing"))))

  (testing "With no arguments"
    (is (= "" (get-arguments-from-line "ls"))))
  )

(deftest test-get-results-from-line
  (testing "Extract results"
    (is (= "a.txt 3000\nb.txt 0" (get-results-from-line "ls /\na.txt 3000\nb.txt 0"))))

  (testing "With no results"
    (is (= "" (get-results-from-line "cd /"))))
  )

(deftest test-create-command)

(deftest test-merge-paths
  (testing "Merge to normal paths"
    (is (= "a/b" (merge-paths "a" "b"))))
  )

  (testing "Merge two absolut paths"
    (is (= "/goodbye/friend" (merge-paths "/hello/world" "/goodbye/friend"))))
