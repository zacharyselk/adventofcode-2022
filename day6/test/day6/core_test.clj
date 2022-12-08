(ns day6.core-test
  (:require [clojure.test :refer :all]
            [day6.core :refer :all]))

(deftest day-6-tests
  (testing "Is valid string uniuqe"
    (is (= true (are-characters-unique? "abcd"))))

  (testing "Is invalid string not uniuqe"
    (is (= false (are-characters-unique? "abca"))))

  (testing "Using character sequence"
    (is (= true (are-characters-unique? '(\a \b \c)))))
  
  (testing "Get all sequences"
    (is (= (list '(\a \b \c \d) '(\b \c \d \e) '(\c \d \e \f))
           (get-all-windows "abcdef" 4))))

  (testing "Find the first unique array"
    (is (= 2 (index-of-first-unique 
               (list '(\a \b \b) 
                     '(\b \b \d) 
                     '(\b \d \e) 
                     '(\d \e \d)))))

  (testing "Sample 0 Part 1" 
    (is (= 7 (find-marker 4 "sample-input-0.txt"))))

  (testing "Sample 1 Part 1" 
    (is (= 5 (find-marker 4 "sample-input-1.txt"))))

  (testing "Sample 2 Part 1" 
    (is (= 6 (find-marker 4 "sample-input-2.txt"))))

  (testing "Sample 3 Part 1" 
    (is (= 10 (find-marker 4 "sample-input-3.txt"))))

  (testing "Sample 4 Part 1" 
    (is (= 11 (find-marker 4 "sample-input-4.txt"))))

  (testing "Full Input Part 1"
    (is (= 1210 (find-marker 4 "input.txt"))))


  (testing "Sample 0 Part 2" 
    (is (= 19 (find-marker 14 "sample-input-0.txt"))))

  (testing "Sample 1 Part 2" 
    (is (= 23 (find-marker 14 "sample-input-1.txt"))))

  (testing "Sample 2 Part 2" 
    (is (= 23 (find-marker 14 "sample-input-2.txt"))))

  (testing "Sample 3 Part 2" 
    (is (= 29 (find-marker 14 "sample-input-3.txt"))))

  (testing "Sample 4 Part 2" 
    (is (= 26 (find-marker 14 "sample-input-4.txt"))))

  (testing "Full Input Part 2"
    (is (= 3476 (find-marker 14 "input.txt"))))))
