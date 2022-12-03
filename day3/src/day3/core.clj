(ns day3.core
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.set :as set])


; Part 1
(defn readlines [filename] (str/split (slurp filename) #"\n"))
(defn split-rucksacks [line] (partition (/ (count line) 2) line))
(defn get-common-letters [line] (apply set/intersection (map set (split-rucksacks line))))
(defn upper-case? [character] (= (str character) (str/upper-case character)))
(defn ascii [letter] (condp = (type letter)
                       java.lang.Character (int letter)
                       java.lang.String (int (.charAt letter 0))
                       0))
(defn score-upper-case-letter [letter] (+ 27 (- (ascii letter) (int \A))))
(defn score-lower-case-letter [letter] (+ 1 (- (ascii letter) (int \a))))
(defn score-letter [letter] (if (upper-case? letter) 
                              (score-upper-case-letter letter) 
                              (score-lower-case-letter letter)))
(defn score-set [set-of-letters] (reduce + (map score-letter set-of-letters)))
(defn score-input [input] (reduce + (map score-set (map get-common-letters input))))
(defn score-file [filename] (score-input (readlines filename)))

; Part 2
(defn get-groups [input] (partition 3 input))
(defn get-group-sets [groups] (map #(apply set/intersection (map set %)) groups)) 
(defn score-group-sets [group-sets] (reduce + (map score-set group-sets)))
(defn score-input-groups [input] (score-group-sets (get-group-sets (get-groups input))))
(defn score-file-groups [filename] (score-input-groups (readlines filename)))


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 3"
  [& args]
  (println "Part 1 (Sample):" (score-file "sample-input.txt")
           "\nPart 1:" (score-file "input.txt")
           "\nPart 2 (Sample):" (score-file-groups "sample-input.txt")
           "\nPart 2:" (score-file-groups "input.txt")))
