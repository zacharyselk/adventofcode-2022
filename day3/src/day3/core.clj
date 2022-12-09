(ns day3.core
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.set :as set])


(defn upper-case? [character] (= (str character) (str/upper-case character)))
(defn get-lines [filename] (str/split (slurp filename) #"\n"))
(defn split-line [line] (partition (/ (count line) 2) line))
(defn make-sets [inputs] (map set inputs))
(defn compute-priority [character] (if (upper-case? character)
       (+ 27 (- (int character) (int \A)))
       (+ 1 (- (int character) (int \a)))))

(defn part1 [filename]
  (->> (get-lines filename)
       (map split-line)
       (map make-sets)
       (map #(apply set/intersection %))
       (map #(apply compute-priority %))
       (reduce +)
  ))

(defn part2 [filename]
  (->> (get-lines filename)
       (partition 3)
       (map make-sets)
       (map #(apply set/intersection %))
       (map #(apply compute-priority %))
       (reduce +)
  ))


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 3"
  [& args]
  (println "Part 1 (Sample):" (part1 "sample-input.txt")
           "\nPart 1:" (part1 "input.txt")
           "\nPart 2 (Sample):" (part2 "sample-input.txt")
           "\nPart 2:" (part2 "input.txt")))
