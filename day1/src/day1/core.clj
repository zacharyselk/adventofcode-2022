(ns day1.core
  (:gen-class))
(require '[clojure.string :as str])

(defn get-calories [filename]
  (map (fn [calories-string] (reduce + (map #(Integer/parseInt %) (str/split calories-string #"\n"))))
       (str/split (slurp filename) #"\n\n")))

(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 1"
  [& args]
  (println
   "Part 1:" (reduce max (get-calories "input.txt"))
   "\nPart 2:" (reduce + (first (partition 3 (reverse (sort (get-calories "input.txt"))))))))
