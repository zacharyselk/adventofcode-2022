(ns day6.core
  (:gen-class))

(defn read-file [filename] 
  (slurp filename))

(defn are-characters-unique? [string] 
  (apply distinct? (interleave string)))
 
(defn get-all-windows [string size] 
  (partition size 1 string))

(defn index-of-first-unique [list-of-windows]
  (first (keep-indexed #(if (are-characters-unique? %2) %1) list-of-windows)))

(defn find-marker [size filename] (+ size 
                                     (-> (read-file filename)
                                         (get-all-windows size)
                                         (index-of-first-unique))))


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 5"
  [& args]
  (println "Part 1 (Sample 0):" (find-marker 4 "sample-input-0.txt")
           "\nPart 1 (Sample 1):" (find-marker 4 "sample-input-1.txt")
           "\nPart 2 (Sample 2):" (find-marker 4 "sample-input-2.txt")
           "\nPart 3 (Sample 3):" (find-marker 4 "sample-input-3.txt")
           "\nPart 4 (Sample 4):" (find-marker 4 "sample-input-4.txt")
           "\nPart 1:" (find-marker 4 "input.txt")
           "\nPart 2 (Sample 0):" (find-marker 14 "sample-input-0.txt")
           "\nPart 2 (Sample 1):" (find-marker 14 "sample-input-1.txt")
           "\nPart 2 (Sample 2):" (find-marker 14 "sample-input-2.txt")
           "\nPart 2 (Sample 3):" (find-marker 14 "sample-input-3.txt")
           "\nPart 2 (Sample 4):" (find-marker 14 "sample-input-4.txt")
           "\nPart 2:" (find-marker 14 "input.txt")))
