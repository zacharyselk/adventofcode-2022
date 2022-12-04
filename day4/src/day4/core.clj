(ns day4.core
  (:gen-class))
(require '[clojure.string :as str])


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 4"
  [& args]
  (println "Part 1 (Sample):" (count-constraind-bounds contained-bound? "sample-input.txt")
           "\nPart 1:" (count-constraind-bounds contained-bound? "input.txt")
           "\nPart 2 (Sample):" (count-constraind-bounds overlapping-bounds? "sample-input.txt")
           "\nPart 2:" (count-constraind-bounds overlapping-bounds? "input.txt")))

(defrecord Bounds [minimum maximum])
; Default constructor for Bounds
(defn create-bounds [minimum maximum] (->Bounds minimum maximum))
; Creates a Bounds from a string in the form of "{min}-{max}"
(defn create-bounds-from-str [minmax-str] (apply #(create-bounds (Integer/parseInt %1) (Integer/parseInt %2)) (str/split minmax-str #"-")))
(defn does-a-contain-b-bounds? [^Bounds a ^Bounds b] (and (<= (:minimum a) (:minimum b)) (>= (:maximum a) (:maximum b))))
(defn overlapping-bounds? [^Bounds a ^Bounds b] (and (<= (:minimum b) (:maximum a)) (>= (:maximum b) (:minimum a))))
(defn contained-bound? [^Bounds a ^Bounds b] (or (does-a-contain-b-bounds? a b) (does-a-contain-b-bounds? b a)))

(defn get-input-lines [filename] (str/split (slurp filename) #"\n"))
(defn create-bounds-from-line [line] (apply #(list (create-bounds-from-str %1) (create-bounds-from-str %2)) (str/split line #",")))
(defn get-constrained-bounds [constraint input-lines] (map #(apply constraint (create-bounds-from-line %)) input-lines))
(defn count-trues [lst] (count (filter #(= % true) lst)))
(defn count-constraind-bounds [constraint filename] (count-trues (get-constrained-bounds constraint (get-input-lines filename))))
