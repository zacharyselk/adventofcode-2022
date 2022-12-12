(ns day8.core
  (:gen-class))
(require '[clojure.set :as set])
(require '[clojure.string :as str])


(def grid (list (list 3 0 3 7 3)
                (list 2 5 5 1 2)
                (list 6 5 3 3 2)
                (list 3 3 5 4 9)
                (list 3 5 3 9 0)))

(defn get-lines
  [filename]
  (str/split (slurp filename) #"\n"))

(defn transpose
  [m]
  (apply mapv vector m))

(defn get-tree-tops
  [line-of-trees]
  (->> line-of-trees
       (reductions max)
       (map-indexed vector)
       (partition-by second)
       (map ffirst)))
(get-tree-tops (list 3 0 3 7 3))

(defn get-visable-trees-in-line
  [line-of-trees]
  (sort (distinct (concat (get-tree-tops line-of-trees)
                        (map #(- (count line-of-trees) (inc %)) (get-tree-tops (reverse line-of-trees)))))))
(get-visable-trees-in-line (list 3 0 3 7 3))

(defn get-visable-tree-rows
  [grid-of-trees]
  (map-indexed (fn [index line] (map #(vector index %) (get-visable-trees-in-line line))) grid-of-trees))

(defn get-visable-tree-rows-transposed
  [grid-of-trees]
  (map-indexed (fn [index line] (map #(vector % index) (get-visable-trees-in-line line))) grid-of-trees))


(defn get-visable-trees-in-grid
  [grid-of-trees]
  (set/union (set (apply concat (get-visable-tree-rows grid-of-trees)))
             (set (apply concat (get-visable-tree-rows-transposed (transpose grid-of-trees))))))

(defn scenic-score-for-tree-in-line
  [tree-index line]
  (let [tree-height (nth line tree-index)
        trees-to-the-left (drop (- (count line) tree-index) (reverse line))
        trees-to-the-right (drop (+ 1 tree-index) line)]
    ;(println tree-height trees-to-the-left trees-to-the-right)
    (* (min (count trees-to-the-right) (+ 1 (count (take-while #(> tree-height %) trees-to-the-right))))
       (min (count trees-to-the-left) (+ 1 (count (take-while #(> tree-height %) trees-to-the-left)))))))
;(scenic-score-for-tree-in-line 2 (list 3 3 5 4 9))

(defn scenic-score-for-line
  [line]
  (map #(scenic-score-for-tree-in-line % line) (range (count line))))
;(scenic-score-for-line (list 3 3 5 4 9))

(defn scenic-score-for-grid
  [grid-of-trees]
  (let [row-scores (map scenic-score-for-line grid-of-trees)
        col-scores (transpose (map scenic-score-for-line (transpose grid-of-trees)))]
    (mapv * (flatten row-scores) (flatten col-scores))))
    ;row-scores))
;(scenic-score-for-grid grid)

(defn part1
  [filename]
  (->> (get-lines filename)
       (map interleave)
       (map (fn [line] (map #(Integer/parseInt (str %)) line)))
       (get-visable-trees-in-grid)
       (count)))

(defn part2
  [filename]
  (->> (get-lines filename)
       (map interleave)
       (map (fn [line] (map #(Integer/parseInt (str %)) line)))
       (scenic-score-for-grid)
       (apply max)
       ))


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 8"
  [& args]
  (println "Part 1 (Sample):" (part1 "sample-input.txt")
           "\nPart 1:" (part1 "input.txt")
           "\nPart 2 (Sample):" (part2 "sample-input.txt")
           "\nPart 2:" (part2 "input.txt")))
