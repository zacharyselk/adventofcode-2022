(ns day9.core
  (:gen-class))
(require '[clojure.string :as str])

(def move-map 
  {
   "U" [0 2]
   "R" [2 0]
   "D" [0 -2]
   "L" [-2 0]
  })

(defn calculate-new-tail-position
  [head-pos tail-pos]
  (let [pos-diff (mapv - head-pos tail-pos)
        tail-adjustment (condp = pos-diff
                          [0 0]   [0 0]
                          [0 1]   [0 0]   ; One above
                          [1 1]   [0 0]   ; One above and to the right
                          [1 0]   [0 0]   ; One to the right
                          [1 -1]  [0 0]   ; One below and to the right
                          [0 -1]  [0 0]   ; One below
                          [-1 -1] [0 0]   ; One below and to the left
                          [-1 0]  [0 0]   ; One to the left
                          [-1 1]  [0 0]   ; One above and to the left

                          [0 2]   [0 1]   ; Head is two above
                          [1 2]   [1 1]   ; Head is two above and on to the right
                          [2 2]   [1 1]   ; Head is two above and two to the right
                          [2 1]   [1 1]   ; Head is one above and two to the right
                          [2 0]   [1 0]   ; Head is two to the right
                          [2 -1]  [1 -1]  ; Head is one below and two to the right
                          [2 -2]  [1 -1]  ; Head is two below and two to the right
                          [1 -2]  [1 -1]  ; Head is two below and one to the right
                          [0 -2]  [0 -1]  ; Head is two below
                          [-1 -2] [-1 -1] ; Head is two below and one to the left
                          [-2 -2] [-1 -1] ; Head is two below and two to the left
                          [-2 -1] [-1 -1] ; Head is one below and two to the left
                          [-2 0]  [-1 0]  ; Head is two to the left
                          [-2 1]  [-1 1]  ; Head is one above and two to the left
                          [-2 2]  [-1 1]  ; Head is two above and two to the left
                          [-1 2]  [-1 1]  ; Head is two above and one to the left
                          [0 0])]
    (mapv + tail-adjustment tail-pos)))
;(calculate-new-tail-position [3 2] [1 1])

(defn apply-move
  [positions move]
  (let [head-pos (first positions)
        tail-pos (second positions)
        new-head-pos (mapv + head-pos move)
        new-tail-pos (calculate-new-tail-position new-head-pos tail-pos)]
    [new-head-pos new-tail-pos]))

(defn apply-move
  [positions move]
  (rest (reductions calculate-new-tail-position (mapv + (first positions) move) positions)))

(defn create-moves-from-line
  [line]
  (let [[direction times] (str/split line #" ")]
    (take (Integer/parseInt times) (repeat (get move-map direction)))))
;(create-moves-from-line "R 4")

(defn get-lines
  [filename]
  (str/split (slurp filename) #"\n"))

(defn count-tail-moves
  [rope-size filename]
  (->> (get-lines filename)
       (map create-moves-from-line)
       (apply concat)
       (reductions apply-move (take rope-size (repeat [0 0])))
       (map last)
       (set)
       (count)
       ))


(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 9"
  [& args]
  (println "Part 1 (Sample):" (count-tail-moves 2 "sample-input-0.txt")
           "\nPart 1:" (count-tail-moves 2 "input.txt")
           "\nPart 2 (Sample 0):" (count-tail-moves 10 "sample-input-0.txt")
           "\nPart 2 (Sample 1):" (count-tail-moves 10 "sample-input-1.txt")
           "\nPart 2:" (count-tail-moves 10 "input.txt")
           ))
