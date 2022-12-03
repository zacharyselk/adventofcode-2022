(ns day2.core
  (:gen-class))
(require '[clojure.string :as str])

(defn move-value [move]
  (case move
    :rock 1
    :paper 2
    :scissors 3))
(def symbol-to-move {"A" :rock, "X" :rock,
                     "B" :paper, "Y" :paper,
                     "C" :scissors, "Z" :scissors})
(def move-to-symbol {:rock "A", :paper "B", :scissors "C"})
(defn get-winning-move [move]
  (if (= move :rock)
    :paper
    (if (= move :paper)
      :scissors
      :rock)))

(defn get-losing-move [move]
  (if (= move :rock)
    :scissors
    (if (= move :paper)
      :rock
      :paper)))
(defn convert-part2-round [round-string] 
  (apply 
   #(case %2
      "X" (str %1 " " (move-to-symbol (get-losing-move (symbol-to-move %1))))
      "Y" (str %1 " " %1)
      "Z" (str %1 " " (move-to-symbol (get-winning-move (symbol-to-move %1))))) 
  (str/split round-string #" ")))

(defn score-round [our-move their-move]
  (if (= our-move their-move)
    3
    (if (= our-move :rock)
      (if (= their-move :paper)
        0
        6)
      (if (= our-move :paper)
        (if (= their-move :scissors)
          0
          6)
        (if (= our-move :scissors)
          (if (= their-move :rock)
            0
            6)
          0)))))
(move-value (get symbol-to-move "X"))

(defn total-score-round [your-move their-move]
  (+ (score-round your-move their-move) (move-value your-move)))

(defn get-moves [data] (str/split data #"\n"))
(defn score-move-string [move-string] (apply #(total-score-round (symbol-to-move %2) (symbol-to-move %1)) (str/split move-string #" ")))
(defn read-data [filename] (slurp filename))
(defn get-input-moves [] (get-moves (read-data "input.txt")))
(defn score-part-1 [] (reduce + (map score-move-string (get-input-moves))))
(defn score-part-2 [] (reduce + (map score-move-string (map #(convert-part2-round %) (get-input-moves)))))

(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 2"
  [& args]
  (println 
   "Part 1:" (score-part-1) 
   "\nPart 2:" (score-part-2)))