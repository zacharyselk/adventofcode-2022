(ns day10.core
  (:gen-class))
(require '[clojure.string :as str])

(defn read-lines
  [filename]
  (str/split (slurp filename) #"\n"))

(defn execute-instruction
  [register command]
  (let [instruction (apply str (take 4 command))
        arg (apply str (drop 5 command))
        register-value (last register)]
    (condp = instruction
      "noop" [register-value]
      "addx" [register-value (+ register-value (Integer/parseInt arg))])))
(execute-instruction [1] "addx 4")

(defn is-in-sprite-bounds?
  [current-cycle sprite-position]
  (> 2 (abs (- current-cycle sprite-position))))

(defn get-symbol
  [current-cycle sprite-position]
  (if (is-in-sprite-bounds? current-cycle sprite-position) \# \.))

(defn part1
  [filename]
  (->> (read-lines filename)
       (reductions execute-instruction [1])
       (flatten)
       (map-indexed #(list (+ 1 %1) %2))
       (drop 19)
       (partition-all 40)
       (map first)
       (map #(* (first %) (second %)))
       (reduce +)
       ))

(defn part2
  [filename]
  (->> (read-lines filename)
       (reductions execute-instruction [1])
       (flatten)
       (partition-all 40)
       (map (fn [group-of-40] (apply str (map-indexed #(get-symbol %1 %2) group-of-40))))
       (str/join "\n")
       ))


(defn -main
  "Part⋅1⋅&⋅2⋅of⋅the⋅2022⋅Advent⋅of⋅Code⋅Day⋅9"
  [& args]
  (println "Part 1 (Sample):" (part1 "sample-input-1.txt")
           "\nPart 1:" (part1 "input.txt")
           "\nPart 2 (Sample):\n" (part2 "sample-input-1.txt")
           "\nPart 2:\n" (part2 "input.txt")))
