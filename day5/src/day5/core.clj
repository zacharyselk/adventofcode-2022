(ns day5.core
  (:gen-class))
(require '[clojure.string :as str])

(defn transpose [m] (apply mapv vector m))

;; Parsing indavidual stack lines
(defn drop-nth [n coll] (keep-indexed #(if (not= (mod %1 n) (- n 1)) %2) coll))
(defn remove-square-brackets [string] (str/replace string #"[\[\]]" ""))
(defn stack-line-to-seq [line] (map #(str/trim (remove-square-brackets (apply str %))) (partition 3 (drop-nth 4 line))))
(defn parse-stack-line [line] (->> line
                                   remove-square-brackets
                                   stack-line-to-seq))
(defn parse-crate-line [line] (->> line
                                   stack-line-to-seq))
;(parse-stack-line " 1   2   3   4 ")
;(parse-stack-line" [a]   [b] [c]   [d]   ")
;(map #(println %) (stack-line-to-seq " 1   2   3 "))
(defn parse-command-line [line] (rest (re-find (re-matcher #"move (\d+) from (\d+) to (\d+)" line))))
(defn parse-commands-string [string] (map parse-command-line (str/split string #"\n")))

(defn clean-up-empty-elements-in-stack [stack] (reverse (drop-while #(= "" %) (reverse stack))))
;(clean-up-empty-elements-in-stack (list "a" "b" "" ""))
(defn take-all-but-first [n seq] (take-last (- (count seq) n) seq))
(defn get-stack-number-value-pair [stack-string] (#(list (take 1 %) (take-all-but-first 1 %)) (reverse (str/split stack-string #"\n"))))
(defn parse-stack-lines [stack-numbers stack-values] (list (apply stack-line-to-seq stack-numbers) (map clean-up-empty-elements-in-stack (transpose (map stack-line-to-seq stack-values)))))
(defn zip-stack-numbers-and-values [stack-numbers stack-values] (zipmap (flatten stack-numbers) stack-values))
(defn parse-stack-string [string] (apply zip-stack-numbers-and-values (apply parse-stack-lines (get-stack-number-value-pair string))))
(defn parse-file-string [string] (apply #(list (parse-stack-string %1) (parse-commands-string %2)) (str/split string #"\n\n")))


(defn peek-n [n stack] (take-last n stack))
(defn pop-n [n stack] (drop-last n stack))
(defn push [items stack] (concat stack items))
;(peek-n 2 (list 1 2 3 4))
;(pop-n 2 (list 1 2 3 4))
;(push (list 8 9) (list 1 2 3 4))

(defn move-n-items-flipped [n from-stack to-stack] (let [new-to (push (reverse (peek-n n from-stack)) to-stack)]
                                 (list (pop-n n from-stack) new-to)))
(defn move-n-items [n from-stack to-stack] (let [new-to (push (peek-n n from-stack) to-stack)]
                                 (list (pop-n n from-stack) new-to)))
;(move-n-items 2 (list "b" "c") (list "" ""))

(defn move-indaviduals [stacks command] (let [[n from to] command
                                 from-stack (get stacks from)
                                 to-stack (get stacks to)]
                             (move-n-items-flipped (Integer/parseInt n) from-stack to-stack)))
(defn move-stack [stacks command] (let [[n from to] command
                                 from-stack (get stacks from)
                                 to-stack (get stacks to)]
                             (move-n-items (Integer/parseInt n) from-stack to-stack)))
;(move (zipmap '["1" "2" "3"] '[("a") ("b" "c") ()]) '("2" "2" "3"))
;(let [stacks (zipmap '["1" "2" "3"] '[("a") ("b" "c") ()])
;      [n from to] '("2" "2" "3")
;      [new-from new-to] (move stacks (list n from to))]
;    (assoc (assoc stacks from new-from) to new-to))

(defn apply-part-1-command-to-stacks [stacks command] (let [[n from to] command
                                           [new-from new-to] (move-indaviduals stacks (list n from to))]
                                       (assoc (assoc stacks from new-from) to new-to)))
(defn apply-part-2-command-to-stacks [stacks command] (let [[n from to] command
                                           [new-from new-to] (move-stack stacks (list n from to))]
                                       (assoc (assoc stacks from new-from) to new-to)))
;(apply-command-to-stacks (zipmap '["1" "2" "3"] '[("a") ("b" "c") ()]) '("2" "2" "3"))

(defn get-stack-tops [stacks] (apply str (map #(peek (reverse (peek %))) (sort-by first stacks))))
;(get-stack-tops {"a" (list 1 2 3) "b" (list 10 9 8)})

(defn simulate-part-1-file [filename] (let [[stacks commands] (parse-file-string (slurp filename))]
  (get-stack-tops (reduce apply-part-1-command-to-stacks stacks commands))))
(defn simulate-part-2-file [filename] (let [[stacks commands] (parse-file-string (slurp filename))]
  (get-stack-tops (reduce apply-part-2-command-to-stacks stacks commands))))




(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 5"
  [& args]
  (println "Part 1 (Sample):" (simulate-part-1-file "sample-input.txt")
           "\nPart 1:" (simulate-part-1-file "input.txt")
           "\nPart 2 (Sample):" (simulate-part-2-file "sample-input.txt")
           "\nPart 2:" (simulate-part-2-file "input.txt")))
