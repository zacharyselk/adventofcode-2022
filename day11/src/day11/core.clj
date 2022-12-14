(ns day11.core
  (:gen-class))
(require '[clojure.string :as str])


(defrecord Monkey [starting-items operation test inspected-items divisor])

;(def sample-input-monkeys
;  [(->Monkey [79, 98] #(* % 19) #(if (= 0 (rem % 23)) 2 3) 0)
;   (->Monkey [54 65 75 74] #(+ % 6) #(if (= 0 (rem % 19)) 2 0) 0)
;   (->Monkey [79 60 97] #(* % %) #(if (= 0 (rem % 13)) 1 3) 0)
;   (->Monkey [74] #(+ % 3) #(if (= 0 (rem % 17)) 0 1) 0)])
;
;(def input-monkeys
;  [(->Monkey [85 79 63 72] #(* % 17) #(if (= 0 (rem % 2)) 2 6) 0)
;   (->Monkey [53 94 65 81 93 73 57 92] #(* % %) #(if (= 0 (rem % 7)) 0 2) 0)
;   (->Monkey [62 63] #(+ % 7) #(if (= 0 (rem % 13)) 7 6) 0)
;   (->Monkey [57 92 56] #(+ % 4) #(if (= 0 (rem % 5)) 4 5) 0)
;   (->Monkey [67] #(+ % 5) #(if (= 0 (rem % 3)) 1 5) 0)
;   (->Monkey [85 56 66 72 57 99] #(+ % 6) #(if (= 0 (rem % 19)) 1 0) 0)
;   (->Monkey [86 65 98 97 69] #(* % 13) #(if (= 0 (rem % 11)) 3 7) 0)
;   (->Monkey [87 68 92 66 91 50 68] #(+ % 2) #(if (= 0 (rem % 17)) 4 3) 0)])

(defn get-monkeys-lcm
  [monkeys]
  (reduce * (map :divisor monkeys)))

(defn read-monkey-lines
  [filename]
  (map #(str/split % #"\n") (str/split (slurp filename) #"\n\n")))

(defn parse-starting-items
  [line]
  (map #(bigint (Integer/parseInt (str/trim %))) (str/split (nth (str/split line #":") 1) #",")))

(defn reorder-operation
  [line]
  (let [split-operation (str/split line #" ")]
    (str/join " " (list (nth split-operation 1) (nth split-operation 0) (nth split-operation 2)))))

(defn parse-operation
  [line]
  (eval (read-string (str "#(" (reorder-operation (str/replace (str/trim (nth (str/split line #"=") 1)) #"old" "%")) ")"))))

(defn parse-divisor
  [line]
  (Integer/parseInt (str/trim (nth (str/split line #" by ") 1))))


(defn parse-test
  [line0 line1 line2]
  (let [divisor (parse-divisor line0)
        true-index (Integer/parseInt (str/trim (nth (str/split line1 #" monkey ") 1)))
        false-index (Integer/parseInt (str/trim (nth (str/split line2 #" monkey ") 1)))]
    (fn [value] (if (= 0 (rem value divisor)) true-index false-index))))

(defn read-monkeys
  [filename]
  (->> (read-monkey-lines filename)
       (map #(->Monkey (parse-starting-items (nth % 1))
                       (parse-operation (nth % 2))
                       (parse-test (nth % 3) (nth % 4) (nth % 5))
                       (bigint 0)
                       (parse-divisor (nth % 3))))
       ))

(defn reduce-worry
  [current-worry]
  (quot current-worry 3))

(defn clone-monkey
  [monkey new-items, new-inspections]
  (->Monkey new-items (:operation monkey) (:test monkey) new-inspections (:divisor monkey)))

(defn append-items-to-monkey
  [monkey items]
  (clone-monkey monkey (concat (:starting-items monkey) items) (:inspected-items monkey)))

(defn handle-monkey-turn
  [monkey worry-operator lcm]
  (->> (:starting-items monkey)
       (map #(rem ((:operation monkey) %) lcm))
       (map worry-operator)
       (map #(list ((:test monkey) %) %))
       ))

(defn end-monkey-turn
  [monkey]
  (clone-monkey [] (+ 1 (:inspected-items monkey))))

(defn new-monkey-list-after-turn
  [worry-operator lcm monkeys current-monkey-index]
  (let [current-monkey (nth monkeys current-monkey-index)]
          (if (= 0 (count (:starting-items current-monkey)))
            monkeys
            (->> (handle-monkey-turn current-monkey worry-operator lcm)
                 (concat (map-indexed #(if (= %1 current-monkey-index) (list %1 []) (list %1 (:starting-items %2))) monkeys))
                 (group-by first)
                 (map (fn [map-item] (list (first map-item) (reduce concat (map #(drop 1 %) (second map-item))))))
                 (map (fn [map-item] (list (first map-item) (flatten (second map-item)))))
                 (map (fn [map-item]
                        (let [[index items] map-item
                              monkey (nth monkeys index)
                              inspected-items (:inspected-items monkey)]
                          (if (= index current-monkey-index)
                            (clone-monkey monkey items (+ (count (:starting-items monkey)) inspected-items))
                            (clone-monkey monkey items inspected-items)))))
                 ))))

(defn part1
  [filename]
  (let [list-of-monkeys (read-monkeys filename)
        lcm (get-monkeys-lcm list-of-monkeys)]
    (->> (range)
         (take (count list-of-monkeys))
         (repeat)
         (take 20)
         (apply concat)
         (reduce #(new-monkey-list-after-turn reduce-worry lcm %1 %2) list-of-monkeys)
         (map :inspected-items)
         (sort)
         (reverse)
         (take 2)
         (reduce *)
       )))

(defn part2
  [filename]
  (let [list-of-monkeys (read-monkeys filename)
        lcm (get-monkeys-lcm list-of-monkeys)]
    (->> (range)
         (take (count list-of-monkeys))
         (repeat)
         (take 10000)
         (apply concat)
         (reduce #(new-monkey-list-after-turn identity lcm %1 %2) list-of-monkeys)
         (map :inspected-items)
         (sort)
         (reverse)
         (take 2)
         (reduce *)
       )))

(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 11"
  [& args]
  (println "Part 1 (Sample):" (part1 "sample-input.txt")
           "\nPart 1:" (part1 "input.txt")
           "\nPart 2 (Sample):" (part2 "sample-input.txt")
           "\nPart 2:" (part2 "input.txt")
           ))
