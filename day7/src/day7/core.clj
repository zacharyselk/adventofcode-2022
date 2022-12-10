(ns day7.core
  (:gen-class))
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as reducers])


(defrecord Command [command path result])
(defrecord File [name type size location children])

(defn get-command-lines
  "Reads input and seperates by command"
  [^String filename]
  (rest (str/split (slurp filename) #"\$ ")))

(defn get-command-from-line
  "Extracts the command string from the line"
  [^String line]
  (first (str/split line #"\s")))

(defn get-arguments-from-line
  "Extracts the argumens from a command string"
  [^String line]
  (or (second (re-matches #"[a-zA-Z0-9]+ +(.+)$" (first (str/split line #"\n")))) ""))

(defn get-results-from-line
  "Extracts the command results from a command string"
  [^String line]
  (str/join "\n" (rest (str/split line #"\n"))))

(defn create-command
  "Parses command line to create a Command record"
  [^String line]
  (->Command (get-command-from-line line) (get-arguments-from-line line) (get-results-from-line line)))

(defn rsplit-path
  "Splits and reverses a path"
  [path]
  (reverse (str/split path #"/")))

(defn rjoin-path
  "Joins a reverse split path"
  [path]
  (str/join "/" (reverse path)))

(defn cleanup-path
  "Cleanup all of the '..'s and '//'s from the path"
  [original-path]
  (let [deslashed-path (str/replace original-path #"//" "/")]
    (case deslashed-path
      "" deslashed-path
      "/" deslashed-path
      (let [path-array (rsplit-path deslashed-path)]
        (if (= ".." (first path-array))
          (rjoin-path (drop 2 path-array))
          (rjoin-path path-array))))))

(defn merge-paths
  "Merges two paths together assuming that path-a is the current path"
  [path-a path-b]
  (if (= "" path-a) path-b
    (if (= "" path-b) path-a
      (if (= \/ (first path-b)) path-b
       (str/join "/" (list path-a path-b))))))

(defn make-path-absolute
  "If a path does not start with a '/', prepend it"
  [path]
  (if (= \/ (first path)) path (str "/" path)))

(defn parse-file
  "Takes a string of a file and parses it for a file type file"
  [file-string path]
  (let [matches (re-matches #"^([0-9]+) (.*)$" file-string)]
    (->File (nth matches 2) "file" (Integer/parseInt (nth matches 1)) path nil)))

(defn is-string-of-dir?
  "Checks to see if a given string is in the form of 'dir SOME-NAME'"
  [string]
  (= "dir " (apply str (take 4 string))))

(defn parse-directory
  "Takes a string of a file and parses it for a directory type file"
  [dir-string path]
  (->File (apply str (drop 4 dir-string)) "dir" 0 path nil))

(defn parse-ls-result
  "Takes a string of files from ls and parses them into File records"
  [ls-result path]
  (let [list-of-files (str/split ls-result #"\n")]
    (map #(if (is-string-of-dir? %) (parse-directory % path) (parse-file % path)) list-of-files)))

(defn adjust-command-path
  "Update the path of the command using the current path"
  [^Command current-command ^Command command]
  (let [absolute-path (make-path-absolute (cleanup-path (merge-paths (:path current-command) (:path command))))]
      (->Command (:command command) absolute-path (:result command))))

(defn get-file-full-path
  "Return the full path of a file"
  [^File file]
  (cleanup-path (merge-paths (:location file) (:name file))))

(defn build-sub-tree
  "Builds a sub tree of the file system using a prefix"
  [prefix files]
  (let [prefixed-files (filter #(= prefix (:location %)) files)
        file-files (filter #(= "file" (:type %)) prefixed-files)
        dir-files (filter #(= "dir" (:type %)) prefixed-files)]  
    (let [children-files (map 
                           (fn [dir-file] (let [[sub-tree-size sub-tree-files] (build-sub-tree (get-file-full-path dir-file) files)]
                                            (->File (:name dir-file) "dir" sub-tree-size prefix sub-tree-files)))
                           dir-files)
          sub-files (concat children-files file-files)]
      (list (reduce + (map :size sub-files)) sub-files))))

(defn create-root-file-structure
  "Builds a root file structure using a series of files"
  [files]
  (let [[file-tree-size file-tree-files] (build-sub-tree "/" files)]
    (->File "/" "dir" file-tree-size "" file-tree-files)))

(defn part1
  [filename]
  (->> (get-command-lines filename)
       (map create-command)
       (reductions adjust-command-path (->Command "" "" ""))
       (rest)
       (filter #(= "ls" (:command %)))
       (map #(parse-ls-result (:result %) (:path %)))
       (flatten)
       (create-root-file-structure)
       (tree-seq #(= "dir" (:type %)) :children)
       (filter #(= "dir" (:type %)))
       (map :size)
       (filter #(<= % 100000))
       (reduce +)))

(defn part2
  [filename] 
  (let [root-file-structure
        (->> (get-command-lines filename)
             (map create-command)
             (reductions adjust-command-path (->Command "" "" ""))
             (rest)
             (filter #(= "ls" (:command %)))
             (map #(parse-ls-result (:result %) (:path %)))
             (flatten)
             (create-root-file-structure))
        space-available (- 70000000 (:size root-file-structure))]
    (println space-available)
    (->> root-file-structure
         (tree-seq #(= "dir" (:type %)) :children)
         (filter #(= "dir" (:type %)))
         (map :size)
         (sort)
         (drop-while #(>= 30000000 (+ % space-available)))
         (first))))



(defn -main
  "Part 1 & 2 of the 2022 Advent of Code Day 5"
  [& args]
  (println "Part 1 (Sample):" (part1 "sample-input.txt")
           "\nPart 1:" (part1 "input.txt")
           "\nPart 2 (Sample):" (part2 "sample-input.txt")
           "\nPart 2:" (part2 "input.txt")))
