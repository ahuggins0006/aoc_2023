(ns aoc-2023.day-01
  (:require [clojure.string :as str]
            [aoc-2023.core :as core]
            ))


;; Part 01

(def sample-data (core/read-file-lines "resources/day_01_sample.txt"))

sample-data
;; => ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]


(try (Integer/parseInt "1")
     (catch Exception e)
     )
;; => 1

(try (Integer/parseInt "a")
     (catch Exception e))
;; => nil

(defn digit? [c]
  (try (Integer/parseInt (str c))
       (catch Exception e)))

(remove nil? (map digit? "a1"))
;; => (1)

(remove nil? (map digit? "1abc2"))
;; => (1 2)


(filter number? (map digit? "1abc2"))
;; => (1 2)


(defn string->numbers [s]
  (filter number? (map digit? s)))

(string->numbers "1abc2")
;; => (1 2)

(map string->numbers sample-data)
;; => ((1 2) (3 8) (1 2 3 4 5) (7))


(Integer/parseInt (str (first [1 2]) (last [1 2])))
;; => 12

(defn first-last->digit [numbers]
  (Integer/parseInt (str (first numbers) (last numbers))))

(first-last->digit '(1 2))
;; => 12

(first-last->digit '(7))
;; => 77

(reduce + (map (comp first-last->digit string->numbers) sample-data))
;; => 142


(def input (core/read-file-lines "resources/day_01_input.txt"))

(reduce + (map (comp first-last->digit string->numbers) input))
;; => 53334

(defn solution1 [input]
   (map (comp first-last->digit string->numbers) input))

(solution1 sample-data)
;; => (12 38 15 77)

(reduce + (solution1 input))
;; => 53334

;; Part 02

(def sample-data2
  ["two1nine"
   "eightwothree"
   "abcone2threexyz"
   "xtwone3four"
   "4nineeightseven2"
   "zoneight234"
   "7pqrstsixteen"])

(def digit-text->number
  {
   "one"  "1"
   "two"  "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six"  "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"
   }
  )

(digit-text->number "one")
;; => "1"


(str/replace "one2three" (re-pattern "one") "1")
;; => "12three"

(str/replace "12three" (re-pattern "six") "6")
;; => "12three"

(for [digit digit-text->number]
  (str/replace "one2three" (re-pattern (first digit)) (second digit)))
;; => ("one2three" "one23" "one2three" "one2three" "one2three" "one2three" "one2three" "12three" "one2three" "one2three")
;; not quite

(loop [digits digit-text->number
       s "one2three"]
  (let [digit (first digits)]
    (println digit)
    (if (not-empty digits)
      (recur (rest digits)
             (str/replace s (re-pattern (first digit)) (second digit)))
      s
      )))
;; => "123"

(loop [s "2eightwothree"
       acc ""
       acc2 ""]
  (println acc)
  (cond
    (empty? s) acc2
    (contains? digit-text->number acc) (recur (rest s)
                                              (first s)
                                              (str acc2 (digit-text->number acc)))
    :else (recur (rest s) (str acc (first s)) acc2)))
;; => "8wothree"

(loop [s "2eightwothree"
       acc {}
       digits digit-text->number]
  (let [digit (first digits)]
    (println digit)
    (cond
      (empty? digits)  acc
      (re-find (re-pattern (first digit)) s) (recur s (conj acc {(str/index-of s (first digit)) (second digit)}) (rest digits))
      :else (recur s acc (rest digits)))))
;; => {8 "3", 5 "2", 1 "8"}

(filter number? (map digit? "2eightwothree"))
;; => (2)

(loop [digits (re-seq #"\d+" "2eightwothree")
       acc {8 "3", 5 "2", 1 "8"}]
  (cond
    (empty? digits) acc
    (re-find (re-pattern (first digits)) "2eightwothree") (recur (rest digits) (conj acc {(str/index-of "2eightwothree" (first digits)) (first digits)}))

    :else (recur (rest digits) acc)))
;; => {8 "3", 5 "2", 1 "8", 0 "2"}

(sort-by key < {8 "3", 5 "2", 1 "8", 0 "2"})
;; => ([0 "2"] [1 "8"] [5 "2"] [8 "3"])
(re-seq #"\d+" "2eightwothree")
;; => ("2")
(re-find (re-pattern "eight") "2eightwothree")
;; => "eight"

(re-find (re-pattern "two") "2eightwothree")
(re-find (re-pattern "three") "2eightwothree")
(re-find (re-pattern "four") "2eightwothree")
(str/index-of "2eightwothree" "two")
;; => 5
(contains?  digit-text->number "eight")
(for [s "2eightwothree"
      ]
  (str s (rest s))
  )

(defn normalize [input]
  (loop [digits digit-text->number
         s input]
    (let [digit (first digits)]
      (if (not-empty digits)
        (recur (rest digits)
               (str/replace s (re-pattern (first digit)) (second digit)))
        s))))

(normalize "8wothree")
;; => "8wo3"
 (defn hit-map->index-of [hit-map]
  (for [h hit-map]
    #(str/index-of  % (second h) (first h))))

(defn find-hits
  ([input hits] (find-hits input hits 0 []))
  ([input hits acc acc2]
   (println [input hits acc acc2])
   (if (empty? hits) acc2
       (recur input (rest hits) (+ 1 (str/index-of input (first hits) acc)) (conj acc2 acc)))))

(defn normalize-words [input]
  (loop [s input
         acc {}
         digits digit-text->number]
    (let [digit (first digits)]
      (cond
        (empty? digits)  acc
        (seq? (re-seq (re-pattern (first digit)) s)) (let [hits (re-seq (re-pattern (first digit)) s)
                                                           hits->digits (map #(digit-text->number %) hits)
                                                           hit-map (zipmap (find-hits input hits) hits)
                                                           hit-map-idxs (for [f (hit-map->index-of hit-map)]
                                                                          (f s))]
                                                       (recur s (conj acc (zipmap hit-map-idxs hits->digits)) (rest digits)))
        :else (recur s acc (rest digits))))))

(seq? (re-seq (re-pattern (first (first digit-text->number))) "1abc2"))
(map normalize-words sample-data)
;; => ({} {} {} {})

(str/index-of "eight5one43nmkxdseight5" "5" )
;; => 5

(find-hits "eight5one43nmkxdseight5" '("5" "5"))
;; => [0 6]
(find-hits "eight5one43nmkxdseight5" '("4"))
;; => [0]
(find-hits "2eightwothree" '("2"))
;; => [0]
(defn normalize-digits [input]
  (loop [s input
         acc {}
         digits (re-seq #"\d" input)]
    (let [digit (first digits)]
      (cond
        (empty? digits) acc
        (seq? (re-seq (re-pattern digit) s)) (let [hits (re-seq (re-pattern digit) s)
                                                   hit-map (zipmap (find-hits input hits) hits)
                                                   hit-map-idxs (for [f (hit-map->index-of hit-map)]
                                                                  (f s))]
                                               (recur s (conj acc (zipmap hit-map-idxs hits)) (rest digits)))

        :else (recur s acc (rest digits))))))



(map normalize-digits sample-data)
;; => ({0 "1", 4 "2"} {3 "3", 7 "8"} {1 "1", 3 "2", 5 "3", 7 "4", 9 "5"} {4 "7"})
(re-seq #"\d" "eight5one43nmkxdseight5")
;; => ("5" "4" "3" "5")
(normalize-digits "eight5one43nmkxdseight5")
;; => {5 "5", 9 "4", 10 "3"}
;; => {5 "5", 9 "4", 10 "3"}
(apply str (vals (sort-by key < (merge (normalize-words "2eightwothree") (normalize-digits "2eightwothree")))))
;; => "2823"
;; => "2823"

(solution1 ["2823"])
;; => (23)






(defn solution2 [input]
  (apply str (vals (sort-by key < (merge (normalize-words input) (normalize-digits input))))))


(map solution2 ["eight5one43nmkxdseight5"])
;; => ("8514385")
(re-seq #"eight" "eight5one43nmkxdseight5")
;; => ("eight" "eight")

(re-seq #"eight" "eight5one43nmkxdseight5")
;; => ("eight" "eight")
(zipmap (range (count '("eight" "eight"))) '("eight" "eight"))
;; => {0 "eight", 1 "eight"}
(defn hit-map->index-of [hit-map]
  (for [h hit-map]
    #(str/index-of  % (second h) (first h))))

((first (hit-map->index-of {0 "eight", 1 "eight"})) "eight5one43nmkxdseight5")

(for [f  (hit-map->index-of {0 "eight", 1 "eight"})]
  (f "eight5one43nmkxdseight5"))
;; => (0 17)

(loop [s  "eight5one43nmkxdseight5"
       acc {}
       digits digit-text->number]
  (let [digit (first digits)]

    (cond
      (empty? digits)  acc
      (seq? (re-seq (re-pattern (first digit)) s)) (let [hits (re-seq (re-pattern (first digit)) s)
                                                         hit-map (zipmap (range (count hits)) hits)
                                                         hit-map-idxs (for [f (hit-map->index-of hit-map)]
                                                                        (f s))] (recur s (conj acc (zipmap hit-map-idxs hits)) (rest digits)))
      :else (recur s acc (rest digits)))))
;; => {0 "eight", 17 "eight", 6 "one"}





((comp solution1 #(map solution2 %)) sample-data)
;; => (12 38 15 77)
(reduce + ((comp solution1 #(map solution2 %)) sample-data))
;; => 142
((comp solution1 #(map solution2 %)) sample-data2)
;; => (29 83 13 24 42 14 76)
(reduce + ((comp solution1 #(map solution2 %)) sample-data2))
;; => 281

(reduce + ((comp solution1 #(map solution2 %)) input))
;; => 52853


(solution1 (map solution2 ["ffjqtrjtdftwosix5zc5six5"]))
;; => "26555"

(solution1 (map solution2 ["eight5one43nmkxdseight5"]))
;; => (85)

(solution1 (map solution2 ["1tkzn"]))
;; => (49)

(reduce + (solution1 (map solution2 input)))
;; => 52828

(reduce + (solution1 (map solution2 sample-data2)))
;; => 281

(reduce + (solution1 (map solution2 sample-data)))
;; => 142
