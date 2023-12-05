(ns aoc-2023.day-01-redo
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is run-tests]]
            [malli.core :as m]
            [malli.dev :as dev]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]
            [malli.experimental :as mx]
            [aoc-2023.core :as core]))

;; Part 01

(def sample-data (core/read-file-lines "resources/day_01_sample.txt"))

(comment
  sample-data
;; => ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]

  (map #(re-seq #"\d" %) sample-data))
;; => (("1" "2") ("3" "8") ("1" "2" "3" "4" "5") ("7"))

(defn first-last->digit
  "takes a seq of strings of single digits and returns an integer or zero in case of error"
  {:malli/schema [:=> [:cat [:or [:sequential [:string {:min 1 :max 1}]]  nil?]] :int]
   :test (fn [] (and (is (= (first-last->digit '("1" "2")) 12))
                     (is (= (first-last->digit '("A" "2")) 0))))}
  [numbers]
  (let [f (first numbers)
        l (last numbers)
        fl (str f l)]
    (cond
      (or (empty? fl) (nil? fl))      0
      (not (re-matches #"\d\d" fl))    0
      :else (Integer/parseInt fl))))

(comment (reduce + (map (comp first-last->digit  #(re-seq #"\d" %)) sample-data)))
;; => 142

(def input (core/read-file-lines "resources/day_01_input.txt"))

(defn input->digits
  "takes a seq of strings and returns a seq of ints"
  {:malli/schema [:=> [:cat [:sequential string?]] [:sequential int?]]
   :test (fn [] (is (= (input->digits ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]) '(12 38 15 77))))}
  [input]
  (->> input
       (map (comp first-last->digit  #(re-seq #"\d" %)))))


(defn solution1
  "takes problem input as seq of strings and returns integer"
  {:malli/schema [:=> [:cat [:sequential string?]] :int]
   :test (fn [] (is (= (solution1 ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]) 142)))}
  [input]
  (reduce + (input->digits input)))

(comment (solution1 input));; => 53334


;; part 2

(def sample-data2
  ["two1nine"
   "eightwothree"
   "abcone2threexyz"
   "xtwone3four"
   "4nineeightseven2"
   "zoneight234"
   "7pqrstsixteen"])

(def digit-text->number
  {"one"  "1"
   "two"  "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six"  "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn find-hits
  " [input hits] 
      finds idxs of occurrences of a number in a string returns the seq of idxs
      input is a target input string and the seq of digits
    [input hits acc acc2] 
      recurrence that takes the input and hits list along with initialized acc and acc2"
  {:malli/schema [:function [:=> [:cat :string [:sequential string?]] [:or [:sequential int?] [:sequential empty?]]]
                  [:=> [:cat :string [:sequential string?] [:int {:min 0}] [:sequential int?]] [:or [:sequential int?] [:sequential empty?]]]]

   :test (fn [] (and (is (= (find-hits "eight5one43nmkxdseight5" '("5" "5")) [5 22]))
                     (is (= (find-hits "eight5one43nmkxdseight5" '("eight" "5")) [0 5]))
                     (is (= (find-hits "eight5one43nmkxdseight5" nil) []))
                     (is (= (find-hits "eight5one43nmkxdseight5" []) []))
                     (is (= (find-hits "eight5one43nmkxdseight5" '("A" "5")) []))
                     (is (= (find-hits "eight5one43nmkxdseight5" '(nil "5")) []))
                     (is (= (find-hits "eight5one43nmkxdseight5" '("A" "5")) []))))}

  ([input hits] (cond
                  (or (empty? input) (empty? hits)) []
                  (or (nil? input) (nil? hits))     []
                  :else (find-hits input hits 0 [])))

  ([input hits acc acc2]
   (let [f (first hits)]
     (cond (empty? hits) acc2
           (nil? f)      []
           (nil? (str/index-of input f acc)) []
           :else (recur input (rest hits) (+ 1 (str/index-of input f acc)) (conj acc2 (str/index-of input f acc)))))))



(m/validate {:schema
           [:function [:=> [:cat :string [:sequential string?]] [:or [:sequential int?] [:sequential empty?]]] [:=> [:cat :string [:sequential string?] [:int {:min 0}] [:sequential int?]] [:or [:sequential int?] [:sequential empty?]]]],
           :ns aoc-2023.day-01-redo,
           :name find-hits})

(defn normalize-words [input]
  (loop [s input
         acc {}
         digits digit-text->number]
    (let [digit (first digits)]
      (cond
        (empty? digits)  acc
        (seq? (re-seq (re-pattern (first digit)) s)) (let [hits (re-seq (re-pattern (first digit)) s)
                                                           hits->digits (map #(digit-text->number %) hits)
                                                           hit-map-idxs (find-hits s hits)]
                                                       (println [s hits hits->digits hit-map-idxs])
                                                       (recur s (conj acc (zipmap hit-map-idxs hits->digits)) (rest digits)))
        :else (recur s acc (rest digits))))))

(comment (run-tests)
         (dev/start! {:report (pretty/reporter)})
         (pp/pprint (mi/check))
         (pp/pprint (map keys (vals (m/function-schemas)))))
(comment
  (map normalize-words sample-data2)
;; => ({0 "2", 4 "9"} {7 "3", 4 "2", 0 "8"} {7 "3", 3 "1"} {1 "2", 3 "1", 7 "4"} {10 "7", 5 "8", 1 "9"} {3 "8", 1 "1"} {6 "6"})

  (map normalize-words sample-data))
;; => ({} {} {} {})

(defn normalize-digits [input]
  (loop [s input
         acc {}
         digits (re-seq #"\d" input)]
    (let [digit (first digits)]
      (cond
        (empty? digits) acc
        (seq? (re-seq (re-pattern digit) s)) (let [hits (re-seq (re-pattern digit) s)
                                                   hit-map-idxs (find-hits s hits)]
                                               (recur s (conj acc (zipmap hit-map-idxs hits)) (rest digits)))

        :else (recur s acc (rest digits))))))

(comment
  (map normalize-digits sample-data2)
;; => ({3 "1"} {} {6 "2"} {6 "3"} {0 "4", 15 "2"} {8 "2", 9 "3", 10 "4"} {0 "7"})
  (apply str (vals (sort-by key < (merge (normalize-words "2eightwothree") (normalize-digits "2eightwothree")))))
;; => "2823"
  (solution1 '("2823"))
;; => 23
)

(defn normalize [input]
  (apply str (vals (sort-by key < (merge (normalize-words input) (normalize-digits input))))))

(comment
  (map normalize ["eight5one43nmkxdseight5"])
;; => ("8514385")

  (solution1 (map normalize ["eight5one43nmkxdseight5"]))
;; => 85

  (solution1 (map normalize ["twotthreewo22"])))
;; => 22

(defn solution2 [input]
  (->> input
       (map normalize)
       solution1
       )
  )


(comment (solution2 input))
;; => 52834

(comment
  (dev/stop!)
  )
