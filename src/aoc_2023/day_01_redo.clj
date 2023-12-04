(ns aoc-2023.day-01-redo
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [malli.core :as m]
            [malli.dev :as dev]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]
            [malli.experimental :as mx]
            [aoc-2023.core :as core]))

;; Part 01

(def sample-data (core/read-file-lines "resources/day_01_sample.txt"))

sample-data
;; => ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]

(map #(re-seq #"\d" %) sample-data)
;; => (("1" "2") ("3" "8") ("1" "2" "3" "4" "5") ("7"))

(defn first-last->digit
  "takes a seq of strings of single digits and returns an integer or zero in case of error"
  {:malli/schema [:=> [:cat [:sequential [:string {:min 1 :max 1}]]] :int]
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
  (reduce + (map (comp first-last->digit  #(re-seq #"\d" %)) input)))

;; TODO test
(solution1 input);; => 53334
(time (solution1 input));; => 53334
;; => 53334

(comment (run-tests)
         (dev/start! {:report (pretty/reporter)})
         (mi/check)
         )

(comment
  (dev/stop!))


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
