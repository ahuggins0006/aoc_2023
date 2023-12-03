(ns aoc-2023.day-01-redo
  (:require [clojure.string :as str]
            [malli.dev :as dev]
            [malli.dev.pretty :as pretty]
            [malli.experimental :as mx]
            [aoc-2023.core :as core]))

;; TODO write specs 
;; Part 01

(def sample-data (core/read-file-lines "resources/day_01_sample.txt"))

(map #(re-seq #"\d" %) sample-data)
;; => (("1" "2") ("3" "8") ("1" "2" "3" "4" "5") ("7"))

(defn first-last->digit
  "takes a seq of strings of single digits and returns the double digit integer"
  {:malli/schema [:=> [:cat [:sequential [:string {:max 1}]]] :int]}
  [numbers]
  (Integer/parseInt (str (first numbers) (last numbers))))

(first-last->digit '("1" "2"))
;; => 12

(first-last->digit '("1" "21"))
;; => 121
(first-last->digit 1)
;; => 
(reduce + (map (comp first-last->digit  #(re-seq #"\d" %)) sample-data))
;; => 142

(def input (core/read-file-lines "resources/day_01_input.txt"))

(defn input->digits
  "takes a seq of strings and returns a seq of ints"
  {:malli/schema [:=> [:cat [:sequential string?]] [:sequential int?]]}
  [input]
  (->> input
       (map (comp first-last->digit  #(re-seq #"\d" %)))))

(input->digits sample-data)
;; => (12 38 15 77)


(defn solution1
  "takes problem input as seq of strings and returns integer"
  {:malli/schema [:=> [:cat [:sequential string?]] :int]}
  [input]
  (reduce + (map (comp first-last->digit  #(re-seq #"\d" %)) input)))

(solution1 input);; => 53334
(time (solution1 input));; => 53334

(dev/start! {:report (pretty/reporter)})

(comment
  (dev/stop!))
