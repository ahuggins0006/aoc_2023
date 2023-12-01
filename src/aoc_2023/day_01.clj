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

(defn solution [input]
  (reduce + (map (comp first-last->digit string->numbers) input)))

(solution input)
;; => 53334

;; Part 02
