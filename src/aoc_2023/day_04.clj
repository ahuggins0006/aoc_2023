(ns aoc-2023.day-04
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.dev :as dev]
            [malli.instrument :as mi]
            [malli.dev.pretty :as pretty]
            [malli.experimental :as mx]
            [aoc-2023.core :as core]))


;; Part 01
(def sample-data (core/read-file-lines "resources/day_04_sample.txt"))

sample-data
;; => ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
;;     "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
;;     "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
;;     "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
;;     "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
;;     "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

(str/split (first sample-data) #"\|")


(let [sample (first sample-data)
      remove-header (str/replace sample #"Card \d+\: " "")
      remove-whites (str/replace remove-header #"  "  " ")
      splitted (str/split remove-whites #"\|")
      winning (set (str/split (str/trim (first splitted)) #" "))
      mine  (set (str/split (str/trim (second splitted)) #" "))]
  (Math/pow 2 (dec (count (set/intersection winning mine)))))

(defn calc-winnings [card]
  (let [sample card
        remove-header (str/replace sample #"Card \d+\: " "")
        remove-whites (str/replace remove-header #"  "  " ")
        splitted (str/split remove-whites #"\|")
        winning (set (str/split (str/trim (first splitted)) #" "))
        mine  (set (str/split (str/trim (second splitted)) #" "))]
    (int (Math/pow 2 (dec (count (set/intersection winning mine)))))))

(calc-winnings (first sample-data))
;; => 8

(apply + (map calc-winnings sample-data))
;; => 13

(def input-data (core/read-file-lines "resources/day_04_input.txt"))

(apply + (map calc-winnings input-data))
;; => 26426

;; part two
(defn calc-copies [card]
  (let [sample (:numbers card)
        remove-header (str/replace sample #"Card \d+\: " "")
        remove-whites (str/replace remove-header #"  "  " ")
        splitted (str/split remove-whites #"\|")
        winning (set (str/split (str/trim (first splitted)) #" "))
        mine  (set (str/split (str/trim (second splitted)) #" "))]
    (range (inc (:card card)) (+ (inc (:card card)) (count (set/intersection winning mine))))))


(range 5 6)
(set/intersection #{59 21 44 53 1} #{69 14 82 21 1 63 72 16})
(calc-copies (make-card (first sample-data)))
;; => (2 3 4 5)
(calc-copies {:card 3, :copies 4, :numbers "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"})
(calc-copies {:card 4, :copies 8, :numbers "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"})
(set/intersection #{69 73 84 41 92} #{58 83 51 59 5 76 84 54})

(defn make-card [card-string]
  (let [sample card-string
        header (re-find #"Card\s+\d+\: " sample)
        card-number (re-find #"\d+" header)]
    {:card (Integer/parseInt card-number)
     :copies 1
     :numbers sample}))

(re-find #"Card \d+\: " (first sample-data))
(make-card (first sample-data))
;; => {:card 1, :copies 1, :numbers "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"}

(zipmap (range) (map make-card sample-data))
;; => {0 {:card 1, :copies 1, :numbers "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"}, 1 {:card 2, :copies 1, :numbers "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"}, 2 {:card 3, :copies 1, :numbers "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"}, 3 {:card 4, :copies 1, :numbers "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"}, 4 {:card 5, :copies 1, :numbers "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"}, 5 {:card 6, :copies 1, :numbers "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"}}



(def cards (zipmap (range) (map make-card sample-data)))

(:card (val (first cards)))

(assoc cards 0 (update (cards 0) :copies inc))
;; => {0 {:card 1, :copies 2, :numbers "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"}, 1 {:card 2, :copies 1, :numbers "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"}, 2 {:card 3, :copies 1, :numbers "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"}, 3 {:card 4, :copies 1, :numbers "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"}, 4 {:card 5, :copies 1, :numbers "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"}, 5 {:card 6, :copies 1, :numbers "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"}}


(defn update-cards [cards ids]
  (loop [i ids
         c cards]
    (if (empty? i) c
        (recur (rest i) (update-in c [(dec (first i)) :copies] inc)))))


(loop [c cards
       acc 0
       idx 0
       cnt (count cards)]
  (println [c acc])
  (cond (>= idx (dec cnt)) (+ acc (:copies (val (first c))))
        :else (let [copies (:copies (val (first c)))] (recur (dissoc (update-cards c (flatten (repeat copies (calc-copies (val (first c)))))) idx) (+ acc copies) (inc idx) cnt))))



(defn part-two [input-data]
  (loop [c (zipmap (range) (map make-card input-data))
         acc 0
         idx 0
         cnt (count input-data)]
    (println (val (first c)))
    (cond (>= idx (dec cnt)) (+ acc (:copies (val (first c))))
          :else (let [copies (:copies (val (first c)))] (recur (dissoc (update-cards c (flatten (repeat copies (calc-copies (val (first c)))))) idx) (+ acc copies) (inc idx) cnt)))))

(defn improved-part-two [input-data]
  (loop [c (into (sorted-map) (zipmap (range) (map make-card input-data)))
         acc 0]
    (let [h (first c)
          t (rest c)
          copies (:copies (val (first c)))]
      (cond (empty? t) (+ acc copies)
            :else (recur (into (sorted-map) (dissoc (update-cards c (flatten (repeat copies (calc-copies (val h))))) (dec (:card (val h))))) (+ acc copies))))))

(part-two sample-data)
;; => 30
(improved-part-two sample-data)
;; => 30
(time (improved-part-two sample-data))
(time (improved-part-two input-data))
(def f (future (improved-part-two input-data)))
(println @f)

