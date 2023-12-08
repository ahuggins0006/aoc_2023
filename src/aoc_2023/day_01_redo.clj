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


(defn find-all-idxs
  "takes input string along with targets to produce a map from the target's idx in input string to the target itself"
  ;; (find-all-idxs " eight5one43nmkxdseight5 " '(" 5 " " 5 ")) ;; => {5 " 5 ", 22 " 5 "}
  {:malli/schema [:=> [:cat :string [:sequential [:string]]] [:map-of int? string?]]
   :test (fn [] (and (is (= (find-all-idxs "eight5one43nmkxdseight5" '("5" "5")) {5 "5", 22 "5"}))
                     (is (= (find-all-idxs "eight5one43nmkxdseight5" nil) {}))
                     (is (= (find-all-idxs "eight5one43nmkxdseight5" []) {}))
                     (is (= (find-all-idxs "eight5one43nmkxdseight5" '(nil "5")) {}))
                     (is (= (find-all-idxs "eight5one43nmkxdseight5" '("A" "5")) {}))))}

  [input targets]
  (loop [t targets
         ctr (zipmap t (repeat (count t) 0)) ;; account for duplicate hits
         acc {}]
    (let [head (first t)
          from-idx (ctr head)]
      (cond
        (empty? t) acc
        (not (and head from-idx)) acc
        (not (str/index-of input head from-idx)) acc
        :else (recur (rest t)
                     (assoc ctr head (inc (str/index-of input head from-idx)))
                     (conj acc {(str/index-of input head from-idx) head}))))))

(comment (find-all-idxs "eight5one43nmkxdseight5" '("5" "5"))
;; => {5 "5", 22 "5"}
         (find-all-idxs "555" '("5" "5" "5"))
;; => {0 "5", 1 "5", 2 "5"}
         (find-all-idxs "eight5one43nmkxdseight5" '(nil "5")))


(defn digitize
  "string input returns {index, digit} for one - nine"
  {:malli/schema [:=> [:cat string?] [:map-of :int :string]]

   :test (fn [] (and (is (= (digitize "xtwone3four") {1 "2", 3 "1", 7 "4"}))
                     (is (= (digitize nil) {}))
                     (is (= (digitize "") {}))))}
  [input]
  (cond
    (nil? input) {}
    (empty? input) {}
    :else (let [input input
                only-keys (keys digit-text->number)
                only-vals (vals digit-text->number)
                hits (flatten (remove nil? (for [d (concat only-keys only-vals)]
                                             (re-seq (re-pattern d) input))))
                idxs (find-all-idxs input hits)

                words->digits  (for [i idxs] (if (not (re-find #"\d" (second  i)))
                                               {(first i) (digit-text->number (second i))}
                                               {(first i) (second i)}))]

            (apply merge words->digits))))

(comment
  (digitize nil)
  (digitize "")
  (map digitize sample-data2)
;; => ({0 "2", 4 "9"} {7 "3", 4 "2", 0 "8"} {7 "3", 3 "1"} {1 "2", 3 "1", 7 "4"} {10 "7", 5 "8", 1 "9"} {3 "8", 1 "1"} {6 "6"})

  (map digitize sample-data)
  )


(map digitize ["eight5one43nmkxdseight5"])
;; => ({0 "8", 17 "8", 6 "1", 10 "3", 5 "5", 22 "5", 9 "4"})


(comment (run-tests)
         (dev/start! {:report (pretty/reporter)})
         (pp/pprint (mi/check))
         (pp/pprint (map keys (vals (m/function-schemas)))))

(map (comp #(apply str %) vals #(sort-by key < %) digitize) sample-data2)
;; => ("219" "823" "123" "2134" "49872" "18234" "76")

(defn normalize [input]
  (map (comp #(apply str %) vals #(sort-by key < %) digitize) input))

(normalize nil)
(comment
  (normalize ["eight5one43nmkxdseight5"])
;; => ("8514385")

  (solution1 (normalize ["eight5one43nmkxdseight5"]))
;; => 85

  (solution1 (normalize ["twotthreewo22"])))
;; => nil
;; => 22

(defn solution2 [input]
  (->> input
       normalize
       solution1
       )
  )


(comment (solution2 input))
;; => 52834

(comment
  (dev/stop!)
  )
