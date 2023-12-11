(ns aoc-2023.day-03
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
(def sample-data (core/read-file-lines "resources/day_03_sample.txt"))

sample-data
;; => ["467..114.."
;;     "...*......"
;;     "..35..633."
;;     "......#..."
;;     "617*......"
;;     ".....+.58."
;;     "..592....."
;;     "......755."
;;     "...$.*...."
;;     ".664.598.."]

(str/split (first sample-data) #"")
;; => ["4" "6" "7" "." "." "1" "1" "4" "." "."]
;; => ["4" "6" "7" "." "." "1" "1" "4" "." "."]


(nth (nth sample-data 1) 3)
;; => \*

(def matrix (mapv #(str/split % #"") sample-data))
;; => [["4" "6" "7" "." "." "1" "1" "4" "." "."]
;;     ["." "." "." "*" "." "." "." "." "." "."]
;;     ["." "." "3" "5" "." "." "6" "3" "3" "."]
;;     ["." "." "." "." "." "." "#" "." "." "."]
;;     ["6" "1" "7" "*" "." "." "." "." "." "."]
;;     ["." "." "." "." "." "+" "." "5" "8" "."]
;;     ["." "." "5" "9" "2" "." "." "." "." "."]
;;     ["." "." "." "." "." "." "7" "5" "5" "."]
;;     ["." "." "." "$" "." "*" "." "." "." "."]
;;     ["." "6" "6" "4" "." "5" "9" "8" "." "."]]

((matrix 1) 3);; => "*"

((matrix 0) 0)
;; => "4"
((matrix 0) 1)
;; => "6"
((matrix 0) 2)
;; => "7"
((matrix 1) 3);; => "*"
((matrix -1) -3);; => "*"

(defn matrix-get [matrix [x y]]

  ((matrix x) y))


;; TODO all numbers begin and end
;; TODO all symbols begin and end

(keyword "123")
(defn make-number [cache counter]
  {:number cache
   :begin (- counter (dec (count cache)))
   :end counter})

(make-number "123" 2)
;; => {:number "123", :begin 0, :end 2}
(let [{:keys [number begin end]} {:number "123", :begin 0, :end 2}]

  (subs number begin (inc end)))
;; => "123"

(make-number "467" 2)
;; => {:number "467", :begin 0, :end 2}
(let [{:keys [number begin end]} {:number "467", :begin 0, :end 2}]

  (subs number begin (inc end)))
;; => "467"

;; check adjacency?
(defn left-of [begin input]
  (if (not (neg? (dec begin))) (nth input (dec begin))
      false))

(defn right-of [end input]
  (if (< (inc end) (count input)) (nth input (inc end))
      false))

(count (first sample-data))
;; => 10
(count sample-data)
;; => 10
(defn above [boundary begin end input]
  (let [boundary boundary 
        above-begin (- begin boundary)
        above-end   (- end boundary)]
    (if (and (not (neg? above-begin)) (not (neg? above-end)))
      (subs input above-begin (inc above-end))
      false)))

(left-of 0 (apply str sample-data))
(above (count (first sample-data)) 0 2 (apply str sample-data))
;; => false

(defn below [boundary begin end input]
  (let [boundary boundary
        below-begin (+ begin boundary)
        below-end   (+ end boundary)]
    (if (and (< below-begin (count input)) (< below-end (count input))) (subs input below-begin (inc below-end))
        false)))

(defn top-right [boundary begin end input]
  (let [boundary boundary
        above (above boundary begin end input)
        right-of (right-of end input)]
    (if (and above right-of)
      (nth input (- (inc end) boundary))
      false)))

(defn top-left [boundary begin end input]
  (let [boundary boundary
        above (above boundary begin end input)
        left-of (left-of end input)]
    (if (and above left-of)
      (nth input (- (dec begin) boundary))
      false)))

(defn bottom-right [boundary begin end input]
  (let [boundary boundary
        below (below boundary begin end input)
        right-of (right-of end input)]
    (if (and below right-of)
      (nth input (+ (inc end) boundary))
      false)))

(defn bottom-left [boundary begin end input]
  (let [boundary boundary
        below (below boundary begin end input)
        left-of (left-of begin input)]
    (if (and below left-of)
      (nth input (+ (dec begin) boundary))
      false)))

(bottom-left (count (first sample-data)) 13 13 (apply str sample-data))
;; => \3
(bottom-left (count (first sample-data)) 0 2 (apply str sample-data))
;; => false

(bottom-left (count (first sample-data)) 2 2 (apply str sample-data))
;; => \.

(bottom-right (count (first sample-data)) 2 2 (apply str sample-data))
;; => \*
(top-left (count (first sample-data)) 13 13 (apply str sample-data))
;; => \7

(top-right (count (first sample-data)) 10 13 (apply str sample-data))
;; => \.

(top-right (count (first sample-data)) 10 10 (apply str sample-data))
;; => \6
(below (count (first sample-data)) 0 2 (apply str sample-data))
;; => "..."

(above (count (first sample-data)) 10 12 (apply str sample-data))
;; => "467"

(left-of 0 (first sample-data))
;; => false

(right-of 2 (first sample-data))
;; => \.

(let [bound (count (first sample-data))
      input (apply str sample-data)
      begin 0
      end   2
      left-of (left-of begin input)
      right-of (right-of end input)
      below (below bound begin end input)
      above (above bound begin end input)
      top-left (top-left bound begin end input)
      top-right (top-right bound begin end input)
      bottom-left (bottom-left bound begin end input)
      bottom-right (bottom-right bound begin end input)]
  [left-of right-of below above top-left top-right bottom-left bottom-right]
  )
;; => [false \. "..." false false false false \*]

(remove false? [false \. "..." false false false false \*])
;; => (\. "..." \*)

(apply str '(\. "..." \*))
;; => "....*"

(defn check-adjacent [boundary input {:keys [number begin end]}]
  (let [bound boundary
        input (apply str input)
        begin begin
        end   end
        left-of (left-of begin input)
        right-of (right-of end input)
        below (below bound begin end input)
        above (above bound begin end input)
        top-left (top-left bound begin end input)
        top-right (top-right bound begin end input)
        bottom-left (bottom-left bound begin end input)
        bottom-right (bottom-right bound begin end input)]
    {:number number
     :adjacency [left-of right-of below above top-left top-right bottom-left bottom-right]}))

(check-adjacent (count (first sample-data))(apply str sample-data) {:number "467" :begin 0 :end 2})
;; => {:number "467", :adjacency [false \. "..." false false false false \*]}

(find-numbers (apply str sample-data))
;; => [{:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97} {:number "467", :begin 103, :end 105} {:number "999", :begin 107, :end 109}]
(check-adjacent (count (first sample-data)) (apply str sample-data) {:number "35" :begin 22 :end 23})
;; => {:number "35", :adjacency [\. \. ".." ".*" \. \. \. \.]}
(def numbers (re-seq #"\d+" (apply str sample-data)))      
numbers
;; => ("467" "114" "35" "633" "617" "58" "592" "755" "664" "598")

(.indexOf (apply str sample-data) "467");; => 0
(.indexOf (apply str sample-data) "114")
;; => 5

(map (fn [a] { :begin (.indexOf (apply str sample-data) a)
               :end (+ )(dec (count a))}) '("467" "114" "35" "633" "617" "58" "592" "755" "664" "598"))
;; => ({:begin 0, :end 2} {:begin 5, :end 2} {:begin 22, :end 1} {:begin 26, :end 2} {:begin 40, :end 2} {:begin 57, :end 1} {:begin 62, :end 2} {:begin 76, :end 2} {:begin 91, :end 2} {:begin 95, :end 2})

;; TODO numbers->begin-end-map naive approach!
;; can't handle dupes or single or double digits
(defn numbers->begin-end-map [input numbers]
  (map (fn [a] (let [begin (.indexOf input a)
                     end   (+ begin (dec (count a)))] {:number a
                     :begin begin
                     :end end})) numbers))

(numbers->begin-end-map (apply str sample-data) numbers)
;; => ({:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97})

;; TODO replace numbers->begin-end-map with find-numbers
(defn find-numbers [input]
  (loop [in input
         begin 0
         end 0
         number ""
         numbers []
         counter 0]
    (let [f (str (first in))]
      (cond
        (empty? in) (if (seq number) (conj numbers {:number number :begin begin :end end}) numbers)
        (and (empty? number) (re-matches #"\d" f)) (recur (rest in) counter counter (str number f) numbers (inc counter))
        (and (seq number) (re-matches #"\d" f)) (recur (rest in) begin counter (str number f) numbers (inc counter))
        (and (seq number) (or (re-matches #"(?!\d|\.)." f) (re-matches #"\." f))) (recur (rest in) begin counter "" (conj numbers {:number number :begin begin :end end}) (inc counter))
        :else (recur (rest in) begin end number numbers (inc counter))))))
(defn improved-find-numbers
  "handles each row independently as so to avoid naively counting two wrap-around values a single number"
  [idx input]
  (loop [in input
         begin 0
         end 0
         number ""
         numbers []
         counter 0]
    (let [f (str (first in))
          offset (* idx (count input))]
      (cond
        (empty? in) (if (seq number) (conj numbers {:number number :begin begin :end end}) numbers)
        (and (empty? number) (re-matches #"\d" f)) (recur (rest in) (+ counter offset) (+ counter offset) (str number f) numbers (inc counter))
        (and (seq number) (re-matches #"\d" f)) (recur (rest in) begin (+ counter offset) (str number f) numbers (inc counter))
        (and (seq number) (or (re-matches #"(?!\d|\.)." f) (re-matches #"\." f))) (recur (rest in) begin (+ counter offset) "" (conj numbers {:number number :begin begin :end end}) (inc counter))
        :else (recur (rest in) begin end number numbers (inc counter))))))

(find-numbers (apply str (first sample-data)))
;; => [{:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7}]
(find-numbers (apply str (last sample-data)))
;; => [{:number "467", :begin 3, :end 5} {:number "9", :begin 9, :end 9}]
(find-numbers (apply str sample-data))
;; => [{:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97} {:number "467", :begin 103, :end 105} {:number "9", :begin 109, :end 109}]

(filter seq (mapcat find-numbers sample-data))
;; => ({:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 2, :end 3} {:number "633", :begin 6, :end 8} {:number "617", :begin 0, :end 2} {:number "58", :begin 7, :end 8} {:number "592", :begin 2, :end 4} {:number "755", :begin 6, :end 8} {:number "664", :begin 1, :end 3} {:number "598", :begin 5, :end 7} {:number "467", :begin 3, :end 5} {:number "999", :begin 7, :end 9})

(filter seq (flatten (map-indexed improved-find-numbers sample-data)))
;; => ({:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97} {:number "467", :begin 103, :end 105} {:number "999", :begin 107, :end 109})

(find-numbers (apply str sample-data))
;; => [{:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97} {:number "467", :begin 103, :end 105} {:number "999", :begin 107, :end 109}]
(= (map :number (filter seq (mapcat find-numbers sample-data))) (map :number (find-numbers (apply str sample-data))))
;; => false
(find-numbers (first sample-data))
(find-numbers (first sample-data))
;; => [{:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7}]
(find-numbers (second sample-data))
(pp/pprint (find-numbers (apply str sample-data)))
(map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent (count (first sample-data))(apply str sample-data) %)) (numbers->begin-end-map (apply str sample-data) numbers))
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....$.."} {:number "598", :adjacency "..*...."})

;; replace numbers->begin-end-map with find-numbers
(map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent (count (first sample-data)) (apply str sample-data) %)) (find-numbers (apply str sample-data)))
;; get all special chars

(re-seq #"(?!\d|\.)." (apply str sample-data))
;; => ("*" "#" "*" "+" "$" "*")
(set '("*" "#" "*" "+" "$" "*"));; => #{"*" "#" "+" "$"}

;; filter adjacent for special chars

(.indexOf "....*" "*")
(.indexOf "....*" "-")
(re-find #"\*" "....*")
(re-find (re-pattern "\\*|\\#|\\+|\\$") "....+")

(defn build-pattern [special-char-set]
  (subs (apply str (map #(str "|\\" %) special-char-set)) 1))

(re-pattern (build-pattern #{"*" "#" "+" "$"}))
;; => #"|\*|\#|\+|\$"

(re-find (re-pattern (build-pattern #{"*" "#" "+" "$"})) "....+")
;; => "+"
(def number-adjacency-sample (map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent (count (first sample-data)) (apply str sample-data) %)) (numbers->begin-end-map (apply str sample-data) numbers)))

number-adjacency-sample;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....$.."} {:number "598", :adjacency "..*...."})

(filter #(re-find (re-pattern (build-pattern #{"*" "#" "+" "$"})) (:adjacency %)) number-adjacency-sample)
;; => ({:number "467", :adjacency "....*"} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....$.."} {:number "598", :adjacency "..*...."})

(def experiment-3 (filter #(re-find (re-pattern (build-pattern #{"*" "#" "+" "$"})) (:adjacency %)) number-adjacency-sample))
(apply + (map (comp (fn [a] (Integer/parseInt a)) :number) experiment-3))
;; => 4361

;; algorithm?

;; calculate adjacency?
(map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent (count (first sample-data))(apply str sample-data) %)) (numbers->begin-end-map (apply str sample-data) numbers))
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....$.."} {:number "598", :adjacency "..*...."})

(defn calc-adjacency [input numbers]
  (let [input-as-str (apply str input)
        boundary (count (first input))
        ]
    (map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent boundary input-as-str %)) (numbers->begin-end-map input-as-str numbers))))
;(filter seq (mapcat find-numbers sample-data))
(defn calc-adjacency [input]
  (let [input-as-str (apply str input)
        boundary (count (first input))]
    (map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent boundary input-as-str %)) (filter seq (flatten (map-indexed improved-find-numbers input))))))

(calc-adjacency sample-data numbers)
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....$.."} {:number "598", :adjacency "..*...."})

(calc-adjacency sample-data)
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....4..$...6"} {:number "598", :adjacency "..7..*....6."} {:number "467", :adjacency "..4.569"} {:number "9", :adjacency "..."})
(pp/pprint (calc-adjacency sample-data numbers))

;; filter adjacent for special chars?
(filter #(re-find (re-pattern (build-pattern #{"*" "#" "+" "$"})) (:adjacency %)) number-adjacency-sample)

(defn filter-special-chars [special-char-set number-adjacency-sample]
  (filter #(re-find (re-pattern (build-pattern special-char-set)) (:adjacency %)) number-adjacency-sample))

(filter-special-chars #{"*" "#" "+" "$"} (calc-adjacency sample-data numbers))

(filter-special-chars #{"*" "#" "+" "$"} (calc-adjacency sample-data))
;; sum numbers?

(apply + (map (comp (fn [a] (Integer/parseInt a)) :number) experiment-3))

(defn sum-part-numbers [part-numbers]
  (apply + (map (comp (fn [a] (Integer/parseInt a)) :number) part-numbers)))

(sum-part-numbers (filter-special-chars #{"*" "#" "+" "$"} (calc-adjacency sample-data numbers)))
;; => 4361

(defn algorithm [sample-data numbers special-chars]
  (as-> sample-data s
      (calc-adjacency s numbers)
      ;(filter-special-chars special-chars s)
      ;(sum-part-numbers s)
      )
  
  )
(algorithm sample-data numbers #{"*" "#" "+" "$"})
;; => 4361

(defn algorithm [sample-data special-chars]
  (as-> sample-data s
    (calc-adjacency s)
    (filter-special-chars special-chars s)
    (sum-part-numbers s)
    ))

(algorithm sample-data #{"*" "#" "+" "$"})
;; => 4361
(def input-data (core/read-file-lines "resources/day_03_input.txt"))
;; find numbers
(def solution-numbers (re-seq #"\d+" (apply str input-data)))      
;; find special chars
(def special-chars (set (re-seq #"(?!\d|\.)." (apply str input-data))))
special-chars
;; => #{"=" "*" "%" "/" "-" "&" "#" "+" "$" "@"}

;;solution?

(algorithm input-data solution-numbers special-chars)
;; => 1159375

(algorithm input-data special-chars)
;; => 527144
(pp/pprint (algorithm input-data special-chars))
(def experiment-4 (take 3 input-data))

experiment-4
;; => (".....613...................................439............498.........................438......617....343.............942..................."
;;     ".......*............790...........269..735..*........................../679..............*444.*.........*.......147...*.............441....."
;;     "....539......422.......*......662*........*..691..........*124.15..675.................=.......404...872............237......930.....+......")

(calc-adjacency experiment-4 solution-numbers)
(pp/pprint (calc-adjacency (take 2 input-data) (re-seq #"\d+" (apply str (take 2 input-data)))))
(pp/pprint (filter-special-chars special-chars (calc-adjacency (take 2 input-data) (re-seq #"\d+" (apply str (take 2 input-data))))))
(pp/pprint (filter-special-chars special-chars (calc-adjacency experiment-4 (re-seq #"\d+" (apply str experiment-4)))))
(pp/pprint (algorithm input-data solution-numbers special-chars))

(pp/pprint (filter-special-chars special-chars (calc-adjacency experiment-4 )))
(type (last input-data))
(pp/pprint (calc-adjacency [(last input-data)]))
(type sample-data)
(pp/pprint (calc-adjacency sample-data))
(pp/pprint (algorithm input-data special-chars))
(count solution-numbers)
;; => 1186
(count (calc-adjacency input-data))
;; => 1186
(count (algorithm input-data special-chars))
;; => 1187
(sum-part-numbers (calc-adjacency input-data))
;; => 1221515

(sum-part-numbers (filter-special-chars special-chars (calc-adjacency input-data)))
;; => 1161843

