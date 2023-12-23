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
;; => false
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
     :adjacency [left-of right-of below above top-left top-right bottom-left bottom-right]
     }))


(check-adjacent (count (first sample-data))(apply str sample-data) {:number "467" :begin 0 :end 2})
;; => {:number "467", :adjacency [false \. "..." false false false false \*]}

(check-adjacent (count (first sample-data)) (apply str sample-data) {:number "35" :begin 22 :end 23})
;; => {:number "35", :adjacency [\. \. ".." ".*" \. \. \. \.]}

(check-adjacent (count (first sample-data)) (apply str sample-data) {:number "*" :begin 13 :end 13})
;; => {:number "*", :adjacency [\. \. "5" "." \7 \. \3 \.]}
(def numbers (re-seq #"\d+" (apply str sample-data)))      
numbers
;; => ("467" "114" "35" "633" "617" "58" "592" "755" "664" "598")

(defn find-gear [adjacency]

  (map #(re-matches #"\*" %) adjacency))



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


(filter seq (flatten (map-indexed improved-find-numbers sample-data)))
;; => ({:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97} {:number "467", :begin 103, :end 105} {:number "999", :begin 107, :end 109})

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

;; algorithm?

;; calculate adjacency?

(map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent (count (first sample-data)) (apply str sample-data) %)) (filter seq (flatten (map-indexed improved-find-numbers sample-data))))
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "...+4..$...6"} {:number "598", :adjacency "..7.9*....69"} {:number "467", :adjacency "+.4.569"} {:number "999", :adjacency ".8..9"})

(defn calc-adjacency [input]
  (let [input-as-str (apply str input)
        boundary (count (first input))]
    (map (comp #(update % :adjacency (fn [a] (apply str (remove false? a)))) #(check-adjacent boundary input-as-str %)) (filter seq (flatten (map-indexed improved-find-numbers input))))))

(calc-adjacency sample-data)
;; => ({:number "467", :adjacency "....*"} {:number "114", :adjacency "......."} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "58", :adjacency ".........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "....4..$...6"} {:number "598", :adjacency "..7..*....6."} {:number "467", :adjacency "..4.569"} {:number "9", :adjacency "..."})


(defn filter-special-chars [special-char-set number-adjacency-sample]
  (filter #(re-find (re-pattern (build-pattern special-char-set)) (:adjacency %)) number-adjacency-sample))

(filter-special-chars #{"*" "#" "+" "$"} (calc-adjacency sample-data))
;; => ({:number "467", :adjacency "....*"} {:number "35", :adjacency ".....*...."} {:number "633", :adjacency "..#........."} {:number "617", :adjacency ".*.........."} {:number "592", :adjacency ".........+.."} {:number "755", :adjacency "..........*."} {:number "664", :adjacency "...+4..$...6"} {:number "598", :adjacency "..7.9*....69"} {:number "467", :adjacency "+.4.569"})

;; sum numbers?
(defn sum-part-numbers [part-numbers]
  (apply + (map (comp (fn [a] (Integer/parseInt a)) :number) part-numbers)))

(sum-part-numbers (filter-special-chars #{"*" "#" "+" "$"} (calc-adjacency sample-data)))


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
;; find special chars
(def special-chars (set (re-seq #"(?!\d|\.)." (apply str input-data))))
special-chars
;; => #{"=" "*" "%" "/" "-" "&" "#" "+" "$" "@"}

;;solution?

(algorithm input-data special-chars)
;; => 527144

;; Part 2

;; check adjacency should add a gear field with the gears location
;; then check each number for corresponding gears or not.


;; algorithm?
;; find gears (coordinates)
;; get sector (lines above and below target gear)
;; check adjacency
;; calulate gear ratio for the gear if valid gear


;; find gears
(.indexOf "*" "*")
(loop [l sample-data
       ctr 0]
  (cond
    (empty? l) (println "done")
    :else (do (println [(.indexOf (first l) "*") ctr]) (recur (rest l) (inc ctr)))))

(def gears
  (loop [l sample-data
         ctr 0
         acc []]
    (cond
      (empty? l) (filter #(pos? (first %)) acc)
      :else (recur (rest l) (inc ctr) (conj acc [(.indexOf (first l) "*") ctr])))))

(keep-indexed #(when (=  %2 \*) %) "***")
;; => (0 1 2)
(keep-indexed #(when (=  %2 \*) %) "*a*b*c")
(defn idxs-of [s target]
  (keep-indexed #(if (=  %2 target) % -1) s))
gears
(comment (defn find-gears-deprecated [input]
           (loop [l input
                  ctr 0
                  acc []]
             (cond
               (empty? l) (filter #(pos? (first %)) acc)
               :else (recur (rest l) (inc ctr) (conj acc [(.indexOf (first l) "*") ctr]))))))

(defn find-gears [input]
  (loop [l input
         ctr 0
         acc []]
    (cond
      (empty? l) (filter #(pos? (first %)) acc)
      :else (recur (rest l) (inc ctr) (concat acc (for [i (idxs-of (first l) \*)] [i ctr]))))))

(find-gears sample-data)
;; get sector
(defn gear->sector [[_ y] input]

  (cond (and (> y 0) (< y (dec (count input))))
        [(input (dec y))
         (input y)
         (input (inc y))]
        (zero? y) [nil (input y) (input (inc y))]
        (= y (dec (count input))) [(input (dec y)) (input y) nil]
        ))


(gear->sector [3 1] sample-data)
;; => ["467..114.." "...*......" "..35..633."]


;; check adjacency
(defn gear-adjacency [[x y]]
  (let [adjacency {:top-left [(dec x) (dec y)]
                   :top [x (dec y)]
                   :top-right [(inc x) (dec y)]
                   :right [(inc x) y]
                   :bottom-right [(inc x) (inc y)]
                   :bottom [x (inc y)]
                   :bottom-left [(dec x) (inc y)]
                   :left [(dec x) y]}]
    adjacency
    ))

(gear-adjacency [3 1])
;; => {:top-left [2 0], :top [3 0], :top-right [4 0], :right [4 1], :bottom-right [4 2], :bottom [3 2], :bottom-left [2 2], :left [2 1]}



(defn xy->char [input [x y]]
  (str (nth (input y) x)))

(xy->char sample-data [2 0])
;; => "7"

(filter #(re-matches #"\d" (xy->char sample-data (second %))) (gear-adjacency [3 1]))
;; => ([:top-left [2 0]] [:bottom [3 2]] [:bottom-left [2 2]])

(filter seq (flatten (map-indexed improved-find-numbers sample-data))) 
;; => ({:number "467", :begin 0, :end 2} {:number "114", :begin 5, :end 7} {:number "35", :begin 22, :end 23} {:number "633", :begin 26, :end 28} {:number "617", :begin 40, :end 42} {:number "58", :begin 57, :end 58} {:number "592", :begin 62, :end 64} {:number "755", :begin 76, :end 78} {:number "664", :begin 91, :end 93} {:number "598", :begin 95, :end 97})

(def sample-numbers (filter seq (flatten (map-indexed improved-find-numbers sample-data))))

(filter #(re-matches #"\d" (xy->char sample-data (second %))) (gear-adjacency [5 8]))
;; => ([:top-right [6 7]] [:bottom-right [6 9]] [:bottom [5 9]])

(defn begin-end-lookup [input [x y] {:keys [number begin end] :as number}]
  (let [one-dimensional (+ x (* y (count (first input))))]
    (if (some #(= one-dimensional %) (range begin (inc end)))
      number
      nil)))

(begin-end-lookup sample-data [2 0] {:number 467 :begin 0 :end 2})
;; => 467


(begin-end-lookup sample-data [3 2] {:number 35 :begin 22 :end 23})

sample-numbers
(filter #(begin-end-lookup sample-data [2 2] %) sample-numbers)
;; => ({:number "35", :begin 22, :end 23})

(defn whats-my-number? [input numbers approx]
  (filter #(begin-end-lookup input approx %) numbers))

(whats-my-number? sample-data sample-numbers [2 2])
;; => ({:number "35", :begin 22, :end 23})

(def potential (for [g gears]
                 (->> g
                      gear-adjacency
                      (filter #(re-matches #"\d" (xy->char sample-data (second %)))) ;; find trace of adjacent numbers
                      )))

potential
;; => (([:top-left [2 0]] [:bottom [3 2]] [:bottom-left [2 2]])
;;     ([:left [2 4]])
;;     ([:top-right [6 7]] [:bottom-right [6 9]] [:bottom [5 9]]))

(filter #(>= (count %) 2) potential)
;; => (([:top-left [2 0]] [:bottom [3 2]] [:bottom-left [2 2]]) ([:top-right [6 7]] [:bottom-right [6 9]] [:bottom [5 9]]))

(def identified-gears (filter #(>= (count %) 2) potential))

identified-gears
(defn calc-gear-ratios [input numbers gears]

  (let [potential (for [g gears]
                    (->> g
                         gear-adjacency
                         (filter #(re-matches #"\d" (xy->char input (second %)))) ;; find trace of adjacent numbers
                         ))
        identified-gears (filter #(>= (count %) 2) potential)
        adjacent-numbers  (for [g identified-gears]

                            (set (mapcat (comp #(whats-my-number? input numbers %) #(second %)) g)))
        qualified-gears (filter #(= (count %) 2) adjacent-numbers)] ;; those with exactly 2
    (apply + (map (fn [x] (apply * (map #(Integer/parseInt %) x))) (for [gq qualified-gears]
                                                                     (map :number gq))))))

(calc-gear-ratios sample-data sample-numbers gears)
;; => 467835


(def solution-numbers (filter seq (flatten (map-indexed improved-find-numbers input-data))))
(def solution-gears
  (find-gears input-data)
  )
solution-gears
(count solution-gears)
(calc-gear-ratios input-data solution-numbers solution-gears)
;; => 30272209

(def ig (filter #(>= (count %) 2) (for [g solution-gears]
                                    (->> g
                                         gear-adjacency
                                         (filter #(re-matches #"\d" (xy->char input-data (second %)))) ;; find trace of adjacent numbers
                                         ))))
(for [g ig]

  (set (mapcat (comp #(whats-my-number? input-data solution-numbers %) #(second %)) g)))
