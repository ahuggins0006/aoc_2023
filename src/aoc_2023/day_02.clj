(ns aoc-2023.day-02
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
(def sample-data (core/read-file-lines "resources/day_02_sample.txt"))

sample-data
;; => ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

(def a-sample (first sample-data))

a-sample;; => "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"


(map str/trim (str/split a-sample #":|;|,"))
;; => ("Game 1" "3 blue" "4 red" "1 red" "2 green" "6 blue" "2 green")

(def experiment-1 (map str/trim (str/split a-sample #":|;|,")))
(.contains "Game 1" "Game")
(loop [e experiment-1
       acc {}]
  (let [h (first e)]
    (println e acc h)
    (cond
      (empty? e) acc
      (.contains h "Game") (recur (rest e) (conj (assoc acc :game (Integer/parseInt (re-find #"\d+" h))) {:red 0 :blue 0 :green 0}))
      (.contains h "red") (recur (rest e) (update acc :red max (Integer/parseInt (re-find #"\d+" h))))
      (.contains h "green") (recur (rest e) (update acc :green max (Integer/parseInt (re-find #"\d+" h))))
      (.contains h "blue") (recur (rest e) (update acc :blue max (Integer/parseInt (re-find #"\d+" h)))))))
;; => {:game 1, :red 5, :blue 9, :green 4}

(defn game-string->game-map [s]
  (let [splitted (map str/trim (str/split s #":|;|,"))]
    (loop [e splitted
           acc {}]
      (let [h (first e)]
        (cond
          (empty? e) acc
          (.contains h "Game") (recur (rest e) (conj (assoc acc :game (Integer/parseInt (re-find #"\d+" h))) {:red 0 :green 0 :blue 0}))
          (.contains h "red") (recur (rest e) (update acc :red max (Integer/parseInt (re-find #"\d+" h))))
          (.contains h "green") (recur (rest e) (update acc :green max (Integer/parseInt (re-find #"\d+" h))))
          (.contains h "blue") (recur (rest e) (update acc :blue max (Integer/parseInt (re-find #"\d+" h)))))))))

(def a-game-map-sample (game-string->game-map a-sample))
(game-string->game-map (nth sample-data 3))
;; => {:game 4, :red 14, :green 3, :blue 15}

;; TODO check colors for only 12 red cubes, 13 green cubes, and 14 blue cubes?

(defn possible-game? [requirements game-map]
  (let [colors (rest (keys game-map))]
    (loop [c colors
           possible? true]
      (if (or (not possible?) (empty? c)) possible?
          (recur (rest c) (if (<= ((first c) game-map) ((first c) requirements)) true false))))))

(possible-game?  {:red 20 :green 13 :blue 14} a-game-map-sample)
;; => true

(possible-game? {:red 20 :green 13 :blue 14} (game-string->game-map (nth sample-data 2)))
(def colors (rest (keys a-game-map-sample)))
(rest (seq colors))
(into [] (rest (keys a-game-map-sample)))

(loop [c colors
       acc false]
  (if (empty? c) acc
      (recur (rest c) (if (<= ((first c) a-game-map-sample) ((first c) {:red 12 :green 13 :blue 14})) true false))))

(for [c colors]
  (println c)
  )

(filter #(possible-game? {:red 12 :green 13 :blue 14} %) (map game-string->game-map sample-data))
;; => ({:game 1, :red 4, :green 2, :blue 6} {:game 2, :red 1, :green 3, :blue 4} {:game 5, :red 6, :green 3, :blue 2})

(apply + (for [g '({:game 1, :red 4, :green 2, :blue 6} {:game 2, :red 1, :green 3, :blue 4} {:game 5, :red 6, :green 3, :blue 2})]
           (:game g)))
;; => 8


(defn solution-1 [requirements input]
  (let [possible-games (filter #(possible-game? requirements %) (map game-string->game-map input))
        possible-games-vals (for [g possible-games]
                              (:game g))
        sums (apply + possible-games-vals)]
    sums))


(solution-1 {:red 12 :green 13 :blue 14} sample-data)
;; => 8

(def input-data (core/read-file-lines "resources/day_02_input.txt"))

(solution-1 {:red 12 :green 13 :blue 14} input-data)
;; => 2348

;;part 2

(map game-string->game-map sample-data)
;; => ({:game 1, :red 4, :green 2, :blue 6} {:game 2, :red 1, :green 3, :blue 4} {:game 3, :red 20, :green 13, :blue 6} {:game 4, :red 14, :green 3, :blue 15} {:game 5, :red 6, :green 3, :blue 2})

(def experiment-2 (map game-string->game-map sample-data))

(defn game-cubed [{:keys [red green blue] }]
  (* red green blue)
  )

(game-cubed (first experiment-2))
;; => 48
(map game-cubed experiment-2)
;; => (48 12 1560 630 36)

(apply + '(48 12 1560 630 36))
;; => 2286

(->> sample-data
     (map (comp game-cubed game-string->game-map))
     (apply +));; => 2286
(->> input
     (map (comp game-cubed game-string->game-map))
     (apply +))
