(ns aoc-2023.core)

(defn read-file-lines [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (line-seq rdr))))
