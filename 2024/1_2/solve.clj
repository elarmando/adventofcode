(import '(java.io BufferedReader StringReader))
(use '[clojure.string :only (split triml)])

(defn parse-line [line]
  (let [v (split line #"\s+")]
    [(Integer/parseInt (get v 0))
     (Integer/parseInt (get v 1))]))


(defn parse-file [file]
  (let [content (slurp "input.txt")
        sq (line-seq (BufferedReader. (StringReader. content)))]
    (loop [my-sq sq v1 [] v2 []]
      (let [item (first my-sq)]
        (if (nil? item)
         {:v1 v1 :v2 v2} 
        ;;else
        (do
          (let [nums (parse-line item)
                n1 (get nums 0)
                n2 (get nums 1)]
            (recur (rest my-sq) (conj v1 n1) (conj v2 n2)))))))))

(defn increase-count [i v mp]
  (let [n1 (get v i)
        current-count (get mp n1)]
    (if (nil? current-count)
      (assoc mp n1 1)
      (assoc mp n1 (inc current-count)))))

(defn count-freq [v]
  (let [l (count v)]
    (loop [i 0 mp {}]
      (if (>= i l)
        mp
      ;;else
        (recur (inc i) (increase-count i v mp))))))

(defn calc-score-index [index v freq-map]
  (let [n1 (get v index)
        freq (get freq-map n1)]
        (if (nil? freq)
          0
        ;else
          (* n1 freq))))

(defn calc-score [v1 v2]
  (let [freq-map (count-freq v2) length (count v1)]
    (loop [i 0 score 0]
      (if (>= i length)
        score
      ;else
        (let [num-score (calc-score-index i v1 freq-map)
              new-score (+ score num-score)]
          (recur (inc i) new-score))))))

(defn solve [file]
  (let [parsed (parse-file file)
        v1 (get parsed :v1)
        v2 (get parsed :v2)]
    (calc-score v1 v2)))

;(println (count-freq [2 2 2 1 1 5 5 5 5]))
;(println "1x3 + 2x0 + 3x1 =" (calc-score [1 2 3] [1 1 1 3]))
;(println "9 + 4 + 0 + 0 + 9 + 9 =" (calc-score [3 4 2 1 3 3] [4 3 5 3 9 3]))
(println (solve "input.txt"))
