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


(defn sum-dif [v1 v2]
  (let [len (count v1)]
    (loop [i 0 sum 0]
      (if (>= i len)
        sum
      ;;else
        (let [n1 (get v1 i)
              n2 (get v2 i)
              plus (abs (- n1 n2))]
          (recur (inc i) (+ sum plus)))))))

(defn solve[]
  (let [file (parse-file "input.txt")
        v1 (vec (sort (get file :v1)))
        v2 (vec (sort (get file :v2)))
        sum (sum-dif v1 v2)]
    (println "result = " sum)))
      
(solve)
;;  (println (sum-dif [2 2] [1 1]))
;;(println (parse-file "input.txt"))
;;(println (parse-line "13 15"))
