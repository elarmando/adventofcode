(import '(java.io BufferedReader StringReader))
(use '[clojure.string :only (split triml)])

(defn read_lines [file]
  (let [content (slurp file)
        sq (line-seq (BufferedReader. (StringReader. content)))]
    sq))

(defn parse_line [line]
  (let [v (split line #"\s+")
        vint (for [x v] (Integer/parseInt x))]
        (vec vint)))

(defn get_diff [v i j]
  (let [n1 (get v i) n2 (get v j)]
    (- n1 n2)))

(defn is_valid_diff [diff]
  (let [abs_diff (abs diff)]
     (and (>= abs_diff 1) (<= abs_diff 3))))

(defn flow_changed [current new_flow]
  (and (not= current nil) (not= current new_flow)))

(defn is_safe [v]
  (loop [i 1 is_increasing_flag nil]
    (if (>= i (count v)) 
      true
    ;;else
      (let [diff (get_diff v (- i 1) i)
            valid_diff (is_valid_diff diff)
            is_increasing (> diff 0)]
        (cond 
          (= valid_diff false) false
          (flow_changed is_increasing_flag is_increasing) false
          :else (recur (inc i) is_increasing))))))


(defn determine_safe [file]
  (for [line (read_lines file)]
    (is_safe (parse_line line))))

(defn solve [file]
  (let [filtered (filter (fn [x] (= x true)) (determine_safe file))]
    (count filtered)))

(println "should be true" (is_safe [1 2 3 4 5]))
(println "should be false" (is_safe [1 2 6 7 8]))
(println "should be true" (is_safe [7 4 3 2 1]))
(println "should be false" (is_safe [7 5 3 2 3]))
(println "should be true" (is_safe [7 6 4 2 1]))
(println "should be false" (is_safe [1 2 7 8 9]))
(println "should be false" (is_safe [9 7 6 2 1]))
(println "should be false" (is_safe [1 3 2 4 5]))
(println "should be false" (is_safe [8 6 4 4 1]))
(println "should be true" (is_safe [1 3 6 7 9]))

;;(println (solve "inputtest.txt"))

(println "solution= " (solve "input.txt"))
