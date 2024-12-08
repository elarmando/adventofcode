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

(defn vec-remove [v pos]
  (into (subvec v 0 pos) (subvec v (inc pos))))

(defn is_safe_removing_index [v i]
  (let [newv (vec-remove v i)]
    (is_safe newv)))

(defn search_safe_removing [v]
  (let [l (count v)]
    (loop [i 0]
      (if (>= i l)
        false
        ;else
        (if (is_safe_removing_index v i)
          true
          ;else
          (recur (inc i)))))))

(defn is_safe_removing_one_level [v]
  (if (is_safe v)
    true
    ;else
    (search_safe_removing v)))

(defn determine_safe [file]
  (for [line (read_lines file)]
    (is_safe_removing_one_level (parse_line line))))

(defn solve [file]
  (let [filtered (filter (fn [x] (= x true)) (determine_safe file))]
    (count filtered)))

(println "should be true" (is_safe_removing_one_level [7 6 4 2 1]))
(println "should be false" (is_safe_removing_one_level [1 2 7 8 9]))
(println "should be false" (is_safe_removing_one_level [9 7 6 2 1]))
(println "should be true" (is_safe_removing_one_level [1 3 2 4 5]))
(println "should be true" (is_safe_removing_one_level [8 6 4 4 1]))
(println "should be true" (is_safe_removing_one_level [1 3 6 7 9]))

;;(println (solve "inputtest.txt"))

(println "solution= " (solve "input.txt"))
