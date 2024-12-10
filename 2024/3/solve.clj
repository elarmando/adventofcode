(defn is-digit [s]
  (or (= "1" s) (= "2" s) (= "3" s) (= "4" s) (= "5" s) (= "6" s) (= "7" s) (= "8" s) (= "9" s)))

(defn get-char [st i]
  (if (< i (count st))
    (subs st i (+ i 1))
    nil))

(defn next3-is-number [st i]
  (if (>= i (count st))
    {:value nil :size 0}
    ;else
    (if (not (is-digit (get-char st i)))
      {:value nil :size 0}
      ;else
      (if (not (is-digit (get-char st (+ i 1))))
        {:value (subs st i (+ i 1)) :size 1}
        ;else
        (if (not (is-digit (get-char st (+ i 2))))
          {:value (subs st i (+ i 2)) :size 2}
          ;else
          {:value (subs st i (+ i 3)) :size 3})))))

(defn valid-mulc-part [input]
  (let [st (get input :st)
        i  (get input :index)
        l (get input :length)]
    (if (< (+ i 4) l)
      (if (not= "mul(" (subs st i (+ i 4)))
        nil
        (do 
          {:st st :index (+ i 4) :length l})))))

(defn valid-number-part [input] 
  (let [st (get input :st)
        i (get input :index)
        number (next3-is-number st i)
        value (get number :value)
        l (get input :length)]
    (if (nil? value)
      nil
      {:st st :value value :index (+ i (get number :size)) :length l})))

(defn valid-char-part [input c]
  (let [st (get input :st)
        i (get input :index)
        l (get input :length)]
    (if (< i l)
      (if (= c (get-char st i))
        {:st st :index (+ i 1) :length l}
        nil))))

(defn get-mul-str [i st]
  (let [l (count st)]
    (cond
      (< (- l i) 4) nil
      :else
      (let [world {:st st :index i :length (count st)} 
            mulc-part (valid-mulc-part world)]
        (if (not (nil? mulc-part))
          (let [num-part (valid-number-part mulc-part)]
            (if (not (nil? num-part))
              (do
                (let [comma-part (valid-char-part num-part ",")]
                  (if (not (nil? comma-part))
                    (let [num2-part (valid-number-part comma-part)]
                      (if (not (nil? num2-part))
                        (let [bracket-part (valid-char-part num2-part ")")]
                          (if (not (nil? bracket-part))
                            (let [index (get bracket-part :index)]
                              {:index index 
                               :value (subs st i index)
                               :n1 (get num-part :value)
                               :n2 (get num2-part :value)
                               })

                            nil))))))))))))))

(defn multiply [w]
  (let [n1 (get w :n1)
        n2 (get w :n2)
        num1 (Integer/parseInt n1)
        num2 (Integer/parseInt n2)]
    (* num1 num2)))

(defn sum-correct-mult [st]
  (let [l (count st)]
    (loop [i 0 res 0]
      (if (>= i l)
        res
        (let [mult-str (get-mul-str i st)]
          (if (nil? mult-str)
            (recur (inc i) res)
            (do 
              (println mult-str)
              (let [mult-res (multiply mult-str)]
                (recur (get mult-str :index) (+ res mult-res))))))))))

(defn solve [file]
   (let [content (slurp file)]
         (sum-correct-mult content))) 
;(println (get-mul-str 0 "mulc(1,1)")) 
;(println (sum-correct-mult "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

(println (solve "input.txt"))

