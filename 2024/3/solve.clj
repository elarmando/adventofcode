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
    (if (< (+ i 5) l)
      (if (not= "mulc(" (subs st i 5))
        nil
        {:st st :index (+ i 5) :length l}))))

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
      (< (- l i) 5) nil
      :else
      (let [world {:st st :index i :length (count st)} 
            mulc-part (valid-mulc-part world)]
        (if (not (nil? mulc-part))
          (let [num-part (valid-number-part mulc-part)]
            (if (not (nil? num-part))
              (let [comma-part (valid-char-part num-part ",")]
                (if (not (nil? comma-part))
                  (let [num2-part (valid-number-part comma-part)]
                    (if (not (nil? num2-part))
                      (let [bracket-part (valid-char-part num2-part ")")]
                        (if (not (nil? bracket-part))
                          (let [index (get bracket-part :index)]
                            {:index index 
                             :value (subs st i (- index i))
                             :n1 (get num-part :value)
                             :n2 (get num2-part :value)
                             })

                          nil)))))))))))))

(defn solve [st]
  (let [l (count st)]
    (loop [i 0 res 0]
      (if (>= i l)
        res
        (let [mult-str (get-mul-str i st)]
          (if (nil? mult-str)
            (recur (inc i) 0)
            (do 
              (println mult-str)
              (recur (get mult-str :index) 0))))))))


;(println (get-mul-str 0 "mulc(1,1)"))
(println (solve "mulc(1,1)amulc("))

