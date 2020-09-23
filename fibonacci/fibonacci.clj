(ns fibonacci)

(defn nth-fibonacci
  "Returns the nth number of the fibonacci sequence"
  [n]
  (if (= n 0)
    0
    (loop [prev (bigint 0)
           curr (bigint 1)
           count 1]
      (if (= n count)
        curr
        (recur curr (+ curr prev) (inc count))))))

;;(nth-fibonacci 10) => 55N
