(ns burrows-wheeler
  (:require [clojure.string :as string]))

;;;; A Clojure implementation of the Burrows-Wheeler Transform

(defn rotate-string [s i]
  (let [len (count s)]
    (str (subs s i len) (subs s 0 (mod i len)))))

;; This should run in O(n), but using Clojure's built-in sorting function yields O(n^2)
(defn bwt [s]
  (let [s (str s "$")
        len (count s)
        indices (range 0 len)
        sorted-indices (sort-by #(rotate-string s %) indices)]
    (->> sorted-indices
         (map #(nth s (mod (+ % (dec len)) len)))
         (apply str)))) 

;; Supposedly this does not necessarily have to call sort at all
(defn ibwt [s]
  (loop [i (dec (count s))
         result (string/split s #"")]
    (if (> i 0)
      (recur (dec i) (map str (string/split s #"") (sort result)))
      (->> result
           (filter #(= (last %) \$))
           first))))

(defn bwt-compress [s]
  (let [s (bwt s)]
    (string/replace s #"(.)\1{2,}" #(str (count (first %1)) (second %1)))))

(defn expand-str [s]
  (as-> (string/split s #"(?<=\d)(?=\D)") $
    (repeat (clojure.edn/read-string (first $)) (second $))
    (apply str $)))

(defn bwt-decompress [s]
  (as-> s $
       (string/replace $ #"(\d+\D)" #(expand-str (first %1)))
       (ibwt $)))

(def lorem-ipsum
  "Dolorem beatae sed eius iste beatae impedit. Unde quos voluptatem velit. Sequi assumenda beatae excepturi. Repellendus dolores consequuntur debitis. Voluptatem quasi blanditiis eum eveniet ipsa sed. Non quasi accusamus delectus eos ut aut pariatur id. Corporis est quia suscipit asperiores qui dicta harum. Vel maxime deleniti eligendi et amet sed nemo. Non excepturi eos minima aut. Commodi repellendus voluptatibus deleniti eligendi consequatur id voluptatem est. Dolor facilis porro aut. Aliquam voluptatem accusantium dignissimos nisi. Soluta et sit et magni soluta. Omnis excepturi suscipit delectus sint impedit commodi eos magni. Fuga et reprehenderit hic voluptas. Eaque quasi exercitationem alias facilis. Quo qui atque error labore et vel aspernatur dolore. Eos et laudantium adipisci esse in id iusto. Repellendus ut possimus consequatur neque deleniti ratione sunt quod.")

;; (count lorem-ipsum)
;;=> 882

(def lorem-ipsum-long (apply str (repeat 4 lorem-ipsum)))

;; (do (time (bwt lorem-ipsum)) nil)
;;=> "Elapsed time: 24.073236 msecs"
;; (do (time (bwt lorem-ipsum-long)) nil)
;;=> "Elapsed time: 304.547108 msecs"

;; (do (time (bwt-decompress (bwt-compress lorem-ipsum))) nil)
;;=> "Elapsed time: 732.792284 msecs"
;; (do (time (bwt-decompress (bwt-compress lorem-ipsum-long))) nil)
;;=> "Elapsed time: 25588.484786 msecs"
