(ns three.solution)

(defn reorder-diagnostic-report
  [diagnostic-report]
  (reduce
   (fn [acc length]
     (assoc acc length (map #(nth % length) diagnostic-report)))
   []
   (range (count (first diagnostic-report)))))

(defn calculate-power-consumption
  [diagnostic-report]
  (->> diagnostic-report
       reorder-diagnostic-report
       (map frequencies)
       (reduce (fn [[gamma epsilon] coll]
                 [
                  (conj gamma (key (apply max-key val coll)))
                  (conj epsilon(key (apply min-key val coll)))
                  ]) [[] []])
       (map #(apply str %))
       (map #(Integer/parseInt % 2))
       (apply *)
       )
  )

(defn solve-part-one
  []
  (->>
   (slurp "resources/3-1.txt")
   (clojure.string/split-lines)
   calculate-power-consumption
   )
  )

(comment
  (def input
    [
     "00100"
     "11110"
     "10110"
     "10111"
     "10101"
     "01111"
     "00111"
     "11100"
     "10000"
     "11001"
     "00010"
     "01010"
     ]
    )

  (calculate-power-consumption input)

  (solve-part-one)

  )
