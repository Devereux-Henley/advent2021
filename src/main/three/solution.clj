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

(defn sort-frequencies
  [compare-one-kv compare-two-kv]
  (let [compare-one (val compare-one-kv)
        compare-two (val compare-two-kv)]
    (if (= compare-one compare-two)
      (= (key compare-one-kv) \1)
      (> compare-one compare-two)
      ))
  )

(defn find-ordering
  [diagnostics index]
  (->> diagnostics
       (map #(nth % index))
       frequencies
       (sort sort-frequencies)
       (map first)
       ))

(defn calculate-rating [diagnostic-report rating-type]
  (let [accessor (case rating-type
                       :oxygen         first
                       :carbon-dioxide second)]
    (first
     (reduce
      (fn [accumulator index]
        (let [signal-ordering   (find-ordering accumulator index)
              next-accumulation (filter #(= (accessor signal-ordering) (nth % index)) accumulator)]
          (if (= (count next-accumulation) 0) (reduced accumulator) next-accumulation))
        )
      diagnostic-report
      (range (count (first diagnostic-report))))))
  )

(defn calculate-life-support-rating
  [diagnostic-report]
  (*
   (Integer/parseInt (calculate-rating diagnostic-report :oxygen) 2)
   (Integer/parseInt (calculate-rating diagnostic-report :carbon-dioxide) 2)
   ))

(defn solve-part-one
  []
  (->>
   (slurp "resources/3-1.txt")
   (clojure.string/split-lines)
   calculate-power-consumption
   )
  )

(defn solve-part-two
  []
  (->>
   (slurp "resources/3-1.txt")
   (clojure.string/split-lines)
   calculate-life-support-rating
   ))

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

  (calculate-life-support-rating input)

  (solve-part-one)

  (solve-part-two)

  )
