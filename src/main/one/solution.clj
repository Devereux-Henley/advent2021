(ns one.solution)

(defn ascending-depths
  ([depths]
   (ascending-depths 1 depths))
  ([group-length depths]
   (->> depths
        (partition group-length 1)
        (map #(apply + %))
        (reduce
         (fn [[count previous] value]
           (if (nil? previous)
             [0 value]
             [(if (< previous value) (inc count) count) value])) [0 nil])
        first
        )
   )
  )

(defn solve-part-one []
  (->>
   (slurp "resources/1-1.txt")
   (clojure.string/split-lines)
   (map #(Integer/parseInt %))
   ascending-depths
   )
  )

(defn solve-part-two []
  (->>
   (slurp "resources/1-1.txt")
   (clojure.string/split-lines)
   (map #(Integer/parseInt %))
   (ascending-depths 3))
  )

(comment
  (ascending-depths [199 200 208 210 200 207 240 269 260 263])

  (ascending-depths 3 [199 200 208 210 200 207 240 269 260 263])

  (solve-part-one)

  (solve-part-two)

  )
