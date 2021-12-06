(ns two.solution)

(defprotocol INavigable
  (navigate [this instruction] "Navigates to a new position based on an inbound instruction.")
  (distance-from-start [this] "Returns the distance of the navigable from it's start position."))

(defrecord JankSubmarine [depth position]
  INavigable
  (navigate [this {:keys [direction magnitude]}]
    (case direction
      :forward  (assoc this :position (+ position magnitude))
      :up (assoc this :depth (- depth magnitude))
      :down (assoc this :depth (+ depth magnitude))
      ))
  (distance-from-start [this]
    (* position depth)))

(defrecord Submarine [depth position aim]
  INavigable
  (navigate [this {:keys [direction magnitude]}]
    (case direction
      :forward (-> this
                (assoc :position (+ position magnitude))
                (assoc :depth (+ depth (* aim magnitude)))
                )
      :up       (assoc this :aim (- aim magnitude))
      :down     (assoc this :aim (+ aim magnitude))
      ))
  (distance-from-start [this]
    (* position depth)))



(defn to-instruction [instruction]
  (let [[direction magnitude] (clojure.string/split instruction #" ")]
    {:direction (keyword direction) :magnitude (Integer/parseInt magnitude)}
    )
  )

(defn drive-submarine
  [instruction])

(defn solve-part-one
  []
  (->>
   (slurp "resources/2-1.txt")
   (clojure.string/split-lines)
   (map to-instruction)
   (reduce (fn [submarine instruction] (navigate submarine instruction)) (->JankSubmarine 0 0))
   distance-from-start
   )
  )

(defn solve-part-two
  []
  (->>
   (slurp "resources/2-1.txt")
   (clojure.string/split-lines)
   (map to-instruction)
   (reduce (fn [submarine instruction] (navigate submarine instruction)) (->Submarine 0 0 0))
   distance-from-start
   )
  )

(comment
  (->>
   [
    {:direction :forward :magnitude 5}
    {:direction :down :magnitude 5}
    {:direction :forward :magnitude 8}
    {:direction :up :magnitude 3}
    {:direction :down :magnitude 8}
    {:direction :forward :magnitude 2}
    ]
   (reduce (fn [submarine instruction] (navigate submarine instruction)) (->JankSubmarine 0 0))
   distance-from-start
   )

  (solve-part-one)

  (->>
   [
    {:direction :forward :magnitude 5}
    {:direction :down :magnitude 5}
    {:direction :forward :magnitude 8}
    {:direction :up :magnitude 3}
    {:direction :down :magnitude 8}
    {:direction :forward :magnitude 2}
    ]
   (reduce (fn [submarine instruction] (navigate submarine instruction)) (->Submarine 0 0 0))
   distance-from-start
   )

  (solve-part-two)

  )
