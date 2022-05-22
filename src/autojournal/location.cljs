(ns autojournal.location
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [EventFetcher Event Date]]
            [cljs-time.coerce :refer [to-long from-long to-string from-string]]))
  
(prn (drive/get-files "journal.txt"))

(defn -date-to-file
  {:malli/schema [:=> [:cat Date] :string]}
  [{:keys [day month year]}]
  (str year month day ".zip"))


(to-long (from-string "2022-05-15T07:00:28.000Z"))


(def -stationary-distance-miles
  "Distance between two readings for movement between them to be ignored."
  0.05)

(def Reading
  [:map [:time :int]
        [:lat :double]
        [:lon :double]
        [:accuracy-miles :double]])

(defn -to-radians [degrees] (* degrees (/ Math/PI 180)))

(defn -haversine-distance
  "Implementation of Haversine formula. Takes two sets of latitude/longitude
  pairs and returns the shortest great circle distance between them (in miles)

  From https://gist.github.com/shayanjm/39418c8425c2a66d480f

  Haversine formula
  a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
  c = 2 ⋅ atan2( √a, √(1−a) )
  d = r ⋅ c
  where φ is latitude, λ is longitude, and r is the Earth’s radius"
  {:malli/schema [:=> [:cat Reading Reading]
                  :double]}
  [{lon1 :lon lat1 :lat} {lon2 :lon lat2 :lat}]
  (let [r 3958.8 ; Radius of Earth in miles
        dlat (-to-radians (- lat2 lat1))
        dlon (-to-radians (- lon2 lon1))
        lat1 (-to-radians lat1)
        lat2 (-to-radians lat2)
        a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
             (* (Math/sin (/ dlon 2)) (Math/sin (/ dlon 2))
                (Math/cos lat1) (Math/cos lat2)))]
    (* r 2 (Math/asin (Math/sqrt a)))))

(defn -meters-to-miles [meters] (/ meters 1609.34))

(defn -row-to-reading
  {:malli/schema [:=> [:cat [:map-of :keyword :string]]
                  Reading]}
  [{:keys [time lat lon accuracy]}]
  {:time (to-long (from-string time))
   :lat (js/parseFloat lat)
   :lon (js/parseFloat lon)
   :accuracy-miles (-meters-to-miles (js/parseFloat accuracy))})
  

(defn -are-same-place
  {:malli/schema [:=> [:cat Reading Reading]
                  :bool]}
  [reading1 reading2]
  (> (+ (:accuracy-miles reading1) (:accuracy-miles reading2)
        -stationary-distance-miles)
     (-haversine-distance reading1 reading2)))

(defn -average
  {:malli/schema [:=> [:cat [:sequential :double]] :double]}
  [coll]
  (/ (reduce + coll) (count coll)))

(defn -get-midpoint
  "Note that this simply averages coordinates, which is NOT ACCURATE for large
  distances or near poles, see https://gis.stackexchange.com/a/7566."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  Reading]}
  [readings]
  {:time (-average (map :time readings))
   :lat (-average (map :lat readings))
   :lon (-average (map :lon readings))
   ; Accuracy is currently not used.
   :accuracy-miles 0})
  

(def -all-same-place-fraction-required
  "The fraction of points that must be within -stationary-distance-miles of
  each other for the whole cluster of points to be considered at the same
  place."
  0.9)


(defn -are-all-same-place
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  :bool]}
  [readings]
  (let [midpoint (-get-midpoint readings)
        total (count readings)
        same (reduce + (for [reading readings]
                         (if (-are-same-place midpoint reading) 1 0)))]
    (> (/ same total)
       -all-same-place-fraction-required)))

(-are-same-place
  (-row-to-reading
    {:elevation "44.99261474609375",
     :accuracy "9.648001",
     :time "2022-05-15T07:00:28.000Z",
     :lon "-122.31444872",
     :lat "47.66866721"})
  (-row-to-reading
    {:elevation "45.0035400390625",
     :accuracy "9.648001",
     :time "2022-05-15T07:01:03.000Z",
     :lon "-122.31444892",
     :lat "47.66866769"}))

(defn -split-readings
  "Splits off the first set of readings that all occured in the same place."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  [:tuple [:sequential Reading] [:sequential Reading]]]}
  [readings]
  [])

(defn -readings-to-event
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  Event]}
  [readings]
  (let [midpoint (-get-midpoint readings)]
    {:start (:time (first readings))
     ; Use something other than `last` if performance is bad
     :end (:time (last readings))
     :summary (str "At " midpoint)}))


(defn -readings-to-events
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  [:sequential Event]]}
  [readings]
  (if (empty? readings)
    []
    (let [[start others] (-split-readings readings)]
      (concat [(-readings-to-event start)]
              (-readings-to-events others)))))
  

(defn -get-files
  {:malli/schema [:=> [:cat [:sequential Date]] ; Start time
                  [:sequential Event]]}
  [days])
  ; (for [day days]))
    
  

(defn get-events
  {:malli/schema EventFetcher}
  [start-time])
  
  
