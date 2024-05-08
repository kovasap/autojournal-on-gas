(ns autojournal.location
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [Timestamp EventFetcher Event Date]]
            [autojournal.testing-utils :refer [assert=]]
            [autojournal.calendar :as calendar]
            [cljs-time.core :as t]
            [clojure.string :as st]
            [cljs-time.coerce :refer [to-long from-long to-string from-string]]))

; This spreadsheet should have columns "lat" "lon" and "name"
(def -location-names-google-sheet-name
  "Location Names")
  
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
                  :boolean]}
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

(defn -get-speed
  "Calculates speed between two readings in MPH."
  {:malli/schema [:=> [:cat Reading Reading]
                  :double]}
  [r1 r2]
  (/ (-haversine-distance r1 r2)
     (/ (Math/abs (- (:time r1) (:time r2)))
        ; ms -> hours
        1000 60 60)))

; Could potentially use a speed cutoff to split readings into travelling /
; not-travelling segments.
(defn -get-avg-speed
  "Calculates average speed between all readings in MPH."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  :double]}
  [readings]
  (-average
    (for [[r1 r2] (partition 2 1 readings)]
      (-get-speed r1 r2))))

(assert=
  0.005744751219511142
  (-get-avg-speed
    [{:time 1652598097000, :lat 47.66866761, :lon -122.31444817, 
      :accuracy-miles 0.00466278101582015}
     {:time 1652598130000, :lat 47.66866714, :lon -122.3144491, 
      :accuracy-miles 0.0039966694421315575}
     {:time 1652598180000, :lat 47.66866672, :lon -122.31445055, 
      :accuracy-miles 0.009991673605328892}
     {:time 1652598213000, :lat 47.66866612, :lon -122.31445134, 
      :accuracy-miles 0.007327227310574521}]))

(def -all-location-group-fraction-required
  "The fraction of points that must be within -stationary-distance-miles of
  each other for the whole cluster of points to be considered at the same
  place."
  0.9)

(def TallyFunction
  [:=> [:cat :any]
   :int])

(defn -enough-meet-criteria
  {:malli/schema [:=> [:cat [:sequential :any] TallyFunction :double]
                  :boolean]}
  [things tally-fn frac-required]
  (let [total (count things)
        matching (reduce + (for [thing things]
                             (tally-fn thing)))]
    (> (/ matching total)
       frac-required)))
  
(defn -are-all-same-place
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  :boolean]}
  [readings]
  (-enough-meet-criteria
    readings
    (let [midpoint (-get-midpoint readings)]
      (fn [reading] (if (-are-same-place midpoint reading) 1 0)))
    -all-location-group-fraction-required))

(defn -are-all-diff-place
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  :boolean]}
  [readings]
  (-enough-meet-criteria
    readings
    (fn [reading]
      ; Note that this is a pairwise expensive O(n^2) operation.
      (if (every? #(not (-are-same-place reading %)) readings) 1 0))
    -all-location-group-fraction-required))

(assert=
  true
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
       :lat "47.66866769"})))

(assert=
  false
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
       :lon "-142.31444892",
       :lat "47.66866769"})))


(def -min-window-size 3)

(defn -split-readings
  "Splits off the first set of readings that all occured in the same place,
  or all occured when travelling. If all readings fit occured in the same place
  or while travelling, then the second return value will be empty."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  [:tuple [:sequential Reading] [:sequential Reading]]]}
  [readings]
  (loop [cur-size (+ 1 -min-window-size)]
    (let [cur-window (take cur-size readings)]
      (cond
        (>= cur-size (count readings)) [readings []]
        (or (-are-all-same-place cur-window)
            (-are-all-diff-place cur-window)) (recur (inc cur-size))
        :else [(drop-last cur-window)
               (drop (- cur-size 1) readings)]))))

(assert=
  ['({:time 1652598028000, :lat 47.66866721, :lon -122.31444872,
      :accuracy-miles 0.005995004784570073}
     {:time 1652598063000, :lat 47.66866769, :lon -122.31444892,
      :accuracy-miles 0.005995004784570073}
     {:time 1652598097000, :lat 47.66866761, :lon -122.31444817,
      :accuracy-miles 0.00466278101582015})
   '({:time 1652598180000, :lat 48.66866672, :lon -122.31445055,
      :accuracy-miles 0.009991673605328892}
     {:time 1652598213000, :lat 48.66866612, :lon -122.31445134,
      :accuracy-miles 0.007327227310574521}
     {:time 1652598250000, :lat 48.66866617, :lon -122.31445277,
      :accuracy-miles 0.008659451079324443}
     {:time 1652598282000, :lat 48.66866565, :lon -122.31445328,
      :accuracy-miles 0.0093255620316403})]
  (-split-readings
    [{:time 1652598028000, :lat 47.66866721, :lon -122.31444872, 
      :accuracy-miles 0.005995004784570073}
     {:time 1652598063000, :lat 47.66866769, :lon -122.31444892, 
      :accuracy-miles 0.005995004784570073}
     {:time 1652598097000, :lat 47.66866761, :lon -122.31444817, 
      :accuracy-miles 0.00466278101582015}
     {:time 1652598180000, :lat 48.66866672, :lon -122.31445055, 
      :accuracy-miles 0.009991673605328892}
     {:time 1652598213000, :lat 48.66866612, :lon -122.31445134, 
      :accuracy-miles 0.007327227310574521}
     {:time 1652598250000, :lat 48.66866617, :lon -122.31445277, 
      :accuracy-miles 0.008659451079324443}
     {:time 1652598282000, :lat 48.66866565, :lon -122.31445328, 
      :accuracy-miles 0.0093255620316403}]))

(defn -name-location
  [{location-lat :lat location-lon :lon} location-names]
  (st/join
    ","
    (->> location-names
         (filter
           (fn [[{:keys [lat lon]} _]]
             (-are-same-place
               {:lat lat :lon lon :accuracy-miles 0.1}
               {:lat location-lat :lon location-lon :accuracy-miles 0.1})))
         (vals))))
                 

(defn -readings-to-event
  "Converts a bunch of readings to a single event. Assumes the readings
  happened in the same place."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  Event]}
  [readings location-names]
  (let [midpoint (-get-midpoint readings)
        coords-str (str (:lat midpoint) ", " (:lon midpoint))]
    {:start (:time (first readings))
     ; Use something other than `last` if performance is bad
     :end (:time (last readings))
     :lat (:lat midpoint)
     :lon (:lon midpoint)
     :speed-mph (-get-avg-speed readings)
     :description (str midpoint)
     :location coords-str
     :summary (str "At " (-name-location midpoint location-names))}))


(defn -readings-to-events
  "Converts many readings to a series of events, with each event containing
  readings in the same location, or that occured during a single travel."
  {:malli/schema [:=> [:cat [:sequential Reading]]
                  [:sequential Event]]}
  [readings location-names]
  (if (empty? readings)
    []
    (let [[start others] (-split-readings readings)]
      (concat [(-readings-to-event start location-names)]
              (-readings-to-events others location-names)))))

(defn -pad-date-zeros
  {:malli/schema [:=> [:cat :int] :string]}
  [n]
  (if (= 1 (count (str n)))
    (str "0" n)
    n))

(defn -date-to-file
 {:malli/schema [:=> [:cat Date] :string]}
 [{:keys [day month year]}]
 (str year (-pad-date-zeros month) (-pad-date-zeros day) ".zip"))

(defn -get-readings-from-drive
  {:malli/schema [:=> [:cat [:sequential Date]]
                  [:sequential Reading]]}
  [dates]
  (let [rows (reduce concat
                     (reduce concat
                           (for [date (distinct dates)]
                             (drive/get-files (-date-to-file date)))))]
    (map -row-to-reading rows)))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn -get-location-names-from-drive
  []
  (->> (drive/get-files -location-names-google-sheet-name)
       (reduce concat)
       (map (fn [{:keys [lat lon name]}]
              {:coordinates {:lat lat :lon lon}
               :name name}))
       (group-by :coordinates)
       (map-vals :name)))

(defn -datetime-to-date
  [datetime]
  {:day (t/day datetime)
   :month (t/month datetime)
   :year (t/year datetime)})

(defn -dates-in-time-window
  {:malli/schema [:=> [:cat Timestamp Timestamp]
                  [:sequential Date]]}
  [start-time end-time]
  (let [start-datetime (from-long start-time)
        end-datetime (from-long end-time)]
    (loop [cur-datetime start-datetime
           dates []]
      (if (t/after? cur-datetime end-datetime)
        dates
        (recur 
          (t/plus cur-datetime (t/days 1))
          (conj dates (-datetime-to-date cur-datetime)))))))

(defn get-events
  {:malli/schema EventFetcher}
  [start-time end-time]
  (-readings-to-events
    (-get-readings-from-drive
       (-dates-in-time-window start-time end-time))
    (-get-location-names-from-drive)))

(defn update-calendar!
  [days-to-update]
  (let [today (t/today)
        start-day (t/minus today (t/days days-to-update))
        events (time (get-events (to-long start-day) (to-long today)))]
    (mapv calendar/add-event! events)))

(assert=
  '({:start 1652598028000
     :end 1652601079000
     :lat 47.66865123012048
     :lon -122.31444797518073
     :speed-mph 0.0513941814803412
     :description
     "{:time 1652599536992.9277, :lat 47.66865123012048, :lon -122.31444797518073, :accuracy-miles 0}"
     :location "47.66865123012048, -122.31444797518073"
     :summary "At 47.66865123012048, -122.31444797518073"})
  (-readings-to-events [{:time 1652598028000
                         :lat  47.66866721
                         :lon  -122.31444872
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652598063000
                         :lat  47.66866769
                         :lon  -122.31444892
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652598097000
                         :lat  47.66866761
                         :lon  -122.31444817
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652598130000
                         :lat  47.66866714
                         :lon  -122.3144491
                         :accuracy-miles 0.0039966694421315575}
                        {:time 1652598180000
                         :lat  47.66866672
                         :lon  -122.31445055
                         :accuracy-miles 0.009991673605328892}
                        {:time 1652598213000
                         :lat  47.66866612
                         :lon  -122.31445134
                         :accuracy-miles 0.007327227310574521}
                        {:time 1652598250000
                         :lat  47.66866617
                         :lon  -122.31445277
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652598282000
                         :lat  47.66866565
                         :lon  -122.31445328
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652598316000
                         :lat  47.66866509
                         :lon  -122.31445331
                         :accuracy-miles 0.007993338884263115}
                        {:time 1652598349000
                         :lat  47.66866487
                         :lon  -122.31445257
                         :accuracy-miles 0.0039966694421315575}
                        {:time 1652598382000
                         :lat  47.66866571
                         :lon  -122.31445294
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652598415000
                         :lat  47.66866621
                         :lon  -122.31445264
                         :accuracy-miles 0.005328892589508743}
                        {:time 1652598467000
                         :lat  47.66866625
                         :lon  -122.31445314
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652598500000
                         :lat  47.66866679
                         :lon  -122.31445321
                         :accuracy-miles 0.005328892589508743}
                        {:time 1652598533000
                         :lat  47.66866531
                         :lon  -122.31445327
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652598589000
                         :lat  47.66866441
                         :lon  -122.31445323
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652598622000
                         :lat  47.66866467
                         :lon  -122.31445279
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652598655000
                         :lat  47.66866476
                         :lon  -122.31445304
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652598688000
                         :lat  47.66866462
                         :lon  -122.31445292
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652598721000
                         :lat  47.6686636
                         :lon  -122.31445401
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652598754000
                         :lat  47.66866389
                         :lon  -122.31445348
                         :accuracy-miles 0.010657785179017486}
                        {:time 1652598787000
                         :lat  47.66866391
                         :lon  -122.31445383
                         :accuracy-miles 0.0033305578684429647}
                        {:time 1652598819000
                         :lat  47.6686643
                         :lon  -122.31445361
                         :accuracy-miles 0.010657785179017486}
                        {:time 1652598863000
                         :lat  47.66866343
                         :lon  -122.31445393
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652598896000
                         :lat  47.66866338
                         :lon  -122.3144534
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652598929000
                         :lat  47.66866379
                         :lon  -122.31445256
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652598962000
                         :lat  47.66866393
                         :lon  -122.31445213
                         :accuracy-miles 0.009991673605328892}
                        {:time 1652598995000
                         :lat  47.66866419
                         :lon  -122.31445158
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652599032000
                         :lat  47.66866353
                         :lon  -122.31445203
                         :accuracy-miles 0.0026644462947543715}
                        {:time 1652599065000
                         :lat  47.66866299
                         :lon  -122.31445195
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652599098000
                         :lat  47.66866308
                         :lon  -122.31445187
                         :accuracy-miles 0.0033305578684429647}
                        {:time 1652599143000
                         :lat  47.66866372
                         :lon  -122.31445168
                         :accuracy-miles 0.013322231473771859}
                        {:time 1652599176000
                         :lat  47.66866329
                         :lon  -122.3144516
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652599211000
                         :lat  47.66866237
                         :lon  -122.31445107
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652599244000
                         :lat  47.66866184
                         :lon  -122.31445161
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652599295000
                         :lat  47.66866203
                         :lon  -122.31445187
                         :accuracy-miles 0.013988343668833187}
                        {:time 1652599349000
                         :lat  47.66865993
                         :lon  -122.31445213
                         :accuracy-miles 0.007993338884263115}
                        {:time 1652599382000
                         :lat  47.66866032
                         :lon  -122.31445263
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652599435000
                         :lat  47.66866067
                         :lon  -122.3144529
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652599467000
                         :lat  47.6686607
                         :lon  -122.3144532
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652599501000
                         :lat  47.66866009
                         :lon  -122.31445414
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652599534000
                         :lat  47.6686604
                         :lon  -122.31445408
                         :accuracy-miles 0.007327227310574521}
                        {:time 1652599580413
                         :lat  47.66865986
                         :lon  -122.31445468
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652599623000
                         :lat  47.66866003
                         :lon  -122.31445458
                         :accuracy-miles 0.012656119900083264}
                        {:time 1652599655000
                         :lat  47.66866
                         :lon  -122.31445411
                         :accuracy-miles 0.01132389675270608}
                        {:time 1652599713000
                         :lat  47.66866015
                         :lon  -122.31445425
                         :accuracy-miles 0.01931723625834193}
                        {:time 1652599746000
                         :lat  47.66866041
                         :lon  -122.31445364
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652599779000
                         :lat  47.66866029
                         :lon  -122.31445422
                         :accuracy-miles 0.005328892589508743}
                        {:time 1652599819000
                         :lat  47.66866085
                         :lon  -122.31445421
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652599852000
                         :lat  47.66866098
                         :lon  -122.31445412
                         :accuracy-miles 0.007327227310574521}
                        {:time 1652599885000
                         :lat  47.66866062
                         :lon  -122.31445422
                         :accuracy-miles 0.009991673605328892}
                        {:time 1652599918000
                         :lat  47.6686604
                         :lon  -122.31445442
                         :accuracy-miles 0.012656119900083264}
                        {:time 1652599951000
                         :lat  47.66879542
                         :lon  -122.31429538
                         :accuracy-miles 0.012656119900083264}
                        {:time 1652599984000
                         :lat  47.66868403
                         :lon  -122.31435937
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652600022000
                         :lat  47.66864453
                         :lon  -122.31438439
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652600055000
                         :lat  47.6686078
                         :lon  -122.31441197
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652600088000
                         :lat  47.66862337
                         :lon  -122.31441364
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652600121000
                         :lat  47.66862935
                         :lon  -122.31442559
                         :accuracy-miles 0.01132389675270608}
                        {:time 1652600168000
                         :lat  47.66861024
                         :lon  -122.314441
                         :accuracy-miles 0.007993338884263115}
                        {:time 1652600201000
                         :lat  47.66861116
                         :lon  -122.31444103
                         :accuracy-miles 0.012656119900083264}
                        {:time 1652600234000
                         :lat  47.66859643
                         :lon  -122.31444974
                         :accuracy-miles 0.010657785179017486}
                        {:time 1652600267000
                         :lat  47.6685929
                         :lon  -122.31446988
                         :accuracy-miles 0.0026644462947543715}
                        {:time 1652600300000
                         :lat  47.66860028
                         :lon  -122.31446246
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652600333000
                         :lat  47.66859874
                         :lon  -122.31446539
                         :accuracy-miles 0.005328892589508743}
                        {:time 1652600366000
                         :lat  47.66860456
                         :lon  -122.31446365
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652600411000
                         :lat  47.66860899
                         :lon  -122.31446719
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652600461000
                         :lat  47.66860808
                         :lon  -122.31446917
                         :accuracy-miles 0.0066611157368859295}
                        {:time 1652600495000
                         :lat  47.66861652
                         :lon  -122.31447442
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652600531000
                         :lat  47.6686256
                         :lon  -122.31447029
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652600565000
                         :lat  47.6686238
                         :lon  -122.31446307
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652600615000
                         :lat  47.66863005
                         :lon  -122.31446024
                         :accuracy-miles 0.013322231473771859}
                        {:time 1652600648000
                         :lat  47.66862588
                         :lon  -122.3144574
                         :accuracy-miles 0.005328892589508743}
                        {:time 1652600681000
                         :lat  47.66862708
                         :lon  -122.31445193
                         :accuracy-miles 0.0093255620316403}
                        {:time 1652600720000
                         :lat  47.66862689
                         :lon  -122.31445162
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652600769000
                         :lat  47.66863375
                         :lon  -122.31444813
                         :accuracy-miles 0.0033305578684429647}
                        {:time 1652600823000
                         :lat  47.66864786
                         :lon  -122.31444083
                         :accuracy-miles 0.0033305578684429647}
                        {:time 1652600855000
                         :lat  47.66864449
                         :lon  -122.31443979
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652600899000
                         :lat  47.66864291
                         :lon  -122.31444133
                         :accuracy-miles 0.008659451079324443}
                        {:time 1652600932000
                         :lat  47.66864203
                         :lon  -122.31443952
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652600965000
                         :lat  47.66863647
                         :lon  -122.31444599
                         :accuracy-miles 0.005995004784570073}
                        {:time 1652600998000
                         :lat  47.66863759
                         :lon  -122.31444504
                         :accuracy-miles 0.00466278101582015}
                        {:time 1652601046000
                         :lat  47.66864056
                         :lon  -122.31444726
                         :accuracy-miles 0.011990008947767408}
                        {:time 1652601079000
                         :lat  47.66863877
                         :lon  -122.31444558
                         :accuracy-miles 0.00466278101582015}] {}))
       
