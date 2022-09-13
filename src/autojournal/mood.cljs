(ns autojournal.mood
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items]]
            [autojournal.schemas :refer [Event]]))

; TODO add the rest of the items.
(def Entry
  [:map
   [:datetime JsDate]
   [:activities [:sequential :string]]])


(defn row->meal
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Meal]}
  [row]
  {:datetime (:Timestamp row)
   :foods    (parse-foods (:Foods row))
   :oil      ((keyword "Oil Amount") row)
   :picture  (:Picture.http row)})

; Will probably need to roundtrip through strings to get daylight savings time
; right.
; The problem is that the timestamps in the momentodb table are local times
; without a timezone label, so when we convert them to unix timestamp longs
; here they are not actually correct, since unix timestamps are UTC.
(def pdt-offset (* 25200 1000))

(defn meal->event
  {:malli/schema [:=> [:cat Meal] Event]}
  [meal]
  {:start       (+ pdt-offset (to-long (:datetime meal)))
   :end         (+ pdt-offset
                   (to-long (plus (from-date (:datetime meal)) (minutes 15))))
   :summary     "Meal"
   :foods       (:foods meal)
   :description (with-out-str (pprint (:foods meal)))})

(defn update-calendar!
  []
  (let [all-meals (map row->meal (first (drive/get-files food-sheet-name)))
        todays-meals (recent-meals all-meals 1)]
    (prn "MOODS")
    (prn todays-meals)
    (mapv calendar/add-event! (map meal->event todays-meals))))
