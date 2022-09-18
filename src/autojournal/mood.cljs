(ns autojournal.mood
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items]]
            [autojournal.drive :as drive]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :refer [join trim]]
            [autojournal.schemas :refer [Event]]))

(def mood-sheet-name "Mood")

; TODO add the rest of the items.
(def Entry
  [:map
   [:datetime JsDate]
   [:activities [:sequential :string]]
   [:raw-data :any]])


(defn row->entry
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Entry]}
  [row]
  {:datetime   (:Timestamp row)
   :raw-data   row
   :activities (into [] (filter #(not (= (trim %) ""))
                          [(:Activity row)
                           ((keyword "Activity 2") row)
                           ((keyword "Activity 3") row)]))})

; Will probably need to roundtrip through strings to get daylight savings time
; right.
; The problem is that the timestamps in the momentodb table are local times
; without a timezone label, so when we convert them to unix timestamp longs
; here they are not actually correct, since unix timestamps are UTC.
(def pdt-offset (* 25200 1000))

(defn entry->event
  {:malli/schema [:=> [:cat Entry] Event]}
  [entry]
  {:start       (+ pdt-offset (to-long (:datetime entry)))
   :end         (+ pdt-offset
                   (to-long (plus (from-date (:datetime entry)) (minutes 30))))
   :activities  (:activities entry)
   :summary     (join ", " (:activities entry))
   :description (with-out-str (pprint (dissoc (:raw-data entry)
                                              :Timestamp
                                              :__id
                                              :Activity
                                              (keyword "Activity 2")
                                              (keyword "Activity 3"))))})
                                                              

(defn update-calendar!
  [days]
  (let [all-entrys (map row->entry (first (drive/get-files mood-sheet-name)))
        todays-entrys (recent-items all-entrys days)]
    (mapv calendar/add-event! (map entry->event todays-entrys))))
