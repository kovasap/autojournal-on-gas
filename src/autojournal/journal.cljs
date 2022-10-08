; Deprecates in favor of the "Journal" sheet
(ns autojournal.journal
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items]]
            [autojournal.drive :as drive]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :refer [join trim split]]
            [autojournal.schemas :refer [Event]]))

(def sheet-name "Journal")

; TODO add the rest of the items.
(def Entry
  [:map
   [:datetime JsDate]
   [:activity :string]
   [:raw-data :any]])


(defn get-elements
  [header row]
  (filter #(not (= "" %)) (map trim (split ((keyword header) row) #","))))


(defn row->entry
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Entry]}
  [row]
  {:datetime          (:Timestamp row)
   :raw-data          row
   :satisfied-reasons (get-elements "Satisfied Reason" row)
   :dissatisfied-reasons (get-elements "Dissatisfied Reason" row)
   :physical          (get-elements "Physical" row)
   :bathroom          (:Bathroom row)
   :activity          (:Activity row)})

(defn overall-mood
  [entry]
  (cond
    (= 0 (count (:satisfied-reasons entry))) "Dissatisfied"
    (= 0 (count (:dissatisfied-reasons entry))) "Satisfied"
    :else "Neutral"))

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
   :activity    (:activity entry)
   :summary     (str (overall-mood entry) ": " (:activity entry))
   :description (with-out-str (pprint (dissoc (:raw-data entry)
                                              :Timestamp
                                              :__id
                                              :Activity)))})

(defn update-calendar!
  [days]
  (let [all-entrys (map row->entry (first (drive/get-files sheet-name)))
        todays-entrys (recent-items all-entrys days)]
    (mapv calendar/add-event! (map entry->event todays-entrys))))
