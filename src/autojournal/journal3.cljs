; Deprecates in favor of the "Journal" sheet
(ns autojournal.journal3
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items]]
            [autojournal.html-utils :refer [Hiccup make-table]]
            [autojournal.drive :as drive]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :refer [join trim split]]
            [autojournal.schemas :refer [Event]]))

(def sheet-name "Journal 3.0")

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
  {:datetime     (:Timestamp row)
   :raw-data     row
   :category     (:Category row)
   :bathroom     (:Bathroom row)
   :mood         (:Mood row)
   :valence      (:Valence row)
   :energy       ((keyword "Energy and Inspiration") row)
   :mood-details ((keyword "Mood Details") row)
   :activity     (:Activity row)})

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
   :summary     (str (:valence entry) ": " (:activity entry))
   :description (with-out-str (pprint (dissoc (:raw-data entry)
                                              :Timestamp
                                              :__id
                                              :Activity)))})

(defn get-recent-entries
  {:malli/schema [:=> [:cat :int] [:sequential Entry]]}
  [days]
  (let [all-entrys (map row->entry (first (drive/get-files sheet-name)))]
    (recent-items all-entrys days)))


(defn report
  "Generates a hiccup summary of recent entries"
  {:malli/schema [:=> [:cat [:sequential Entry] :int] Hiccup]}
  [days-to-summarize]
  (let [entries (get-recent-entries days-to-summarize)]
    [:div
     [:h1 "Last " days-to-summarize " Day Mood Summary"]
     (make-table ["Category"
                  "Activity"
                  "Valence"
                  "Energy"
                  "Mood"
                  "Mood Details"
                  "Bathroom"]
                 (reverse (sort-by :datetime
                                   (for [entry entries]
                                     [(:category entry)
                                      (:activity entry)
                                      (:valence entry)
                                      (:energy entry)
                                      (:mood entry)
                                      (:mood-details entry)
                                      (:bathroom entry)]))))]))

(defn update-calendar!
  [days]
  (mapv calendar/add-event!
    (map entry->event (get-recent-entries days))))
