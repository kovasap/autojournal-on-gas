; Deprecates in favor of the "Journal" sheet
(ns autojournal.journal5
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items]]
            [autojournal.html-utils :refer [Hiccup make-table]]
            [autojournal.drive :as drive]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as st]
            [autojournal.schemas :refer [Event]]))

(def sheet-name "Journal 5.0 Simple")

; TODO add the rest of the items.
(def Entry
  [:map
   [:datetime JsDate]
   [:activity :string]
   [:raw-data :any]])


(defn get-elements
  [header row]
  (filter #(not (= "" %))
    (map st/trim (st/split ((keyword header) row) #","))))


(defn row->entry
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] [:vec Entry]]}
  [row]
  (for [activity (st/split #"," (:Activity row))]
    {:datetime   (:Timestamp row)
     :raw-data   row
     :valence    (:Valence row)
     :energy     (:Energy/Motivation row)
     :notes      (:Notes row)
     :digestion  (:Digestion row)
     :engagement ((keyword "Activity Engagement") row)
     :activity   activity}))

; Will probably need to roundtrip through strings to get daylight savings time
; right.
; The problem is that the timestamps in the momentodb table are local times
; without a timezone label, so when we convert them to unix timestamp longs
; here they are not actually correct, since unix timestamps are UTC.
(def pdt-offset (* 25200 1000))


(def valence-to-number {"Meh" 0 "OK" 1 "Good" 2 "Great" 3})

(defn entry->event
  {:malli/schema [:=> [:cat Entry] Event]}
  [entry]
  {:start       (+ pdt-offset (to-long (:datetime entry)))
   :end         (+ pdt-offset
                   (to-long (plus (from-date (:datetime entry)) (minutes 60))))
   :activity    (:activity entry)
   :energy      (:energy entry)
   :digestion   (:digestion entry)
   :valence     (:valence entry)
   #_(get valence-to-number (:valence entry) -1)
   :summary     (str (:valence entry) ": " (:activity entry))
   :description (with-out-str
                  (pprint
                    (dissoc (:raw-data entry) :Timestamp :__id :Activity)))})

(defn get-all-entries
  []
  (reduce concat (map row->entry (first (drive/get-files sheet-name)))))

(defn get-recent-entries
  {:malli/schema [:=> [:cat :int] [:sequential Entry]]}
  [days]
  (recent-items (get-all-entries) days))


(defn get-events
  []
  (map entry->event (get-all-entries)))


(defn make-entry-table
  {:malli/schema [:=> [:cat [:sequential Entry]] Hiccup]}
  ([entries columns] (make-entry-table entries columns reverse))
  ([entries columns sort-modifier]
   (make-table 
     (map name columns)
     (sort-modifier (sort-by :datetime
                       (for [entry entries]
                         (map #(% entry) columns)))))))


(def table-keys [:activity :engagement :valence :energy :digestion :notes])


(defn report
  "Generates a hiccup summary of recent entries"
  {:malli/schema [:=> [:cat :int] Hiccup]}
  [days-to-summarize]
  (let [entries (get-recent-entries days-to-summarize)]
    [:div
     [:h1 "Last " days-to-summarize " Day Mood Summary"]
     (make-entry-table entries table-keys)]))


(defn update-calendar!
  [days]
  (mapv calendar/add-event!
    (map entry->event (get-recent-entries days))))
