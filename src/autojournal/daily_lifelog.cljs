(ns autojournal.daily-lifelog
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.time :refer [JsDate recent-items get-day-str]]
            [autojournal.html-utils :refer [Hiccup make-table]]
            [autojournal.drive :as drive]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [date-time hour day minute second month year
                                    plus minutes]]
            [cljs-time.coerce :refer [to-long from-date to-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as st]
            [autojournal.schemas :refer [Event]]))

(def sheet-name "Daily Lifelog")

; TODO add the rest of the items.
(def Entry
  [:map
   [:datetime JsDate]
   [:activity :string]
   [:raw-data :any]])

(defn row->entry
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Entry]}
  [row]
  (let [rdate (from-date (:Date row))
        rtime (from-date (:Time row))]
    {:datetime     (to-date (date-time (year rdate) (month rdate)
                                       (day rdate)
                                       (hour rtime) (minute rtime)
                                       (second rtime)))
     :raw-data     row
     :valence      (:Valence row)
     :importance   (:Importance row)
     :experience   (:Experience row)
     :picture-link (:Picture.http row)
     :tags         (into #{} (st/split (:Tags row) #" "))}))

; Will probably need to roundtrip through strings to get daylight savings time
; right.
; The problem is that the timestamps in the momentodb table are local times
; without a timezone label, so when we convert them to unix timestamp longs
; here they are not actually correct, since unix timestamps are UTC.
(def pst-offset (* 8 60 60 1000))


(defn get-emphasis
  [entry]
  (case (:importance entry)
    "Normal"        ""
    "Novel"         "**"
    "Important"     "!!"
    "Life Changing" "~~!!~~"
    ""))

(defn entry->event
  {:malli/schema [:=> [:cat Entry] Event]}
  [entry]
  (merge 
    (select-keys entry [:importance :valence :tags :experience])
    {:start       (+ pst-offset (to-long (:datetime entry)))
     :end         (+ pst-offset
                     (to-long (plus (from-date (:datetime entry)) (minutes 60))))
     :calendar    (cond
                    (contains? (:tags entry) "food") "Lifelog Food"
                    (contains? (:tags entry) "sleep") "Lifelog Sleep"
                    (contains? (:tags entry) "work") "Lifelog Work"
                    (contains? (:tags entry) "exercise") "Lifelog Exercise"
                    :else "Lifelog")
     :summary     (str (get-emphasis entry) (:experience entry)
                       (get-emphasis entry))
     :description (with-out-str
                    (pprint
                      (dissoc (:raw-data entry) :__id)))}))

(defn get-all-entries
  []
  (map row->entry (first (drive/get-files sheet-name))))

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

(def table-keys [:valence :importance :experience :tags])

(defn entries->sentence-summaries-by–day
  [entries]
  (into {}
        (for [[day-str daily-entries] (group-by #(get-day-str (:datetime %))
                                                entries)]
          [day-str
           (st/join ".  "
                    (->> daily-entries
                         (sort-by :datetime)
                         ; (filter #(not (contains? (:tags %) "sleep")))
                         (map :experience)))])))

(defn make-all-sentence-summaries-by–day
  []
  (entries->sentence-summaries-by–day (get-all-entries)))

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
