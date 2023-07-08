(ns autojournal.activitywatch
  (:require [autojournal.drive :as drive]
            [autojournal.time :refer [recent-items]]
            [autojournal.calendar :as calendar]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date from-string]]
            [cljs.pprint :refer [pprint]]
            [autojournal.schemas :refer [Event]]))

(def export-filename "aw-buckets-export.json")


(defn aw-event->entry
  [{:keys [timestamp duration] {:keys [app]} :data :as aw-event}
   aw-bucket-name]
  {:datetime (from-string timestamp)
   :duration duration
   :app-name app
   :bucket   aw-bucket-name
   :raw-data aw-event})

(defn parse-entries
  [raw-jsons]
  (reverse
   (for [filedata raw-jsons
         [bucket-name bucket-data] (:buckets filedata)
         aw-event (take 50 (:events bucket-data))
         :when (not (nil? (:duration aw-event)))]
     (aw-event->entry aw-event bucket-name))))

(defn get-entries
  []
  (prn "Raw Data" (drive/get-files export-filename))
  (parse-entries (drive/get-files export-filename)))

; 10 minutes
(def min-entry-size-secs (* 60 10))


(defn- merge-entries
  "Assume entry1 comes before entry2."
  [entry1 entry2]
  ; (prn entry1)
  ; (prn entry2)
  ; (prn)
  (let [entry1-is-merged? (not (string? (:app-name entry1)))]
    {:datetime (:datetime entry1)
     :duration (+ (/ (- (to-long (:datetime entry2))
                        (to-long (:datetime entry1)))
                     1000)
                  (:duration entry2))
     :app-name (if entry1-is-merged?
                 (conj (:app-name entry1) (:app-name entry2))
                 #{(:app-name entry1) (:app-name entry2)})
     :bucket (:aw-bucket-name entry1)
     :raw-data (if entry1-is-merged?
                 (conj (:app-name entry1) (:app-name entry2))
                 [(:raw-data entry1) (:raw-data entry2)])}))
  

; TODO I may need to do this on a per-bucket basis so entries that are from
; different devices are not condensed.
(defn condense-entries
  "Condense entries so that there are a manageable amount per day."
  ([entries] (condense-entries [] (first entries) (rest entries)))
  ([condensed-entries cur-entry remaining-entries]
   (let [grown-cur-entry
         (merge-entries cur-entry (first remaining-entries))]
    (cond
      (empty? remaining-entries)
      (conj condensed-entries cur-entry)
      (> (:duration grown-cur-entry) min-entry-size-secs)
      ; Our current entry has reached max size
      (condense-entries
        (conj condensed-entries cur-entry)
        (first remaining-entries)
        (rest remaining-entries))
      :else
      ; Our current entry is still growing
      (condense-entries
        condensed-entries
        grown-cur-entry
        (rest remaining-entries))))))
  

(condense-entries
  (parse-entries
    [{:buckets
      {:aw-watcher-android-test
       {:events
        [{:data      {:app       "ActivityWatch"
                      :classname "net.activitywatch.android.MainActivity"
                      :package   "net.activitywatch.android"}
          :duration  0
          :id        95045
          :timestamp "2023-07-08T16:20:55.235Z"}
         {:data      {:app       "Nova7"
                      :classname "com.teslacoilsw.launcher.NovaLauncher"
                      :package   "com.teslacoilsw.launcher"}
          :duration  28.309
          :id        95044
          :timestamp "2023-07-08T16:20:26.875Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.alarmclock.AlarmClock"
                      :package   "com.urbandroid.sleep"}
          :duration  3.373
          :id        95042
          :timestamp "2023-07-08T16:20:23.322Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.alarmclock.AlarmClock"
                      :package   "com.urbandroid.sleep"}
          :duration  11.767
          :id        95040
          :timestamp "2023-07-08T16:12:36.877Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.alarmclock.AlarmClock"
                      :package   "com.urbandroid.sleep"}
          :duration  2.075
          :id        95039
          :timestamp "2023-07-08T15:53:53.061Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.alarmclock.SetAlarm"
                      :package   "com.urbandroid.sleep"}
          :duration  8.372
          :id        95038
          :timestamp "2023-07-08T15:53:44.642Z"}
         {:data      {:app "Sleep"
                      :classname
                      "com.urbandroid.sleep.alarmclock.RatingActivity"
                      :package "com.urbandroid.sleep"}
          :duration  5.675
          :id        95037
          :timestamp "2023-07-08T15:53:38.928Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.alarmclock.SetAlarm"
                      :package   "com.urbandroid.sleep"}
          :duration  1.054
          :id        95036
          :timestamp "2023-07-08T15:53:37.857Z"}
         {:data      {:app       "Sleep"
                      :classname "com.urbandroid.sleep.Sleep"
                      :package   "com.urbandroid.sleep"}
          :duration  3.474
          :id        95035
          :timestamp "2023-07-08T15:53:34.359Z"}]}}}]))


(defn entry->event
  [{:keys [datetime duration app-name raw-data]}]
  {:start       (to-long datetime)
   :end         (+ (to-long datetime) (* 1000 duration)) 
   :app-name    app-name
   :summary     (str app-name)
   :description (with-out-str (pprint (dissoc raw-data
                                              :Timestamp
                                              :__id
                                              :Activity)))})

(defn update-calendar!
  [days]
  (let [all-entrys (condense-entries (get-entries))
        recent-entries (recent-items all-entrys days)]
    (prn "Processed entries" all-entrys)
    (mapv calendar/add-event! (map entry->event recent-entries))))
