(ns autojournal.activitywatch
  (:require [autojournal.drive :as drive]
            [autojournal.time :refer [recent-items]]
            [autojournal.calendar :as calendar]
            [clojure.string :refer [join split]]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date from-string]]
            [cljs.pprint :refer [pprint]]
            [autojournal.schemas :refer [Event]]))

; TODO make this a pattern instead
(def export-filenames
  ["aw-buckets-export.json"
   "aw-buckets-export-frosty.json"
   "aw-buckets-export-kovas2.json"])

(def buckets-to-ignore
  #{:aw-watcher-android-unlock :aw-watcher-afk_frosty :aw-watcher-afk_kovas2})


(defn aw-event->entry
  [{:keys [timestamp duration] {:keys [app title]} :data :as aw-event}
   aw-bucket-name]
  ; Remove the end of timestamps like "2023-07-07T00:27:28.387000+00:00"
  {:datetime (from-string (first (split timestamp #"\.")))
   :duration duration
   :app-name app
   :title title
   :bucket   aw-bucket-name
   :raw-data aw-event})

(defn parse-entries
  [raw-jsons]
  (reverse
   (for [filedata raw-jsons
         [bucket-name bucket-data] (:buckets filedata)
         aw-event (take 1000 (:events bucket-data))
         :when (and
                 (not (contains? buckets-to-ignore bucket-name))
                 (not (nil? (:duration aw-event))))]
     (aw-event->entry aw-event bucket-name))))

(defn get-entries
  [export-filename]
  ; (prn "Raw Data" (drive/get-files export-filename))
  (parse-entries (drive/get-files export-filename)))

; 15 minutes
(def min-entry-size-secs (* 60 15))


(defn- conj-not-nil
  [coll x]
  (if (not (nil? x))
    (conj coll x)
    coll))

(defn- merge-entries
  "Assume entry1 comes before entry2."
  [entry1 entry2]
  (let [entry1-is-merged? (set? (:app-name entry1))]
    {:datetime (:datetime entry1)
     :duration (+ (/ (- (to-long (:datetime entry2))
                        (to-long (:datetime entry1)))
                     1000)
                  (:duration entry2))
     :app-name (conj-not-nil (if entry1-is-merged?
                               (:app-name entry1)
                               #{(:app-name entry1)})
                             (:app-name entry2))
     :bucket   (:bucket entry1)
     :raw-data (conj-not-nil (if entry1-is-merged?
                               (:raw-data entry1)
                               [(:raw-data entry1)])
                             (:raw-data entry2))}))

(defn get-end-time-secs
  [entry]
  (+ (/ (to-long (:datetime entry)) 1000)
     (:duration entry)))
  
(defn condense-entries
  "Condense entries so that there are a manageable amount per day."
  ([entries] (condense-entries [] (first entries) (rest entries)))
  ([condensed-entries cur-entry remaining-entries]
   (let [grown-cur-entry (merge-entries cur-entry (first remaining-entries))
         next-entry-start-time-secs
         (/ (to-long (:datetime (second remaining-entries))) 1000)]
     (cond
       (empty? remaining-entries)
       ; Base case - done processing
       (conj condensed-entries cur-entry)
       (and (> (:duration grown-cur-entry) min-entry-size-secs)
            (or (not next-entry-start-time-secs)
                (> (+ 30 next-entry-start-time-secs)
                   (get-end-time-secs grown-cur-entry))))
       ; Our current entry has reached max size and the next entries are not
       ; occuring right after this condensed one.
       (condense-entries (conj condensed-entries cur-entry)
                         (first remaining-entries)
                         (rest remaining-entries))
       :else
       ; Our current entry is still growing
       (condense-entries condensed-entries
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

(def app-names-to-hide
  #{"Nova7" "One UI Home" "Android System"
    ; Windows
    "LockApp.exe"})

(defn app-set->str
  [app-set]
  (join ", " (filter #(not (contains? app-names-to-hide %)) app-set)))

(defn entry->event
  [{:keys [datetime duration app-name bucket raw-data] :as event}]
  (let [most-used-title (:title (:data (if (vector? raw-data)
                                         (first (reverse (sort-by :duration
                                                                  raw-data)))
                                         raw-data)))]
    {:start       (to-long datetime)
     :end         (+ (to-long datetime) (* 1000 duration))
     :bucket      bucket
     :summary     (cond most-used-title most-used-title
                        (set? app-name) (app-set->str app-name)
                        :else           app-name)
     :description (with-out-str (pprint event))}))

(defn update-calendar!
  [days]
  (doseq [export-filename export-filenames]
    (let [all-entrys        (time (get-entries export-filename))
          recent-entries    (recent-items all-entrys days)
          entries-by-bucket (group-by :bucket recent-entries)]
      ; reverse is just here to make the android entries parse last
      (doall (for [[bucket entries] (reverse entries-by-bucket)
                   :let [condensed-entries (condense-entries entries)]]
               (do (prn "Processed "  (count all-entrys)
                        " uploading " (count condensed-entries)
                        " condensed entries for bucket " bucket)
                   (mapv calendar/add-event!
                     (map entry->event condensed-entries))))))))
