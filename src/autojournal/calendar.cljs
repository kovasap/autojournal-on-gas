(ns autojournal.calendar
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.schemas :refer [Event]]
            [cljs-time.coerce :refer [from-long to-date to-long]]))


(def get-cal-by-name-cached
  (memoize (fn [cal-name]
             (first (. js/CalendarApp (getCalendarsByName cal-name))))))

(def managed-calendar-names
  #{"Locations and Travel" "Food" "Mood" "Journal" "Android ActivityWatch"
    "Laptop ActivityWatch" "Desktop ActivityWatch"})

(defn get-calendar-name
  [event]
  (cond 
        (contains? event :calendar)        (:calendar event)
        (contains? event :lat)        "Locations and Travel"
        (contains? event :foods)      "Food"
        (contains? event :activities) "Mood"
        (contains? event :activity)   "Journal"
        (= (:bucket event) :aw-watcher-android-test) "Android ActivityWatch"
        (= (:bucket event) :aw-watcher-window_kovas2) "Laptop ActivityWatch"
        (= (:bucket event) :aw-watcher-window_frosty) "Desktop ActivityWatch"
        :else                         ""))

; TODO use malli types here to match instead of single keys
(defn get-calendar
  {:malli/schema [:=> [:cat Event] :any]}
  [event]
  (get-cal-by-name-cached (get-calendar-name event)))
    

(defn get-js-start-end-times
  [event]
  [(to-date (from-long (:start event)))
   (to-date (from-long (:end event)))])


(defn =tol [a b] (> 1000 (Math/abs (- a b))))


(defn delete-duplicate-event!
  [event]
  (env-switch
    {:node       #(prn event)
     :app-script (fn []
                   (let [[start-time end-time] (get-js-start-end-times event)
                         calendar        (get-calendar event)
                         existing-events (. calendar
                                            (getEvents start-time end-time))]
                     (doseq [cal-event existing-events]
                       (if (=tol (to-long (. cal-event getStartTime))
                                 (:start event))
                         (. cal-event deleteEvent)))))}))

(defn add-event!
  {:malli/schema [:=> [:cat Event] :nil]}
  [event]
  (env-switch
    {:node       #(prn event)
     :app-script (fn []
                   (let [calendar (get-calendar event)
                         [start-time end-time] (get-js-start-end-times event)]
                     (if (and (not (nil? calendar))
                              (not (nil? event))
                              (> (count event) 0))
                       (do (prn "Adding "       (:summary event)
                                " to calendar " (get-calendar-name event))
                           (try
                            (delete-duplicate-event! event)
                            (. calendar
                               (createEvent (:summary event)
                                          start-time
                                          end-time
                                          (clj->js
                                            {:location    (:location event)
                                             :description (:description
                                                            event)})))
                            (catch :default e
                              (prn "ERROR: " e))))
                       (prn "Cound not add " event
                            " to calendar "  (get-calendar-name event)))))}))

(defn clean-events!
  [days]
  (env-switch
    {:node       #(prn "Cleaning")
     :app-script (fn []
                   (doseq [calendar-name managed-calendar-names]
                     (let [now             (js/Date.)
                           days-ago        (js/Date.
                                             (- (.getTime now)
                                                (* 1000 60 60 24 days)))
                           calendar        (get-cal-by-name-cached
                                             calendar-name)
                           existing-events (. calendar
                                              (getEvents days-ago now))]
                       (prn "Date range: " now days-ago)
                       (prn "Cleaning " (count existing-events)
                            " from "    calendar-name)
                       (doseq [cal-event existing-events]
                         (. cal-event deleteEvent)))))}))
