(ns autojournal.calendar
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.schemas :refer [Event]]
            [cljs-time.coerce :refer [from-long to-date to-long]]))


(def get-cal-by-name-cached
  (memoize (fn [cal-name]
             (first (. js/CalendarApp (getCalendarsByName cal-name))))))

; TODO use malli types here to match instead of single keys
(defn get-calendar
  {:malli/schema [:=> [:cat Event] :any]}
  [event]
  (get-cal-by-name-cached
    (cond
       (contains? event :lat) "Locations and Travel"
       (contains? event :foods) "Food"
       (contains? event :activities) "Mood"
       (contains? event :activity) "Journal"
       :else "")))

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
    {:node #(prn event),
     :app-script (fn []
                   (let [calendar (get-calendar event)
                         [start-time end-time] (get-js-start-end-times event)]
                     (prn "Adding" (:summary event))
                     (delete-duplicate-event! event)
                     (. calendar
                        (createEvent (:summary event)
                                     start-time
                                     end-time
                                     (clj->js {:description (:description
                                                              event)})))))}))
