(ns autojournal.calendar
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.schemas :refer [Event]]
            [cljs-time.coerce :refer [from-long to-date]]))


; TODO use atom to cache calls if necessary
(defn get-calendar
  {:malli/schema [:=> [:cat Event] :any]}
  [event]
  (let [cal-name (cond
                    (contains? event :lat) "Locations and Travel"
                    (contains? event :foods) "Food"
                    :else "")]
    (first (. js/CalendarApp
              (getCalendarsByName cal-name)))))


(defn get-js-start-end-times
  [event]
  [(to-date (from-long (:start event)))
   (to-date (from-long (:end event)))])

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
                       (if (= (. cal-event getStartTime) start-time)
                         (. cal-event deleteEvent)))))}))

(defn add-event!
  {:malli/schema [:=> [:cat Event] :nil]}
  [event]
  (env-switch
    {:node #(prn event),
     :app-script (fn []
                   (let [calendar (get-calendar event)
                         [start-time end-time] (get-js-start-end-times event)]
                     (prn "EVEND")
                     (prn event)
                     (delete-duplicate-event! event)
                     (. calendar
                        (createEvent (:summary event)
                                     start-time
                                     end-time
                                     (clj->js {:description (:description
                                                              event)})))))}))
