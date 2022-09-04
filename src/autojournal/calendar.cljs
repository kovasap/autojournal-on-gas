(ns autojournal.calendar
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.schemas :refer [Event]]
            [cljs-time.coerce :refer [from-long]]))


(defn get-calendar-name
  {:malli/schema [:=> [:cat Event] :string]}
  [event]
  (cond
    (contains? event :lat) "Locations and Travel"
    :else ""))


(defn add-event!
  {:malli/schema [:=> [:cat Event] :nil]}
  [event]
  (env-switch
    {:node #(prn event)
     :app-script
     (fn []
       (let [calendar (. js/CalendarApp
                         (getCalendarsByName (get-calendar-name event)))]
         (. calendar (createEvent (:summary event)
                                  (from-long (:start event))
                                  (from-long (:end event))
                                  (clj->js {:description
                                            (:description event)})))))}))
