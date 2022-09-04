(ns autojournal.core
  ; We import all custom malli schemas to avoid undeclared-var error when malli
  ; goes to do the schema checking in the refresh function.
  (:require [autojournal.sheets :as sheets]
            [autojournal.calendar :as calendar]
            [autojournal.schemas :refer [Timestamp EventFetcher Event Date]]
            [autojournal.food-summary :as food-summary]
            [autojournal.drive :as drive]
            [autojournal.location :as location :refer [Reading TallyFunction]]
            [autojournal.env-switching :refer [env-switch]]
            [cljs-time.core :refer [date-time today minus days]]
            [malli.core :as m]
            [cljs-time.coerce :refer [to-long]]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export update-lifelog-with-today []
  (let [today (today)
        yesterday (minus today (days 1))
        events (location/get-events
                 (to-long yesterday)
                 (to-long today))]
    (calendar/add-event! (first events))
    (sheets/update-events! events)))

(defn ^:export summarize-food []
  (food-summary/send-report))

(defn main [])

(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      #(do (prn "Hot code Remount")
           ; Check all malli function schemas
           (dev/start! {:report (pretty/reporter)}))
     :app-script #()}))
