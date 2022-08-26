(ns autojournal.core
  ; We import all custom malli schemas to avoid undeclared-var error when malli
  ; goes to do the schema checking in the refresh function.
  (:require [autojournal.sheets :as sheets]
            [autojournal.schemas :refer [Timestamp EventFetcher Event Date]]
            [autojournal.food-summary :as food-summary]
            [autojournal.drive :as drive]
            [autojournal.location :as location :refer [Reading TallyFunction]]
            [autojournal.env-switching :refer [env-switch]]
            [cljs-time.core :refer [date-time]]
            [malli.core :as m]
            [cljs-time.coerce :refer [to-long]]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export update-lifelog []
  (prn "hi")
  (prn (into [] (for [file (drive/get-files "20220515.zip")]
                  file)))
  (sheets/update-events!
    (location/get-events
      (to-long (date-time 2022 05 15))
      (to-long (date-time 2022 05 17))))
  (sheets/append! "1ZDPrV6ZngilK00Pb0DVs64yAVs6YQtiLr_vE5-YCiLc"
                  ["hello" "world"]))

(defn ^:export summarize-food []
  (food-summary/send-daily-report))

(defn main [])

(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      #(do (prn "Hot code Remount")
           ; Check all malli function schemas
           (dev/start! {:report (pretty/reporter)}))
     :app-script #()}))
