(ns autojournal.core
  ; We import all custom malli schemas to avoid undeclared-var error when malli
  ; goes to do the schema checking in the refresh function.
  (:require [autojournal.sheets :as sheets]
            [autojournal.calendar :as calendar]
            [autojournal.sleep :as sleep]
            [autojournal.gmail :as gmail]
            [autojournal.activitywatch :as activitywatch]
            [autojournal.html-utils :refer [Hiccup]]
            [autojournal.schemas :refer [Timestamp EventFetcher Event Date]]
            [autojournal.food.main :as food]
            [autojournal.drive :as drive]
            [autojournal.mood :as mood]
            [autojournal.journal5 :as journal5 :refer [Entry]]
            [autojournal.food-and-journal :as food-and-journal]
            [autojournal.food.common :refer [Meal NutrientName Food]]
            [autojournal.food.report-email :refer [PercentOfTarget]]
            [autojournal.food.food-db :refer [PhraseMatch FoodDB]]
            [autojournal.food.food-db-build :refer [RawCronFood]]
            [autojournal.sleep :refer [Night]]
            [autojournal.location :as location :refer [Reading TallyFunction]]
            [autojournal.time :refer [JsDate]]
            [autojournal.env-switching :refer [env-switch]]
            [autojournal.continuous-glucose-monitoring :as cgm]
            [autojournal.vega
             :refer
             [make-all-event-plots write-vega-page timeline-plot-types]]
            [cljs-time.core :refer [date-time today minus days]]
            [malli.core :as m]
            [cljs-time.coerce :refer [to-long]]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export clean-last-week-events
  []
  (calendar/clean-events! 5))

(defn ^:export write-report
  []
  (let [cgm-data     (remove
                       #(js/Number.isNaN (:glucose %))
                       (cgm/get-data "KovasPalunas_glucose_4-21-2021.csv"))
        journal-data (journal5/get-events)
        food-data    (food/get-last-meal-events 1000)]
    (write-vega-page
      "vega.html"
      (make-all-event-plots
        (concat cgm-data journal-data food-data)
        {"glucose"    {:timeline-type :line :aggregation "mean"}
         ; "valence" {:timeline-type :ordinal-line
         ;            :timeline-args {:sort-order
         ;                            ["Meh" "OK" "Good" "Great" "Amazing"]}
         ;            :aggregation   "count"}
         "food-count" {:timeline-type :line
                       :timeline-args {:tooltip-key :image
                                       :label-key   :food-str}}}))))

; TODO use sheets/update-events! to also write this data to a google sheet.
(defn ^:export update-lifelog []
  (let [days-to-update 2]
      (time (activitywatch/update-calendar! days-to-update))
      ; (mood/update-calendar! days-to-update)
      (time (journal5/update-calendar! days-to-update))
      (time (food/update-calendar! days-to-update))
      ; TODO make it so that some locations are named, based on a google drive
      ; spreadsheet with columns (lat, long, radius (size of place), name)
      (time (location/update-calendar! days-to-update))))


(defn ^:export send-report-email
  {:malli/schema [:=> [:cat :int] Hiccup]}
  [days-to-summarize]
  (gmail/send-self-mail "Daily Report"
    [:html
     [:head]
     [:body
      (food-and-journal/group-meals days-to-summarize)
      (journal5/report days-to-summarize)
      (sleep/report days-to-summarize)
      (food/report days-to-summarize)]]))


(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      #(do (prn "Hot code Remount"))
           ; Check all malli function schemas
           ; TODO fix this and uncomment
           ; (dev/start! {:report (pretty/reporter)}))
     :app-script #()}))
