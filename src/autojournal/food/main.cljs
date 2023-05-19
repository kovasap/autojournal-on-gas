(ns autojournal.food.main
  (:require [autojournal.gmail :as gmail]
            [autojournal.calendar :as calendar]
            [autojournal.time :refer [recent-items]]
            [autojournal.drive :as drive]
            [autojournal.food.food-db :refer [get-food-db add-db-data-to-meals]]
            ; This is apparently necessary for apps script to see this
            ; function.
            [autojournal.food.food-db-build :refer [make-new-food-db-sheet]]
            [autojournal.food.food-parsing :refer [row->meal meal->event]]
            [autojournal.food.report-email :refer [build-report-email]]
            [autojournal.food.common :refer [food-sheet-name]]))

; --------------- Main -----------------------------------------

(defn get-meals
  [days]
  (let [food-db   (get-food-db)
        all-meals (map row->meal (first (drive/get-files food-sheet-name)))]
    (if (= 0 (count all-meals))
      (str "No foods in last " days ", did you sync momentodb?")
      (add-db-data-to-meals (recent-items all-meals days)
                            food-db))))

(defn get-meal-events
  [days]
  (map meal->event (get-meals days)))


(defn report
  [days-to-summarize]
  (build-report-email (get-meals days-to-summarize) days-to-summarize))

(defn update-calendar!
  [days]
  (mapv calendar/add-event! (get-meal-events days)))
