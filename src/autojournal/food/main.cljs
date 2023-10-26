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

(defn no-meals-str
  [days]
  (str "No foods in last " days ", did you sync momentodb?"))


(defn get-meals-with-db-data
  "Filter function takes in a seq of meals and returns a subset."
  [filter-fn]
  (let [food-db   (get-food-db)
        all-meals (map row->meal (first (drive/get-files food-sheet-name)))]
    (if (= 0 (count all-meals))
      (do (prn "No meals returned!") [])
      (add-db-data-to-meals (filter-fn all-meals)
                            food-db))))

(defn get-meals
  "Filter function takes in a seq of meals and returns a subset."
  [filter-fn]
  (filter-fn (remove nil?
               (map row->meal (first (drive/get-files food-sheet-name))))))

(defn get-recent-meals-with-db-data
  [days]
  (get-meals #(recent-items % days)))

(defn get-last-meals
  [n]
  (get-meals #(take-last n %)))

(defn get-last-meal-events
  [n]
  (map meal->event (get-last-meals n)))


(defn report
  [days-to-summarize]
  (let [meals (get-recent-meals-with-db-data days-to-summarize)]
    (build-report-email (if (seq meals) meals (no-meals-str days-to-summarize))
                        days-to-summarize)))

(defn update-calendar!
  [days]
  (mapv calendar/add-event!
        (map meal->event (get-recent-meals-with-db-data days))))
