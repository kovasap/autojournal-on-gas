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
            [autojournal.food.common :refer [DAYS-TO-SUMMARIZE food-sheet-name]]))

; --------------- Main -----------------------------------------

(defn send-report
  []
  (let [food-db (get-food-db)
        all-meals (map row->meal (first (drive/get-files food-sheet-name)))
        email-body (if (= 0 (count all-meals))
                     (str "No foods in last " DAYS-TO-SUMMARIZE
                          ", did you sync momentodb?")
                     (-> (recent-items all-meals DAYS-TO-SUMMARIZE)
                         (add-db-data-to-meals food-db)
                         (build-report-email)))]
    (gmail/send-self-mail "Daily Report" email-body)))


(defn update-calendar!
  [days]
  (let [all-meals (map row->meal (first (drive/get-files food-sheet-name)))
        todays-meals (recent-items all-meals days)]
    (mapv calendar/add-event! (map meal->event todays-meals))))
