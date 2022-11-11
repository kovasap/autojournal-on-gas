(ns autojournal.food-and-journal
  (:require [autojournal.journal5 :as journal5]
            [autojournal.food.main :as food]
            [clojure.string :as st]
            [autojournal.food.common :refer [Meal]]
            [cljs-time.core :refer [plus seconds]]
            [cljs-time.coerce :refer [from-date]]
            [cljs-time.format :refer [unparse formatters]]))


(defn entries-by-meal
  "All entries after a meal, up until the next meal."
  {:malli/schema [:=> [:cat [:sequential journal5/Entry]
                            [:sequential Meal]]
                  [:map-of Meal [:sequential journal5/Entry]]]}
  [entries meals]
  (into {} (for [[meal next-meal]
                 (partition 2 1 (conj (vec meals)
                                      ; Dummy last date so that we don't skip
                                      ; the last meal's entries.
                                      {:datetime (js/Date.)}))]
             [meal (filter (fn [entry]
                             (> (:datetime next-meal)
                                (:datetime entry)
                                (:datetime meal)))
                           entries)])))

(defn group-meals
  [days]
  (let [journal-entries (journal5/get-recent-entries days)
        meals (food/get-meals days)]
    (into
      [:div]
      (for [[meal entries] (entries-by-meal journal-entries meals)]
        [:div
         [:div (unparse (formatters :hour-minute)
                        (from-date (:datetime meal)))]
                        ; Time zone correction
                        ; (plus (from-date (:datetime meal)) (seconds 25200)))]
         [:div (st/join ", " (map :name (:foods meal)))]
         [:br]
         (journal5/make-entry-table entries journal5/table-keys identity)
         [:br]]))))

