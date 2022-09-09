(ns autojournal.food.main
  (:require [autojournal.sheets :as sheets]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [node-only assert=]]
            [autojournal.food.food-db :refer [get-food-db add-db-data-to-meals]]
            [autojournal.food.food-parsing :refer [row->meal]]
            [autojournal.food.report-email :refer [build-report-email]]
            [autojournal.food.common :refer [DAYS-TO-SUMMARIZE food-sheet-name Meal]]
            [cljs.pprint :refer [pprint]]
            [clojure.string :as st
             :refer [split split-lines lower-case join
                     starts-with? includes? trim]]))

; --------------- Main -----------------------------------------

(defn days-between
  [date1 date2]
  (let [diff_ms (abs (- (.getTime date1) (.getTime date2)))]
    (/ diff_ms 1000.0 60 60 24)))

(defn recent-meals
  {:malli/schema [:=> [:cat [:sequential Meal]] [:sequential Meal]]}
  [food-data]
  (let [today (js/Date.)]
    (filter #(< (days-between (:datetime %) today) DAYS-TO-SUMMARIZE)
            food-data)))

(defn send-report
  []
  (prn (first (drive/get-files food-sheet-name)))
  (let [food-db (get-food-db)
        all-meals (map row->meal (first (drive/get-files food-sheet-name)))
        email-body (if (= 0 (count all-meals))
                     (str "No foods in last " DAYS-TO-SUMMARIZE
                          ", did you sync momentodb?")
                     (-> (recent-meals all-meals)
                         (add-db-data-to-meals food-db)
                         (build-report-email)))]
    (gmail/send-self-mail "Daily Report" email-body)))

; Necessary to def these because the map gets confused by the extra #inst token
(def t1 #inst "2022-08-11T18:53:12.275-00:00")
(def t2 #inst "2022-08-11T18:58:45.096-00:00")
(def t3 #inst "2022-08-11T19:12:06.700-00:00")
(def t4 #inst "2022-08-11T19:20:21.938-00:00")

(def test-food-db 
  {"potatoes russet flesh and skin baked"
   {"merium"
    {:name "Potatoes, Russet, Flesh and Skin, Baked",
     :quantity 1,
     :category "roots"
     :quantity-units "medium",
     :nutrients {"Energy (kcal)" 164.35, "Carbs (g)" 37.09}}},
   "potatoes"
   {"medium"
    {:name "Potatoes, Russet, Flesh and Skin, Baked",
     :quantity 1,
     :category "roots"
     :quantity-units "medium",
     :nutrients {"Energy (kcal)" 164.35, "Carbs (g)" 37.09}}}
   "steamed rice"
   {"g"
    {:name "White Rice, steamed",
     :quantity 100,
     :category "grains"
     :quantity-units "g",
     :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}},
    "cup"
    {:name "White Rice, steamed",
     :quantity 1,
     :category "grains"
     :quantity-units "cup",
     :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}}},
   "rice"
   {"g"
    {:name "White Rice, steamed",
     :quantity 100,
     :category "grains"
     :quantity-units "g",
     :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}},
    "cup"
    {:name "White Rice, steamed",
     :quantity 1,
     :category "grains"
     :quantity-units "cup",
     :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}}}
   "cauliflower"
   {"cup"
    {:name "Cauliflower",
     :quantity 1,
     :category "Vegetable"
     :quantity-units "cup",
     :nutrients {"Energy (kcal)" 50, "Carbs (g)" 0}}}
   "carrot"
   {"unit"
    {:name "Carrot",
     :quantity 1,
     :category "Vegetable"
     :quantity-units "unit",
     :nutrients {"Energy (kcal)" 30, "Carbs (g)" 0}}}
   "bell pepper"
   {"cup"
    {:name "Bell Pepper",
     :quantity 1,
     :category "Vegetable"
     :quantity-units "cup",
     :nutrients {"Energy (kcal)" 10, "Carbs (g)" 0}}}})

(node-only
  #(gmail/send-self-mail "Daily Report"
     (build-report-email
       (add-db-data-to-meals
         (map row->meal
              '({:Timestamp t1, :Foods "one cup cauliflower \n 1/2 cup steamed rice\n", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "TWZbOlhGSGszUkRPJUgobkBzTUQ"}
                {:Timestamp t2, :Foods "half cup rice\none cup cauliflower\none cup tomatoes\n one carrot\n1/2 cup bell pepper\n1/2 cup olives", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "UHh3ZVRpO3cjPG90OWcpKFAjbGI"}
                {:Timestamp t3, :Foods "half cup rice", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "IzFzV1lKenM6Rz5ANmsoTExdODY"}
                {:Timestamp t4, :Foods "", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "R1QmJiF1Om8zMFsyZE4hQnRKUj4"}))
         test-food-db))))
