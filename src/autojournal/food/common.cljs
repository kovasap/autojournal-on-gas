(ns autojournal.food.common
  (:require [inflections.core :refer [singular]]))
             

; TODO Make this reference a sheet name
(def nutrient-targets-sheet-id
  "1bz_n3VlnejYkCr1uaieYFir3ajvyP4IVva6tOvW1nhI")
(def food-sheet-name "Food")
(def food-db-sheet-name "Food Database")
(def cronometer-export-filename
  "cronometer.csv")

(def DAYS-TO-SUMMARIZE 5)


(def FoodName :string)
(def NutrientName :string)
(def Food
  [:map [:quantity :double]
        [:quantity-units :string]
        [:name FoodName]
        [:category :string]
        [:nutrients [:map-of NutrientName :double]]])

(def Meal
  [:map [:datetime :any]  ; js datetime
        [:foods [:sequential Food]]
        [:oil [:enum "None" "Light" "Medium" "Heavy"]]
        [:picture :string]]) 


(def units-map
  {"cup" ["cups"]
   "tbsp" ["tablespoons" "tablespoon"]
   "unit" ["sausage" "softgel" "tablet" "full recipe" "can" "each" "bottle"
           "dash" "per portion" "slice" "serving"]
   "fl oz" ["fluid ounce"]
   "small" []
   "medium" []
   "large" []})


(defn -singular-fixed
  [s]
  (cond
    (= s "olives") "olive"
    :else (singular s)))


