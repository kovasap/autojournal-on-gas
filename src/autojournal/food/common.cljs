(ns autojournal.food.common
  (:require [inflections.core :refer [singular]]
            [clojure.string
             :as
             st
             :refer
             [includes? starts-with? trim split lower-case]]))


; TODO Make this reference a sheet name
(def nutrient-targets-sheet-id
  "1bz_n3VlnejYkCr1uaieYFir3ajvyP4IVva6tOvW1nhI")
(def food-sheet-name "Food")
(def food-db-sheet-name "Food Database")
(def cronometer-export-filename
  "cronometer.csv")

(def DAYS-TO-SUMMARIZE 1)

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
   "calories" ["cal" "calories" "calorie"]
   "small" []
   "medium" []
   "large" []})

(def units->cups
  {"cup" 1
   "tbsp" (/ 1 16)
   "tsp" (/ 1 48)
   "fl oz" (/ 1 8)})

(defn convert-units
  [quantity unit new-unit]
  (let [in-cups (if (= unit "cup")
                  quantity
                  (* quantity (get units->cups unit)))]
    (/ in-cups (get units->cups new-unit))))

(defn get-amount-fields
  {:malli/schema [:=>
                  [:cat [:map-of :string :string]]
                  [:cat :string :string :double]]}
  [row]
  (for [[k quantity] row
        :when (and (not (nil? k)) (starts-with? k "Amount "))
        :let [unit (st/replace k #"Amount " "")]]
    [k unit quantity]))

(defn simplify-db-unit
  [units]
  (if (nil? units)
    "unit"
    ((comp
      #(if ((set (get units-map "unit")) %)
         "unit" %)
      trim
      #(get {"tablespoon" "tbsp"
             "teaspoon" "tsp"
             "fluid ounce" "fl oz"
             "cal" "calories"
             "cups" "cup"}
            % %)
      #(if (includes? % ",") (first (split % #",")) %)
      #(if (includes? % "-") (first (split % #"-")) %)
      #(cond
         (includes? % "small") "small"
         (includes? % "medium") "medium"
         (includes? % "large") "large"
         :else %)
      lower-case)
     units)))


(defn singular-fixed
  [s]
  (cond
    (= s "olives") "olive"
    :else (singular s)))
