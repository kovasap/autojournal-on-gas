(ns autojournal.food.report-email
  (:require [autojournal.sheets :as sheets]
            [autojournal.html-utils :refer [Hiccup make-table]]
            [autojournal.food.common :refer [NutrientName Food Meal DAYS-TO-SUMMARIZE
                                             nutrient-targets-sheet-id]]
            [autojournal.testing-utils :refer [node-only assert=]]
            [cljs.pprint :refer [pprint]]))


(defn round [n]
  (/ (Math/round (* 1000 (+ n (. js/Number -EPSILON)))) 1000))


(defn get-nutrient-targets
  "Returns a map like {\"Vitamin C (mg)\"  1}"
  {:malli/schema [:=> [:cat]
                  [:map-of [NutrientName :double]]]}
  []
  (first (sheets/transposed-sheet-to-maps nutrient-targets-sheet-id)))


(defn sum-food-nutrients
  {:malli/schema [:=> [:cat [:sequential Food]]
                   [:map-of NutrientName :double]]}
  [foods]
  (reduce #(merge-with + %1 %2)
          (map :nutrients foods)))


(def PercentOfTarget :double)

(defn get-off-target-nutrients
  {:malli/schema [:=> [:cat [:sequential Food]]
                   [:vector-of [NutrientName :double :double PercentOfTarget]]]}
  [foods]
  (let [nutrient-targets (get-nutrient-targets)]
    (sort-by
      last
      (for [[nutrient amount] (sum-food-nutrients foods)
            :let [target (get nutrient-targets nutrient 100)
                  percent-of-target (/ amount target)]
            :when (or (> percent-of-target 2) (< percent-of-target 1))]
        [nutrient amount target (* 100 (round percent-of-target))]))))

(defn nutrient-target-performance
  {:malli/schema [:=> [:cat [:sequential Food]]
                   Hiccup]}
  [foods]
  [:div
   [:p "Eat more or less of these nutrients tomorrow:"]
   (make-table ["Nutrient" "Amount" "Target" "Percent of Target"]
               (get-off-target-nutrients foods))])

(defn sum-nutrient
  {:malli/schema [:=> [:cat [:sequential Food] NutrientName]
                  :double]}
  [foods nutrient-name]
  (reduce + (map #(get (:nutrients %) nutrient-name) foods)))


(defn food-category-calorie-breakdown
  {:malli/schema [:=> [:cat [:sequential Food]]
                   Hiccup]}
  [foods]
  [:div
   [:p "Food category breakdown"]
   (let [total-cals (sum-nutrient foods "Energy (kcal)")
         categories-to-cals
         (reverse
           (sort-by last (for [[cat cat-foods] (group-by :category foods)]
                           [cat (sum-nutrient cat-foods "Energy (kcal)")])))]
     (make-table ["Category" "Percent Daily Calories"]
                 (for [[cat cat-cals] categories-to-cals]
                   [cat (* 100 (round (/ cat-cals total-cals)))])))])

(defn combine-duplicate-foods
  {:malli/schema [:=> [:cat [:sequential Food]]
                  [:sequential Food]]}
  [foods]
  (for [same-foods (vals (group-by :name foods))]
    (assoc (first same-foods) 
           :nutrients (sum-food-nutrients same-foods))))
  
  
(defn foods-by-nutrient
  {:malli/schema [:=> [:cat [:sequential Food] NutrientName]
                   Hiccup]}
  [foods nutrient]
  ; [:details
  ;  [:summary "All foods by " nutrient]
  [:p "All foods by " nutrient]
  (make-table ["Food" "DB name" "Quantity" "Units" nutrient]
              (reverse
                (sort-by last (for [food (combine-duplicate-foods foods)]
                                [(:name food)
                                 (:db-name food)
                                 (:quantity food)
                                 (:quantity-units food)
                                 (get (:nutrients food) nutrient)])))))
                                
; TODO normalize all volumes to cups, then tabulate
(defn foods-by-volume
  {:malli/schema [:=> [:cat [:sequential Food]]
                   Hiccup]}
  [foods])


(defn debug-meals
  {:malli/schema [:=> [:cat [:sequential Meal]]
                  Hiccup]}
  [meals]
  [:details
   [:summary "Debug Meals"]
   [:div {:style "white-space: pre-wrap"}
         (with-out-str (pprint meals))]])

   
(defn build-report-email
  {:malli/schema [:=> [:cat [:sequential Meal :int]]
                  Hiccup]}
  [meals days-to-summarize]
  (let [all-foods (reduce concat (map :foods meals))]
    [:div
      [:h1 "Last " days-to-summarize " day food summary"]
      (food-category-calorie-breakdown all-foods)
      [:br]
      (foods-by-nutrient all-foods "Energy (kcal)")
      [:br]
      (foods-by-volume all-foods)
      [:br]
      (nutrient-target-performance all-foods)
      [:br]
      (debug-meals meals)]))
