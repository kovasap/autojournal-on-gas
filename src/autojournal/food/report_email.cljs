(ns autojournal.food.report-email
  (:require [autojournal.sheets :as sheets]
            [autojournal.food.common :refer [NutrientName Food Meal DAYS-TO-SUMMARIZE
                                             nutrient-targets-sheet-id]]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [node-only assert=]]
            [inflections.core :refer [singular]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :as st
             :refer [split split-lines lower-case join
                     starts-with? includes? trim]]))



(def Hiccup
  [:schema
   {:registry {"hiccup" [:orn
                         [:node [:catn
                                 [:name keyword?]
                                 [:props [:? [:map-of keyword? any?]]]
                                 [:children [:* [:schema [:ref "hiccup"]]]]]]
                         [:primitive [:orn
                                      [:nil nil?]
                                      [:boolean boolean?]
                                      [:number number?]
                                      [:text string?]]]]}}
   "hiccup"])

(defn make-table
  {:malli/schema [:=> [:cat [:sequential :string]
                            [:sequential [:sequential :any]]]
                   Hiccup]}
  [headers rows]
  [:table
   [:tbody
    ; https://www.w3schools.com/html/html_table_headers.asp
    (into [:tr] (for [h headers] [:th h]))
    (for [row rows]
      (into [:tr] (for [i row] [:td i])))]])

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
  (make-table ["Food" nutrient]
              (reverse
                (sort-by last (for [food (combine-duplicate-foods foods)]
                                [(:name food)
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
  {:malli/schema [:=> [:cat [:sequential Meal]]
                  Hiccup]}
  [meals]
  (let [all-foods (reduce concat (map :foods meals))]
    [:html
     [:head]
     [:body
      [:h1 "Last " DAYS-TO-SUMMARIZE " day food summary"]
      (food-category-calorie-breakdown all-foods)
      [:br]
      (nutrient-target-performance all-foods)
      [:br]
      (foods-by-nutrient all-foods "Energy (kcal)")
      [:br]
      (foods-by-volume all-foods)
      [:br]
      (debug-meals meals)]]))
