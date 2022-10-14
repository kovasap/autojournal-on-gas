(ns autojournal.food.food-db-build
  (:require [autojournal.sheets :as sheets]
            [autojournal.food.common
             :refer
             [get-amount-fields
              convert-units
              units->cups
              cronometer-export-filename
              units->cups
              simplify-db-unit
              food-db-sheet-name]]
            [autojournal.drive :refer [floatify-vals get-raw-rows]]
            [autojournal.testing-utils :refer [assert=]]
            [clojure.set :refer [union]]
            [clojure.string :as st :refer [split join starts-with?]]))

; -------------------- Food Database Construction --------------------------

; A flattened food map pulled right from a cronometer export
(def RawCronFood
  [:map-of :string :string])

(defn merge-food-units
  {:malli/schema [:=> [:cat RawCronFood RawCronFood]
                   RawCronFood]}
  [food1 food2]
  (let [new-unit (first (filter #(starts-with? % "Amount ") (keys food2)))]
    (if (contains? food1 new-unit)
      (do (assert= food1 food2)
          food1)
      ; We need to scale the units.
      ; We are using calories here to do this but could in theory use anything.
      (let [cals1 (get food1 "Energy (kcal)")
            cals2 (get food2 "Energy (kcal)")
            factor (/ cals1 cals2)]
        (assoc food1 new-unit (* factor (get food2 new-unit)))))))

(defn split-cronometer-unit
  [cron-unit]
  (let [[raw-quantity units] (split (str cron-unit) #" " 2)
        quantity (js/parseFloat raw-quantity)]
    (if (and (not (nil? units)) (starts-with? units "x "))
      (let [[_ multiplier units] (split units #" " 3)]
        [(* (js/parseFloat multiplier) quantity) units])
      [quantity units])))
                  

(assert= (split-cronometer-unit "1.00 x 0.25 tsp")
         [0.25 "tsp"])
(assert= (split-cronometer-unit "2.50 x 0.5 cup")
         [1.25 "cup"])
(assert= (split-cronometer-unit "2.50 cup, whole pieces")
         [2.5 "cup, whole pieces"])
(assert= (split-cronometer-unit "246.00 g")
         [246 "g"])

(defn add-calorie-quantities
  {:malli/schema [:=>
                  [:cat [:sequential RawCronFood]]
                  [:sequential RawCronFood]]}
  [cronometer-rows]
  (for [row cronometer-rows]
    (assoc row "Amount calories" (get row "Energy (kcal)"))))


(defn get-reference-unit
  [row]
  (first (filter #(contains? units->cups (second %)) (get-amount-fields row))))

(defn populate-units
  {:malli/schema [:=>
                  [:cat [:sequential RawCronFood]]
                  [:sequential RawCronFood]]}
  [cronometer-rows]
  (for [row  cronometer-rows
        :let [[ref-unit ref-quantity]  (get-reference-unit  row)]]
    (if (nil? ref-unit)
      row
      (reduce (fn [r unit]
                (assoc r
                  (str "Amount " unit) (convert-units
                                         ref-quantity ref-unit unit)))
        row
        (keys units->cups)))))

(defn parse-cronometer-db
  {:malli/schema [:=>
                  [:cat [:sequential [:map-of :string :string]]]
                  [:sequential RawCronFood]]}
  [rows]
  ; For getting test data.
  ; (prn (mapv #(select-keys % ["Category" "Food Name" "Amount" "Energy (kcal)"
  ;                             "Carbs (g)"
  ;            (take 10 rows)]
  ; https://groups.google.com/g/clojure/c/UdFLYjLvNRs
  (->
    (apply
      merge-with
      merge-food-units
      (for [row  rows
            :let [food-name        (get row "Food Name")
                  [quantity units] (split-cronometer-unit (get row "Amount"))]]
        {food-name (-> row
                       (dissoc "Day" "Time" "Group" "Amount")
                       (floatify-vals)
                       (assoc "Aliases" "")
                       (assoc (str "Amount " (simplify-db-unit units))
                              quantity))}))
    vals
    populate-units
    add-calorie-quantities))
        

(assert= (parse-cronometer-db
           [{"Category"      "Vegetables and Vegetable Products"
             "Food Name"     "Potatoes, Russet, Flesh and Skin, Baked"
             "Amount"        "1.00 potato medium (2-1/4\" to 3-1/4\" dia.)"
             "Energy (kcal)" "164.35"
             "Carbs (g)"     "37.09"}
            {"Category"      "Breakfast Cereals"
             "Food Name"     "Oatmeal, Regular or Quick, Dry"
             "Amount"        "100.00 g"
             "Energy (kcal)" "100.00"
             "Carbs (g)"     "50.00"}
            {"Category"      "Breakfast Cereals"
             "Food Name"     "Oatmeal, Regular or Quick, Dry"
             "Amount"        "100.00 cups"
             "Energy (kcal)" "200.00"
             "Carbs (g)"     "100.00"}
            {"Category"      "Beverages"
             "Food Name"     "Tap Water"
             "Amount"        "1000.00 g"
             "Energy (kcal)" "0.00"
             "Carbs (g)"     "0.00"}])
         '({"Category"        "Vegetables and Vegetable Products"
            "Food Name"       "Potatoes, Russet, Flesh and Skin, Baked"
            "Energy (kcal)"   164.35
            "Carbs (g)"       37.09
            "Aliases"         ""
            "Amount medium"   1
            "Amount calories" 164.35}
           {"Amount g"        100
            "Amount cup"      50
            "Amount tsp"      2400
            "Category"        "Breakfast Cereals"
            "Amount tbsp"     800
            "Amount calories" 100
            "Food Name"       "Oatmeal, Regular or Quick, Dry"
            "Aliases"         ""
            "Carbs (g)"       50
            "Energy (kcal)"   100
            "Amount fl oz"    400}
           {"Category"        "Beverages"
            "Food Name"       "Tap Water"
            "Energy (kcal)"   0
            "Carbs (g)"       0
            "Aliases"         ""
            "Amount g"        1000
            "Amount calories" 0}))

(defn merge-rows
  "Merge Aliases, overwrite with old Category."
  [existing-rows cronometer-rows]
  (if (nil? existing-rows)
    cronometer-rows
    (let [existing-by-name (group-by #(get % "Food Name") existing-rows)]
      (for [row  cronometer-rows
            :let [food-name (get row "Food Name")]]
        (if-let [existing-row (first (get existing-by-name food-name))]
          (assoc row
            "Aliases" (join "\n"
                            (union (set (split (get row "Aliases") #"\n"))
                                   (set (split (get existing-row "Aliases")
                                               #"\n"))))
            "Category" (get existing-row "Category"))
          row)))))

(defn ^:export make-new-food-db-sheet
  []
  (let [existing-rows   (get-raw-rows food-db-sheet-name)
        cronometer-rows (parse-cronometer-db (get-raw-rows
                                               cronometer-export-filename))
        merged-rows     (merge-rows existing-rows cronometer-rows)]
    (sheets/maps-to-sheet merged-rows food-db-sheet-name)
    (prn (str "Wrote new database to " food-db-sheet-name ". "
              "Make sure to delete the old one!"))))
