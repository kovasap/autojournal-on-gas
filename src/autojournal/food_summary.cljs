(ns autojournal.food-summary
  (:require [autojournal.sheets :as sheets]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [assert=]]
            [hiccups.runtime :refer [render-html]]
            [clojure.set :refer [difference]]
            [clojure.string :refer [split split-lines lower-case replace join
                                    starts-with? includes? trim]]))

(def food-sheet-name "Food")
(def food-db-sheet-name "Food Database")
(def cronometer-export-filename
  "cronometer.csv")
; TODO Make this reference a sheet name
(def nutrient-targets-sheet-id
  "1bz_n3VlnejYkCr1uaieYFir3ajvyP4IVva6tOvW1nhI")
  

(def FoodName :string)
(def NutrientName :string)
(def Food
  [:map [:quantity :double]
        [:quantity-units :string]
        [:name FoodName]
        [:nutrients [:map-of NutrientName :double]]])

(def Meal
  [:map [:timestamp :any]  ; js datetime
        [:foods [:sequential Food]]
        [:oil [:enum "None" "Light" "Medium" "Heavy"]]
        [:picture :string]]) 


; ------------------------- Food Parsing ---------------------------------


(defn numberify
  {:malli/schema [:=> [:cat :string] :double]}
  [s]
  (-> s
      (replace "one" "1")
      (replace "two" "2")
      (replace "three" "3")
      (replace "four" "4")
      (replace "five" "5")
      (replace "six" "6")
      (replace "seven" "7")
      (replace "eight" "8")
      (replace "nine" "9")
      (replace "ten" "10")
      (js/parseFloat)))

(def food-quantity-regexes
  [[#"(\w+)\s+cup|cups\s+(.+)" "cups"]
   [#"(\w+)\s+(.+)" "whole"]])

(defn parse-food-units
  [food]
  (last (for [[re units] food-quantity-regexes
              [quantity food] (re-matches re food)
              :while (nil? quantity)]
            {:name food
             :quantity (numberify quantity)
             :quantity-units units})))
    
(defn parse-food
  {:malli/schema [:=> [:cat :string] Food]}
  [food]
  (parse-food-units (lower-case food)))

; (doseq [[input expected]
;         [["three small potatoes" []]
;          ["half cup craisins" []]
;          ["1/3 cup popcorn kernels" []]
;          ["nectarine" []]])
;   (assert= expected (parse-food input)))
     

(defn parse-foods
  {:malli/schema [:=> [:cat :string] [:sequential Food]]}
  [foods]
  (map parse-food (split-lines foods)))


(defn row->meal
  [row]
  {:timestamp (:Timestamp row)
   :foods     (parse-foods (:Foods row))
   :oil       ((keyword "Oil Amount") row)
   :picture   (:Picture.http row)})
  


; -------------------- Food Database Construction --------------------------  

(defn stringify-keys
  [m]
  (into {} (for [[k v] m]
             [(name k) v])))

(defn get-cronometer-db-raw-rows
  []
  (map stringify-keys (first (drive/get-files cronometer-export-filename))))


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

(defn floatify-vals
  [m]
  (into {} (for [[k v] m
                 :let [floatv (js/parseFloat v)]]
             [k (if (js/Number.isNaN floatv) v floatv)])))

(defn generate-alt-names
  {:malli/schema [:=> [:cat FoodName] [:sequential FoodName]]}
  [food-name]
  (let [no-punc (replace food-name #",|\." "")
        no-descrip (first (split food-name #","))]
    (distinct (map lower-case [no-punc no-descrip]))))

(defn simplify-unit
  [unit]
  ((comp
    trim
    #(get {"tablespoon" "tbsp"
           "cups" "cup"}
          % %)
    #(if (includes? % ",") (first (split % #",")) %)
    #(if (includes? % "-") (first (split % #"-")) %)
    lower-case)
   unit))

(defn parse-cronometer-db
  {:malli/schema [:=> [:cat [:map-of :string :string]
                       [:sequential RawCronFood]]]}
  [rows]
  ; For getting test data.
  ; (prn (mapv #(select-keys % ["Category" "Food Name" "Amount" "Energy (kcal)"
  ;                             "Carbs (g)"
  ;            (take 3 rows)]
  ; https://groups.google.com/g/clojure/c/UdFLYjLvNRs
  (vals (apply merge-with merge-food-units
               (for [row rows
                     :let [food-name (get row "Food Name")
                           [quantity units] (split (get row "Amount") #" " 2)]]
                 {food-name
                   (-> row
                    (dissoc "Day" "Time" "Group" "Amount")
                    (floatify-vals)
                    (assoc "Aliases" (join "\n" (generate-alt-names food-name))
                           (str "Amount " (simplify-unit units))
                           (js/parseFloat quantity)))}))))

(assert=
  '({"Category" "Vegetables and Vegetable Products",
     "Energy (kcal)" 164.35,
     "Carbs (g)" 37.09,
     "Aliases" "potatoes russet flesh and skin baked\npotatoes",
     "Food Name" "Potatoes, Russet, Flesh and Skin, Baked"
     "Amount potato medium (2-1/4\" to 3-1/4\" dia.)" 1}
    {"Category" "Breakfast Cereals",
     "Energy (kcal)" 100,
     "Carbs (g)" 50,
     "Aliases" "oatmeal regular or quick dry\noatmeal",
     "Food Name" "Oatmeal, Regular or Quick, Dry"
     "Amount g" 100,
     "Amount cups" 50}
    {"Category" "Beverages",
     "Energy (kcal)" 0,
     "Carbs (g)" 0,
     "Aliases" "tap water",
     "Food Name" "Tap Water"
     "Amount g" 1000})
  (parse-cronometer-db
    [{"Category" "Vegetables and Vegetable Products", "Food Name" "Potatoes, Russet, Flesh and Skin, Baked", "Amount" "1.00 potato medium (2-1/4\" to 3-1/4\" dia.)", "Energy (kcal)" "164.35", "Carbs (g)" "37.09"}
     {"Category" "Breakfast Cereals", "Food Name" "Oatmeal, Regular or Quick, Dry", "Amount" "100.00 g", "Energy (kcal)" "100.00", "Carbs (g)" "50.00"}
     {"Category" "Breakfast Cereals", "Food Name" "Oatmeal, Regular or Quick, Dry", "Amount" "100.00 cups", "Energy (kcal)" "200.00", "Carbs (g)" "100.00"}
     {"Category" "Beverages", "Food Name" "Tap Water", "Amount" "1000.00 g", "Energy (kcal)" "0.00", "Carbs (g)" "0.00"}]))

(defn print+return [x] (prn x) x)

(defn ^:export make-new-food-db-sheet
  []
  (sheets/maps-to-sheet
    (print+return (parse-cronometer-db (get-cronometer-db-raw-rows)))
    "Food Database"))

; --------------------- Food Database Retrival and Access --------------------

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])


(defn get-food-db
  {:malli/schema [:=> [:cat] FoodDB]}
  []
  (let [raw-db (drive/get-files food-db-sheet-name)]))



; (defn add-alt-names-to-food-db
;   {:malli/schema [:=> [:cat FoodDB] FoodDB]}
;   [db]
;   (reduce
;     merge
;     (for [[food-name food] db]
;       (into {} (for [alt-name (generate-alt-names food-name)]
;                  [alt-name food])))))


(defn add-nutrients
  "(logged quantity / DB quantity) * DB nutrient = logged nutrient
  (5 apples / 1 DB apple) * 100mg DB thing = 500 mg thing in logged food
  "
  [logged-food db-food]
  (let [factor (/ (:quantity logged-food) (:quantity db-food))]
    (assoc logged-food :nutrients
           (into {} (for [[k v] (:nutrients db-food)] [k (* factor v)])))))
    

(defn add-all-nutrients
  {:malli/schema [:=> [:cat [:sequential Food] FoodDB]
                  [:sequential Food]]}
  [foods food-db]
  (for [food foods
        :let [db-food (-> food-db
                          (get (:name food))
                          (get (:quantity-units food)))]]
    (add-nutrients food db-food)))

(defn add-all-nutrients-to-meals
  {:malli/schema [:=> [:cat [:sequential Meal] FoodDB]
                  [:sequential Meal]]}
  [meals food-db]
  (for [meal meals]
    (update meal :foods #(add-all-nutrients % food-db))))



; --------------- Report Email Construction -----------------------------

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


(defn get-nutrient-targets
  "Returns a map like {\"Vitamin C (mg)\"  1}"
  {:malli/schema [:=> [:cat]
                  [:map-of [NutrientName :double]]]}
  []
  (sheets/transposed-sheet-to-maps nutrient-targets-sheet-id))



(defn sum-food-nutrients
  {:malli/schema [:=> [:cat [:sequential Food]]
                   [:map-of NutrientName :double]]}
  [foods]
  (reduce #(merge-with + %1 %2)
          (map :nutrients foods)))


(def PercentOfTarget :double)

(defn get-off-target-nutrients
  {:malli/schema [:=> [:cat [:sequential Food]]
                   [:vector-of [NutrientName PercentOfTarget]]]}
  [foods]
  (let [nutrient-targets (get-nutrient-targets)]
    (sort-by
      #(%2)
      (for [[nutrient amount] (sum-food-nutrients foods)
            :let [percent-of-target (/ amount (get nutrient-targets nutrient))]
            :when (or (> percent-of-target 2) (< percent-of-target 1))]
        [nutrient percent-of-target]))))


(defn nutrient-section
  {:malli/schema [:=> [:cat [:sequential Food]]
                  Hiccup]}
  [foods]
  [:div
   [:p "Eat more or less of these nutrients tomorrow:"
    [:table
      [:tbody
    ;  https://www.w3schools.com/html/html_table_headers.asp
       [:tr [:th "Nutrient"] [:th "Percent of Target"]]
       (for [[nutrient percent-of-target] (get-off-target-nutrients foods)]
         [:tr [:td nutrient] [:td percent-of-target]])]]]])
   
  
   

(defn build-report-email
  {:malli/schema [:=> [:cat [:sequential Meal]]
                  :string]}
  [meals]
  (let [all-foods (reduce concat (map :foods meals))]
    (render-html 
      [:div
       (nutrient-section all-foods)])))
  

; --------------- Main -----------------------------------------

(defn todays-meals
  {:malli/schema [:=> [:cat [:sequential Meal]] [:sequential Meal]]}
  [food-data]
  (let [today (js/Date.)]
    (filter #(= (. today toDateString)
                (. (:Timestamp %) toDateString))
            food-data)))


; TODO think about making this weekly instead
(defn send-daily-report
  []
  (let [food-db (get-food-db)
        all-meals (map row->meal (drive/get-files food-sheet-name))
        email-body (-> (todays-meals all-meals)
                       (add-all-nutrients-to-meals food-db)
                       (build-report-email))]
    (gmail/send-self-mail "Daily Report" email-body)))
