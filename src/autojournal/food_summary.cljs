(ns autojournal.food-summary
  (:require [autojournal.sheets :as sheets]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [assert=]]
            [inflections.core :refer [singular]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :as st
             :refer [split split-lines lower-case join
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
        [:category :string]
        [:nutrients [:map-of NutrientName :double]]])

(def Meal
  [:map [:timestamp :any]  ; js datetime
        [:foods [:sequential Food]]
        [:oil [:enum "None" "Light" "Medium" "Heavy"]]
        [:picture :string]]) 


; ------------------------- Food Parsing ---------------------------------


(defn numberify
  {:malli/schema [:=> [:cat :string] :string]}
  [s]
  (-> s
      (st/replace #"one half " "0.5 ")
      (st/replace #"1/3 " "0.333 ")
      (st/replace #"1/2 " "0.5 ")
      (st/replace #"1/4 " "0.25 ")
      (st/replace #"3/4 " "0.75 ")
      (st/replace #"one " "1 ")
      (st/replace #"half " "0.5 ")
      (st/replace #"two " "2 ")
      (st/replace #"three " "3 ")
      (st/replace #"four " "4 ")
      (st/replace #"five " "5 ")
      (st/replace #"six " "6 ")
      (st/replace #"seven " "7 ")
      (st/replace #"eight " "8 ")
      (st/replace #"nine " "9 ")
      (st/replace #"ten " "10 ")))

(def -decimal-regex
  #"([0-9]+\.?[0-9]*|\.[0-9]+)\s+(.+)")

(defn -food-regex
  [raw-unit]
  (re-pattern (str "(.*)\\s*(" raw-unit ")\\s+(.+)")))

(def units-map
  {"cup" ["cups"]
   "tbsp" ["tablespoons" "tablespoon"]
   "unit" ["sausage" "softgel" "tablet" "full recipe" "can" "each" "bottle"
           "dash" "per portion" "slice" "serving"]
   "fl oz" ["fluid ounce"]
   "small" []
   "medium" []
   "large" []})

(defn extract-unitless-quantity
  "Returns [amount \"unit\" food] for a food string without any units in it."
  [food-str]
  (let [[quantity food] (rest (re-matches -decimal-regex food-str))]
    (if (nil? quantity)
      ["1" "unit" food-str]
      [quantity "unit" food])))

(defn extract-units
  "Returns [amount units food]."
  [food-str]
  (let [matches
        (remove empty?
                (reduce concat
                        (for [[std-unit raw-units] units-map]
                          (for [raw-unit (conj raw-units std-unit)]
                            (replace {raw-unit std-unit}
                                     (rest (re-matches (-food-regex raw-unit)
                                                       food-str)))))))]
    (cond
      (< 1 (count matches)) (do (prn "Multiple matches for " food-str) nil)
      (= 0 (count matches)) (extract-unitless-quantity food-str)
      :else (first matches))))
    
(defn -singular-fixed
  [s]
  (cond
    (= s "olives") "olive"
    :else (singular s)))

(defn -remove-of
  [of-food?]
  (let [clean-of-food? (trim of-food?)
        no-of-food (last (re-matches #"of\s+(.+)" clean-of-food?))]
    (if (nil? no-of-food) of-food? no-of-food)))

(defn -sum-and-quantity
  [quantity and-food?]
  (let [no-and-food (last (re-matches #"and\s+(.+)" and-food?))]
    (if (nil? no-and-food)
      [(js/parseFloat quantity) and-food?]
      (let [[quantity2 _ food] (extract-unitless-quantity no-and-food)]
        [(+ (js/parseFloat quantity) (js/parseFloat quantity2)) food]))))

(defn parse-food
  {:malli/schema [:=> [:cat :string] Food]}
  [raw-food]
  (let [[quantity units food]
        (extract-units (numberify (lower-case (trim raw-food))))
        [summed-quantity no-and-food] (-sum-and-quantity quantity food)]
    {:name (-singular-fixed (-remove-of no-and-food))
     :quantity summed-quantity
     :quantity-units units}))
  
(assert= {:name "potato", :quantity 3, :quantity-units "small"}
         (parse-food "three small potatoes"))
(assert= {:name "craisin", :quantity 0.5, :quantity-units "cup"}
         (parse-food "half cup craisins"))
(assert= {:name "popcorn kernel", :quantity 0.333, :quantity-units "cup"}
         (parse-food "1/3 cup popcorn kernels"))
(assert= {:name "nectarine", :quantity 1, :quantity-units "unit"}
         (parse-food "nectarine"))
(assert= {:name "carrot", :quantity 1, :quantity-units "unit"}
         (parse-food "one carrot"))
(assert= {:name "carrot", :quantity 1, :quantity-units "cup"}
         (parse-food "one cup of carrots"))
(assert= {:name "carrot", :quantity 1, :quantity-units "unit"}
         (parse-food "one and one half carrot"))
     

(defn parse-foods
  {:malli/schema [:=> [:cat :string] [:sequential Food]]}
  [foods]
  (if (= foods "")
    []
    (map parse-food (split-lines foods))))


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

(defn floatify-vals
  [m]
  (into {} (for [[k v] m
                 :let [floatv (js/parseFloat v)]]
             [k (if (js/Number.isNaN floatv) v floatv)])))

(defn get-raw-rows
  [filename]
  (map (comp floatify-vals stringify-keys)
       (first (drive/get-files filename))))

(defn get-existing-aliases
  {:malli/schema [:=> [:cat] [:map-of FoodName [:sequential FoodName]]]}
  []
  (into {} (for [row (get-raw-rows food-db-sheet-name)]
             [(get row "Food Name")
              (split (get row "Aliases") #"\n")])))
    

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

(defn generate-alt-names
  {:malli/schema [:=> [:cat FoodName] [:sequential FoodName]]}
  [food-name]
  (let [no-punc (st/replace food-name #",|\." "")
        no-descrip (first (split food-name #","))]
    (distinct (map lower-case [no-punc no-descrip]))))

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

(defn parse-cronometer-db
  {:malli/schema [:=> [:cat [:map-of :string :string]]
                   [:sequential RawCronFood]]}
  [rows]
  ; For getting test data.
  ; (prn (mapv #(select-keys % ["Category" "Food Name" "Amount" "Energy (kcal)"
  ;                             "Carbs (g)"
  ;            (take 3 rows)]
  (let [aliases (get-existing-aliases)]
    ; https://groups.google.com/g/clojure/c/UdFLYjLvNRs
    (vals (apply merge-with merge-food-units
                 (for [row rows
                       :let [food-name (get row "Food Name")
                             [quantity units] (split (get row "Amount") #" " 2)]]
                   {food-name
                     (-> row
                      (dissoc "Day" "Time" "Group" "Amount")
                      (floatify-vals)
                      (assoc "Aliases"
                             (join "\n"
                                   (union (set (generate-alt-names food-name))
                                          (set (get aliases food-name))))
                             (str "Amount " (simplify-db-unit units))
                             (js/parseFloat quantity)))})))))

(assert=
  '({"Category" "Vegetables and Vegetable Products",
     "Food Name" "Potatoes, Russet, Flesh and Skin, Baked",
     "Energy (kcal)" 164.35,
     "Carbs (g)" 37.09,
     "Aliases" "potatoes russet flesh and skin baked\npotatoes",
     "Amount medium" 1}
    {"Category" "Breakfast Cereals",
     "Food Name" "Oatmeal, Regular or Quick, Dry",
     "Energy (kcal)" 100,
     "Carbs (g)" 50,
     "Aliases" "oatmeal regular or quick dry\noatmeal",
     "Amount g" 100,
     "Amount cup" 50}
    {"Category" "Beverages",
     "Food Name" "Tap Water",
     "Energy (kcal)" 0,
     "Carbs (g)" 0,
     "Aliases" "tap water",
     "Amount g" 1000})
  (parse-cronometer-db
    [{"Category" "Vegetables and Vegetable Products", "Food Name" "Potatoes, Russet, Flesh and Skin, Baked", "Amount" "1.00 potato medium (2-1/4\" to 3-1/4\" dia.)", "Energy (kcal)" "164.35", "Carbs (g)" "37.09"}
     {"Category" "Breakfast Cereals", "Food Name" "Oatmeal, Regular or Quick, Dry", "Amount" "100.00 g", "Energy (kcal)" "100.00", "Carbs (g)" "50.00"}
     {"Category" "Breakfast Cereals", "Food Name" "Oatmeal, Regular or Quick, Dry", "Amount" "100.00 cups", "Energy (kcal)" "200.00", "Carbs (g)" "100.00"}
     {"Category" "Beverages", "Food Name" "Tap Water", "Amount" "1000.00 g", "Energy (kcal)" "0.00", "Carbs (g)" "0.00"}]))

(declare get-food-db)

(defn ^:export make-new-food-db-sheet
  []
  (sheets/maps-to-sheet
    (parse-cronometer-db (get-raw-rows cronometer-export-filename))
    "Food Database")
  (prn (get-food-db)))


; --------------------- Food Database Retrival and Access --------------------

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])

(defn get-amount-fields
  [row]
  (for [[k quantity] row
        :when (starts-with? k "Amount ")
        :let [unit (st/replace k #"Amount " "")]]
    [k unit quantity]))
    
(defn raw-db-row->food
  [row unit]
  {:name (get row "Food Name")
   :category (get row "Category")
   :quantity (first (for [[_ aunit quantity] (get-amount-fields row)
                          :when (= unit aunit)]
                      quantity))
   :quantity-units unit
   :nutrients (apply dissoc row
                     (concat ["Food Name" "Aliases" "Category"]
                             (for [[k _ _] (get-amount-fields row)] k)))})

(defn parse-raw-food-db
  [raw-db]
  (apply merge
         (for [row raw-db]
           (apply merge
                 (for [alt-name (split (get row "Aliases") "\n")]
                   {alt-name
                     (apply merge
                            (for [[_ unit quantity] (get-amount-fields row)
                                  :when (double? quantity)]
                              {unit (raw-db-row->food row unit)}))})))))

(assert=
 {"potatoes russet flesh and skin baked"
  {"medium"
    {:name "Potatoes, Russet, Flesh and Skin, Baked",
     :category "Vegetables and Vegetable Products",
     :quantity 1,
     :quantity-units "medium",
     :nutrients {"Energy (kcal)" 164.35, "Carbs (g)" 37.09}}},
  "potatoes"
  {"medium"
   {:name "Potatoes, Russet, Flesh and Skin, Baked",
    :category "Vegetables and Vegetable Products",
    :quantity 1,
    :quantity-units "medium",
    :nutrients {"Energy (kcal)" 164.35, "Carbs (g)" 37.09}}},
  "oatmeal regular or quick dry"
  {"g"
   {:name "Oatmeal, Regular or Quick, Dry",
    :category "Breakfast Cereals",
    :quantity 100,
    :quantity-units "g",
    :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}},
   "cup"
   {:name "Oatmeal, Regular or Quick, Dry",
    :category "Breakfast Cereals",
    :quantity 50,
    :quantity-units "cup",
    :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}}},
  "oatmeal"
  {"g"
   {:name "Oatmeal, Regular or Quick, Dry",
    :category "Breakfast Cereals",
    :quantity 100,
    :quantity-units "g",
    :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}},
   "cup"
   {:name "Oatmeal, Regular or Quick, Dry",
    :category "Breakfast Cereals",
    :quantity 50,
    :quantity-units "cup",
    :nutrients {"Energy (kcal)" 100, "Carbs (g)" 50}}},
  "tap water"
  {"g"
   {:name "Tap Water",
    :category "Beverages",
    :quantity 1000,
    :quantity-units "g",
    :nutrients {"Energy (kcal)" 0, "Carbs (g)" 0}}}}
 (parse-raw-food-db 
   '({"Category" "Vegetables and Vegetable Products",
      "Food Name" "Potatoes, Russet, Flesh and Skin, Baked",
      "Energy (kcal)" 164.35,
      "Carbs (g)" 37.09,
      "Aliases" "potatoes russet flesh and skin baked\npotatoes",
      "Amount medium" 1}
     {"Category" "Breakfast Cereals",
      "Food Name" "Oatmeal, Regular or Quick, Dry",
      "Energy (kcal)" 100,
      "Carbs (g)" 50,
      "Aliases" "oatmeal regular or quick dry\noatmeal",
      "Amount g" 100,
      "Amount cup" 50}
     {"Category" "Beverages",
      "Food Name" "Tap Water",
      "Energy (kcal)" 0,
      "Carbs (g)" 0,
      "Aliases" "tap water",
      "Amount g" 1000})))

(defn get-food-db
  {:malli/schema [:=> [:cat] FoodDB]}
  []
  (parse-raw-food-db (get-raw-rows food-db-sheet-name)))

(defn add-db-data-to-food
  "(logged quantity / DB quantity) * DB nutrient = logged nutrient
  (5 apples / 1 DB apple) * 100mg DB thing = 500 mg thing in logged food
  "
  [logged-food db-food]
  (let [factor (/ (:quantity logged-food) (:quantity db-food))]
    (assoc
      logged-food
      :nutrients (into {} (for [[k v] (:nutrients db-food)] [k (* factor v)]))
      :category (:category db-food))))
    

(defn add-db-data-to-foods
  {:malli/schema [:=> [:cat [:sequential Food] FoodDB]
                  [:sequential Food]]}
  [foods food-db]
  (for [food foods
        :let [db-food (-> food-db
                          (get (:name food))
                          (get (:quantity-units food)))]]
    (add-db-data-to-food food db-food)))

(defn add-db-data-to-meals
  {:malli/schema [:=> [:cat [:sequential Meal] FoodDB]
                  [:sequential Meal]]}
  [meals food-db]
  (for [meal meals]
    (update meal :foods #(add-db-data-to-foods % food-db))))



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
  
(defn foods-by-nutrient
  {:malli/schema [:=> [:cat [:sequential Food] NutrientName]
                   Hiccup]}
  [foods nutrient]
  [:details
   [:summary "All foods by " nutrient]
   (make-table ["Food" nutrient]
               (reverse
                 (sort-by last (for [food foods]
                                 [(:name food)
                                  (get (:nutrients food) nutrient)]))))])
                                
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
      (food-category-calorie-breakdown all-foods)
      [:br]
      (nutrient-target-performance all-foods)
      [:br]
      (foods-by-nutrient all-foods "Energy (kcal)")
      [:br]
      (foods-by-volume all-foods)
      [:br]
      (debug-meals meals)]]))
  

; --------------- Main -----------------------------------------

(def DAYS-TO-SUMMARIZE 5)

(defn days-between
  [date1 date2]
  (let [diff_ms (abs (- (.getTime date1) (.getTime date2)))]
    (/ diff_ms 1000.0 60 60 24)))

(defn recent-meals
  {:malli/schema [:=> [:cat [:sequential Meal]] [:sequential Meal]]}
  [food-data]
  (let [today (js/Date.)]
    (filter #(< (days-between (:timestamp %) today) DAYS-TO-SUMMARIZE)
            food-data)))


; TODO think about making this weekly instead
(defn send-daily-report
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
   {"medium"
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

(gmail/send-self-mail "Daily Report"
  (build-report-email
    (add-db-data-to-meals
      (map row->meal
           '({:Timestamp t1, :Foods "one cup cauliflower \n 1/2 cup steamed rice\n", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "TWZbOlhGSGszUkRPJUgobkBzTUQ"}
             {:Timestamp t2, :Foods "half cup rice\none cup cauliflower\none cup tomatoes\n one carrot\n1/2 cup bell pepper\n1/2 cup olives", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "UHh3ZVRpO3cjPG90OWcpKFAjbGI"}
             {:Timestamp t3, :Foods "half cup rice", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "IzFzV1lKenM6Rz5ANmsoTExdODY"}
             {:Timestamp t4, :Foods "", (keyword "Oil Amount") "None", :Picture "", :Picture.http "", :__id "R1QmJiF1Om8zMFsyZE4hQnRKUj4"}))
      test-food-db)))
