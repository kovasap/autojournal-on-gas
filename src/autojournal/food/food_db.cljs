(ns autojournal.food.food-db
  (:require [autojournal.sheets :as sheets]
            [autojournal.food.common :refer [NutrientName FoodName units-map singular-fixed Food cronometer-export-filename Meal
                                             food-db-sheet-name]]
            [clj-fuzzy.metrics :refer [levenshtein]]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [node-only assert=]]
            [inflections.core :refer [singular]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :as st
             :refer [split split-lines lower-case join
                     starts-with? includes? trim]]))


; -------------------- Food Database Construction --------------------------

(defn stringify-keys
  [m]
  (into {} (for [[k v] m]
             [(name k) v])))

(defn floatify-vals
  [m]
  (into {} (for [[k v] m
                 :let [floatv (js/parseFloat v)]]
             [k (if (or (float? v)
                        (js/Number.isNaN floatv)
                        ; parseFloat will take any leading numbers and make
                        ; them a float, so we explicitly make sure no letters
                        ; are in the string
                        (not (nil? (re-matches #".*[a-zA-Z]+.*" v))))
                    v
                    floatv)])))

(assert=
  {"a" "1.0 g"
   "b" 0
   "c" 1}
  (floatify-vals {"a" "1.0 g"
                  "b" "0.00"
                  "c" 1.00}))

(defn get-raw-rows
  [filename]
  (map (comp floatify-vals stringify-keys)
       (first (drive/get-files filename))))

(defn get-existing-aliases
  {:malli/schema [:=> [:cat] [:map-of FoodName [:sequential FoodName]]]}
  [rows]
  (into {} (for [row rows]
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

(def preparations
  #{"cooked" "raw" "chopped" "dry" "unsweetened"})


; TODO delete - this isn't useful now that we have searching
(defn generate-alt-names
  {:malli/schema [:=> [:cat FoodName] [:sequential FoodName]]}
  [food-name]
  (let [lower-food (lower-case food-name)
        no-punc (st/replace lower-food #",|\." "")
        no-descrip (first (split lower-food #","))
        no-descrip-singular (singular-fixed no-descrip)
        w-preps (reduce concat (for [prep preparations
                                     :when (includes? lower-food prep)]
                                 [(str prep " " no-descrip-singular)
                                  (str no-descrip-singular " " prep)]))]
    (distinct (concat [no-punc no-descrip no-descrip-singular] w-preps))))

(assert=
  '("kale cooked from fresh" "kale" "cooked kale" "kale cooked")
  (generate-alt-names "Kale, Cooked from Fresh"))


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
  (vals
    (apply merge-with
      merge-food-units
      (for [row  rows
            :let [food-name        (get row "Food Name")
                  [quantity units] (split-cronometer-unit (get row "Amount"))]]
        {food-name (-> row
                       (dissoc "Day" "Time" "Group" "Amount")
                       (floatify-vals)
                       (assoc (str "Amount " (simplify-db-unit units))
                              quantity))}))))

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
         '({"Category" "Vegetables and Vegetable Products"
            "Food Name" "Potatoes, Russet, Flesh and Skin, Baked"
            "Energy (kcal)" 164.35
            "Carbs (g)" 37.09
            "Amount medium" 1}
           {"Category" "Breakfast Cereals"
            "Food Name" "Oatmeal, Regular or Quick, Dry"
            "Energy (kcal)" 100
            "Carbs (g)" 50
            "Amount g" 100
            "Amount cup" 50}
           {"Category"      "Beverages"
            "Food Name"     "Tap Water"
            "Energy (kcal)" 0
            "Carbs (g)"     0
            "Amount g"      1000}))

(declare get-food-db)

(defn merge-rows
  "Merge Aliases, overwrite with old Category."
  [existing-rows cronometer-rows]
  (if (nil? existing-rows)
    cronometer-rows
    (let [existing-by-name (group-by #(get % "Food Name") existing-rows)]
      (for [row  cronometer-rows
            :let [food-name (get row "Food Name")]]
        (if-let [existing-row (get existing-by-name food-name)]
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


; --------------------- Food Database Retrival and Access --------------------

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])

(defn get-amount-fields
  [row]
  (for [[k quantity] row
        :when (starts-with? k "Amount ")
        :let [unit (st/replace k #"Amount " "")]]
    [unit quantity]))

(defn raw-db-row->foods
  [row]
  (for [food-name (conj (split (get row "Aliases") #"\n") (get row "Food Name"))
        [unit quantity] #p (get-amount-fields row)
        :when (not (= "" food-name))]
    {:name food-name
     :category (get row "Category")
     :quantity quantity
     :quantity-units unit
     :nutrients (apply dissoc row
                       (concat ["Food Name" "Aliases" "Category"]
                               (for [[k _ _] (get-amount-fields row)] k)))}))

(defn parse-db-foods
  [raw-rows]
  (reduce concat (for [row raw-rows]
                   (raw-db-row->foods row))))


(defn index-db-foods
  [db-foods]
  (into {} (for [[k g] (group-by :name db-foods)]
             [k (group-by :quantity-units g)])))


#_(defn parse-raw-food-db
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

(def test-food-db
  (index-db-foods
    (parse-db-foods
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
         "Amount g" 1000}
        {"Category" "Beverages",
         "Food Name" "Sprite",
         "Energy (kcal)" 0,
         "Carbs (g)" 0,
         "Aliases" "",
         "Amount g" 100}))))

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
 test-food-db)


(defn word-levenshtein
  "Sums the min distance of every word in the query against every db word."
  [query-phrase db-phrase]
  (reduce + (for [query-word (split query-phrase #"\s+")]
              (apply min (for [db-word (split db-phrase #"\s+")]
                           (levenshtein query-word db-word))))))

(defn clean-phrase
  "Remove punctuation and make lower case."
  [phrase]
  (-> phrase
      (lower-case)
      (st/replace #",|\." "")))


(defn most-closely-matching-food
  "The food in the db that has a name or aliases closest to the input-name."
  [input-name food-db]
  (prn (keys food-db))
  (prn input-name)
  (let [clean-input-name (clean-phrase input-name)
        distances        (for [db-food-name (keys food-db)]
                           [db-food-name
                            (word-levenshtein clean-input-name
                                              (clean-phrase db-food-name))])]
    (prn distances)
    (prn (first (apply min-key last distances)))
    (first (apply min-key last distances))))


(assert= (most-closely-matching-food "potato baked" test-food-db)
         "Potatoes, Russet, Flesh and Skin, Baked")


(defn get-food-db
  {:malli/schema [:=> [:cat] FoodDB]}
  []
  (index-db-foods (parse-db-foods (get-raw-rows food-db-sheet-name))))

(defn get-scaled-db-food
  "(logged quantity / DB quantity) * DB nutrient = logged nutrient
  (5 apples / 1 DB apple) * 100mg DB thing = 500 mg thing in logged food
  "
  [logged-food db-food]
  (let [factor (/ (:quantity logged-food) (:quantity db-food))]
    (assoc
      logged-food
      :db-name   (:name db-food)
      :category  (:category db-food)
      :nutrients (into {} (for [[k v] (:nutrients db-food)] [k (* factor v)])))))


(defn add-db-data-to-foods
  {:malli/schema [:=> [:cat [:sequential Food] FoodDB] [:sequential Food]]}
  [foods food-db]
  (for [food foods
        :let [db-food (-> food-db
                          (get #p (most-closely-matching-food (:name food)
                                                              food-db))
                          (get #p (simplify-db-unit (:quantity-units food))))]]
    (if (nil? db-food) food (get-scaled-db-food food db-food))))

(add-db-data-to-foods
  [{:name "potato" :quantity-units "medium" :quantity 0.5}]
  test-food-db)

(defn add-db-data-to-meals
  {:malli/schema [:=> [:cat [:sequential Meal] FoodDB]
                  [:sequential Meal]]}
  [meals food-db]
  (for [meal meals]
    (update meal :foods #(add-db-data-to-foods % food-db))))
