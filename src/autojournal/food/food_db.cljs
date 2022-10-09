(ns autojournal.food.food-db
  (:require [autojournal.sheets :as sheets]
            [autojournal.food.common
             :refer
             [NutrientName
              FoodName
              units-map
              singular-fixed
              Food
              cronometer-export-filename
              Meal
              units->cups
              food-db-sheet-name]]
            [clj-fuzzy.metrics :refer [levenshtein]]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [node-only assert=]]
            [inflections.core :refer [singular]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union difference]]
            [clojure.string
             :as
             st
             :refer
             [split split-lines lower-case join starts-with? includes? trim]]))


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


(defn get-amount-fields
  {:malli/schema [:=>
                  [:cat RawCronFood]
                  [:cat :string :double]]}
  [row]
  (for [[k quantity] row
        :when (starts-with? k "Amount ")
        :let [unit (st/replace k #"Amount " "")]]
    [unit quantity]))


(defn get-reference-unit
  [row ])


(defn populate-units
  {:malli/schema [:=>
                  [:cat [:sequential RawCronFood]]
                  [:sequential RawCronFood]]}
  [cronometer-rows]
  (for [row cronometer-rows
        :let [amounts (into {} (get-amount-fields row))]]
    (reduce (fn [r unit]
              (assoc r (str "Amount " unit) (convert-units)))
            row (keys units->cups))
    (assoc row "Amount calories" (get row "Energy (kcal)"))))
  


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
  (add-calorie-quantities
    (vals
      (apply merge-with
        merge-food-units
        (for [row  rows
              :let [food-name        (get row "Food Name")
                    [quantity units] (split-cronometer-unit (get row "Amount"))]]
          {food-name (-> row
                         (dissoc "Day" "Time" "Group" "Amount")
                         (floatify-vals)
                         (assoc "Aliases" "") 
                         (assoc (str "Amount " (simplify-db-unit units))
                                quantity))})))))

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
         '({"Category" "Vegetables and Vegetable Products",
            "Food Name" "Potatoes, Russet, Flesh and Skin, Baked",
            "Energy (kcal)" 164.35,
            "Carbs (g)" 37.09,
            "Aliases" "",
            "Amount medium" 1,
            "Amount calories" 164.35}
           {"Category" "Breakfast Cereals",
            "Food Name" "Oatmeal, Regular or Quick, Dry",
            "Energy (kcal)" 100,
            "Carbs (g)" 50,
            "Aliases" "",
            "Amount g" 100,
            "Amount cup" 50,
            "Amount calories" 100}
           {"Category" "Beverages",
            "Food Name" "Tap Water",
            "Energy (kcal)" 0,
            "Carbs (g)" 0,
            "Aliases" "",
            "Amount g" 1000,
            "Amount calories" 0}))

(declare get-food-db)

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


; --------------------- Food Database Retrival and Access --------------------

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])

(defn raw-db-row->foods
  [row]
  (for [food-name (conj (split (get row "Aliases") #"\n") (get row "Food Name"))
        [unit quantity] (get-amount-fields row)
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
  (into {} (for [[n g] (group-by :name db-foods)]
             [n (into {} (for [food g]
                           [(:quantity-units food) food]))])))


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
         "Aliases" "potatoes",
         "Amount calories" 164.35
         "Amount medium" 1}
        {"Category" "Breakfast Cereals",
         "Food Name" "Oatmeal, Regular or Quick, Dry",
         "Energy (kcal)" 100,
         "Carbs (g)" 50,
         "Aliases" "oatmeal",
         "Amount calories" 100
         "Amount g" 100,
         "Amount cup" 50}
        {"Category" "Beverages",
         "Food Name" "Tap Water",
         "Energy (kcal)" 0,
         "Carbs (g)" 0,
         "Aliases" "tap water",
         "Amount calories" 0
         "Amount g" 1000}
        {"Category" "Beverages",
         "Food Name" "Sprite",
         "Energy (kcal)" 0,
         "Carbs (g)" 0,
         "Aliases" "",
         "Amount calories" 0
         "Amount g" 100}))))

(assert=
  test-food-db
  {"potatoes"
    {"calories" {:name           "potatoes"
                 :category       "Vegetables and Vegetable Products"
                 :quantity       164.35
                 :quantity-units "calories"
                 :nutrients      {"Energy (kcal)"   164.35
                                  "Carbs (g)"       37.09
                                  "Amount calories" 164.35
                                  "Amount medium"   1}}
     "medium"   {:name           "potatoes"
                 :category       "Vegetables and Vegetable Products"
                 :quantity       1
                 :quantity-units "medium"
                 :nutrients      {"Energy (kcal)"   164.35
                                  "Carbs (g)"       37.09
                                  "Amount calories" 164.35
                                  "Amount medium"   1}}}
   "Potatoes, Russet, Flesh and Skin, Baked"
    {"calories" {:name           "Potatoes, Russet, Flesh and Skin, Baked"
                 :category       "Vegetables and Vegetable Products"
                 :quantity       164.35
                 :quantity-units "calories"
                 :nutrients      {"Energy (kcal)"   164.35
                                  "Carbs (g)"       37.09
                                  "Amount calories" 164.35
                                  "Amount medium"   1}}
     "medium"   {:name           "Potatoes, Russet, Flesh and Skin, Baked"
                 :category       "Vegetables and Vegetable Products"
                 :quantity       1
                 :quantity-units "medium"
                 :nutrients      {"Energy (kcal)"   164.35
                                  "Carbs (g)"       37.09
                                  "Amount calories" 164.35
                                  "Amount medium"   1}}}
   "oatmeal"   {"calories" {:name           "oatmeal"
                            :category       "Breakfast Cereals"
                            :quantity       100
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)"   100
                                             "Carbs (g)"       50
                                             "Amount calories" 100
                                             "Amount g"        100
                                             "Amount cup"      50}}
                "g"        {:name           "oatmeal"
                            :category       "Breakfast Cereals"
                            :quantity       100
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)"   100
                                             "Carbs (g)"       50
                                             "Amount calories" 100
                                             "Amount g"        100
                                             "Amount cup"      50}}
                "cup"      {:name           "oatmeal"
                            :category       "Breakfast Cereals"
                            :quantity       50
                            :quantity-units "cup"
                            :nutrients      {"Energy (kcal)"   100
                                             "Carbs (g)"       50
                                             "Amount calories" 100
                                             "Amount g"        100
                                             "Amount cup"      50}}}
   "Oatmeal, Regular or Quick, Dry"
     {"calories" {:name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       100
                  :quantity-units "calories"
                  :nutrients      {"Energy (kcal)"   100
                                   "Carbs (g)"       50
                                   "Amount calories" 100
                                   "Amount g"        100
                                   "Amount cup"      50}}
      "g"        {:name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       100
                  :quantity-units "g"
                  :nutrients      {"Energy (kcal)"   100
                                   "Carbs (g)"       50
                                   "Amount calories" 100
                                   "Amount g"        100
                                   "Amount cup"      50}}
      "cup"      {:name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       50
                  :quantity-units "cup"
                  :nutrients      {"Energy (kcal)"   100
                                   "Carbs (g)"       50
                                   "Amount calories" 100
                                   "Amount g"        100
                                   "Amount cup"      50}}}
   "tap water" {"calories" {:name           "tap water"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        1000}}
                "g"        {:name           "tap water"
                            :category       "Beverages"
                            :quantity       1000
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        1000}}}
   "Tap Water" {"calories" {:name           "Tap Water"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        1000}}
                "g"        {:name           "Tap Water"
                            :category       "Beverages"
                            :quantity       1000
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        1000}}}
   "Sprite"    {"calories" {:name           "Sprite"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        100}}
                "g"        {:name           "Sprite"
                            :category       "Beverages"
                            :quantity       100
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)"   0
                                             "Carbs (g)"       0
                                             "Amount calories" 0
                                             "Amount g"        100}}}})


(defn clean-phrase
  "Remove punctuation and make lower case."
  [phrase]
  (-> phrase
      (lower-case)
      (st/replace #",|\." "")))


(defn word-levenshtein
  "Sums the min distance of every word in the query against every db word."
  [query-phrase db-phrase]
  (reduce + (for [query-word (split query-phrase #"\s+")]
              (apply min (for [db-word (split db-phrase #"\s+")]
                           (levenshtein query-word db-word))))))

(def PhraseMatch
  [:map [:matched-words [:sequential :string]]
        [:unmatched-words [:sequential :string]]
        ; The sum of the min distance of every matched word to the word that it
        ; matched.
        [:matched-distance [:sequential :string]]])

(defn match-phrases
  {:malli/schema [:=> [:cat :string :string] PhraseMatch]}
  [query-phrase target-phrase]
  (let [query-words   (split query-phrase #"\s+")
        target-words  (split target-phrase #"\s+")
        matched-words (for [qw query-words]
                        (merge {:query-word qw}
                               (apply min-key
                                 :distance
                                 (for [tw target-words]
                                   {:target-word tw
                                    :distance (levenshtein qw tw)}))))
        unmatched-words (difference (set target-words)
                                    (set (map :target-word matched-words)))]
    {:matched-words matched-words
     :matched-distance-sum (reduce + (map :distance matched-words))
     :unmatched-words unmatched-words}))

(defn compare-matches
  [pm1 pm2]
  (if (= (:matched-distance-sum pm1) (:matched-distance-sum pm2))
    (- (count (:unmatched-words pm1)) (count (:unmatched-words pm2)))
    (- (:matched-distance-sum pm1) (:matched-distance-sum pm2))))

(defn make-target-comparer
  [query-phrase]
  (fn [target-phrase1 target-phrase2]
    (compare-matches (match-phrases query-phrase (clean-phrase target-phrase1))
                     (match-phrases query-phrase (clean-phrase target-phrase2)))))

(defn rank-matches
  [query-phrase target-phrases]
  ; (doseq [tp target-phrases]
  ;   (prn tp (match-phrases (clean-phrase query-phrase) tp))]
  (let [target-comparer (make-target-comparer (clean-phrase query-phrase))]
    (sort target-comparer target-phrases)))

(assert=
  '("Tofu, poachet"
    "Tofu Stir Fry, with Carrots or Dark Green Vegetables"
    "potato")
  (rank-matches "tofu"
                ["Tofu, poachet"
                 "Tofu Stir Fry, with Carrots or Dark Green Vegetables"
                 "potato"]))

(defn most-closely-matching-food
  "The food name in the db that has a name or aliases closest to the input-name."
  [input-name food-db]
  (first (rank-matches input-name (keys food-db)))
  #_(let [clean-input-name (clean-phrase input-name)
          distances        (for [db-food-name (keys food-db)]
                             [db-food-name
                              (word-levenshtein clean-input-name
                                                (clean-phrase db-food-name))])]
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
                          (get (most-closely-matching-food (:name food)
                                                           food-db))
                          (get (simplify-db-unit (:quantity-units food))))]]
    (if (nil? db-food) food (get-scaled-db-food food db-food))))

(assert= '({:name           "potato"
            :quantity-units "medium"
            :quantity       0.5
            :db-name        "Potatoes, Russet, Flesh and Skin, Baked"
            :category       "Vegetables and Vegetable Products"
            :nutrients      {"Energy (kcal)" 82.175
                             "Carbs (g)"     18.545
                             "Amount medium" 0.5}})
         (add-db-data-to-foods
           [{:name "potato" :quantity-units "medium" :quantity 0.5}]
           test-food-db))

(defn add-db-data-to-meals
  {:malli/schema [:=> [:cat [:sequential Meal] FoodDB]
                  [:sequential Meal]]}
  [meals food-db]
  (for [meal meals]
    (update meal :foods #(add-db-data-to-foods % food-db))))
