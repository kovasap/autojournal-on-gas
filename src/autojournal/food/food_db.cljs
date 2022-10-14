(ns autojournal.food.food-db
  (:require [autojournal.food.common
             :refer
             [FoodName get-amount-fields Food Meal food-db-sheet-name
              simplify-db-unit]]
            [clj-fuzzy.metrics :refer [levenshtein]]
            [autojournal.testing-utils :refer [assert=]]
            [autojournal.drive :refer [get-raw-rows]]
            [clojure.set :refer [difference]]
            [clojure.string :as st :refer [split lower-case]]))


; --------------------- Food Database Retrival and Access --------------------

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])

(defn raw-db-row->foods
  [row]
  (for [food-name (conj (split (get row "Aliases") #"\n") (get row "Food Name"))
        [_ unit quantity] (get-amount-fields row)
        :let [db-name (get row "Food Name")]
        :when (not (= "" food-name))]
    {:alias food-name
     :name db-name
     :category (get row "Category")
     :quantity (if (= "" quantity) 0 quantity)
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
  (into {} (for [[n g] (group-by :alias db-foods)]
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
         "Amount cup" ""
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
  {"potatoes"  {"cup"      {:alias "potatoes"
                            :name "Potatoes, Russet, Flesh and Skin, Baked"
                            :category "Vegetables and Vegetable Products"
                            :quantity 0
                            :quantity-units "cup"
                            :nutrients {"Energy (kcal)" 164.35
                                        "Carbs (g)"     37.09}}
                "calories" {:alias "potatoes"
                            :name "Potatoes, Russet, Flesh and Skin, Baked"
                            :category "Vegetables and Vegetable Products"
                            :quantity 164.35
                            :quantity-units "calories"
                            :nutrients {"Energy (kcal)" 164.35
                                        "Carbs (g)"     37.09}}
                "medium"   {:alias "potatoes"
                            :name "Potatoes, Russet, Flesh and Skin, Baked"
                            :category "Vegetables and Vegetable Products"
                            :quantity 1
                            :quantity-units "medium"
                            :nutrients {"Energy (kcal)" 164.35
                                        "Carbs (g)"     37.09}}}
   "Potatoes, Russet, Flesh and Skin, Baked"
     {"cup"      {:alias          "Potatoes, Russet, Flesh and Skin, Baked"
                  :name           "Potatoes, Russet, Flesh and Skin, Baked"
                  :category       "Vegetables and Vegetable Products"
                  :quantity       0
                  :quantity-units "cup"
                  :nutrients      {"Energy (kcal)" 164.35 "Carbs (g)" 37.09}}
      "calories" {:alias          "Potatoes, Russet, Flesh and Skin, Baked"
                  :name           "Potatoes, Russet, Flesh and Skin, Baked"
                  :category       "Vegetables and Vegetable Products"
                  :quantity       164.35
                  :quantity-units "calories"
                  :nutrients      {"Energy (kcal)" 164.35 "Carbs (g)" 37.09}}
      "medium"   {:alias          "Potatoes, Russet, Flesh and Skin, Baked"
                  :name           "Potatoes, Russet, Flesh and Skin, Baked"
                  :category       "Vegetables and Vegetable Products"
                  :quantity       1
                  :quantity-units "medium"
                  :nutrients      {"Energy (kcal)" 164.35 "Carbs (g)" 37.09}}}
   "oatmeal"   {"calories" {:alias          "oatmeal"
                            :name           "Oatmeal, Regular or Quick, Dry"
                            :category       "Breakfast Cereals"
                            :quantity       100
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)" 100
                                             "Carbs (g)"     50}}
                "g"        {:alias          "oatmeal"
                            :name           "Oatmeal, Regular or Quick, Dry"
                            :category       "Breakfast Cereals"
                            :quantity       100
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)" 100
                                             "Carbs (g)"     50}}
                "cup"      {:alias          "oatmeal"
                            :name           "Oatmeal, Regular or Quick, Dry"
                            :category       "Breakfast Cereals"
                            :quantity       50
                            :quantity-units "cup"
                            :nutrients      {"Energy (kcal)" 100
                                             "Carbs (g)"     50}}}
   "Oatmeal, Regular or Quick, Dry"
     {"calories" {:alias          "Oatmeal, Regular or Quick, Dry"
                  :name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       100
                  :quantity-units "calories"
                  :nutrients      {"Energy (kcal)" 100 "Carbs (g)" 50}}
      "g"        {:alias          "Oatmeal, Regular or Quick, Dry"
                  :name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       100
                  :quantity-units "g"
                  :nutrients      {"Energy (kcal)" 100 "Carbs (g)" 50}}
      "cup"      {:alias          "Oatmeal, Regular or Quick, Dry"
                  :name           "Oatmeal, Regular or Quick, Dry"
                  :category       "Breakfast Cereals"
                  :quantity       50
                  :quantity-units "cup"
                  :nutrients      {"Energy (kcal)" 100 "Carbs (g)" 50}}}
   "tap water" {"calories" {:alias          "tap water"
                            :name           "Tap Water"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}
                "g"        {:alias          "tap water"
                            :name           "Tap Water"
                            :category       "Beverages"
                            :quantity       1000
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}}
   "Tap Water" {"calories" {:alias          "Tap Water"
                            :name           "Tap Water"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}
                "g"        {:alias          "Tap Water"
                            :name           "Tap Water"
                            :category       "Beverages"
                            :quantity       1000
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}}
   "Sprite"    {"calories" {:alias          "Sprite"
                            :name           "Sprite"
                            :category       "Beverages"
                            :quantity       0
                            :quantity-units "calories"
                            :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}
                "g"        {:alias          "Sprite"
                            :name           "Sprite"
                            :category       "Beverages"
                            :quantity       100
                            :quantity-units "g"
                            :nutrients      {"Energy (kcal)" 0
                                             "Carbs (g)"     0}}}})


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
  (let [factor (if (= 0 (:quantity db-food))
                 0
                 (/ (:quantity logged-food) (:quantity db-food)))]
    (assoc logged-food
      :db-name   (:name db-food)
      :category  (:category db-food)
      :nutrients (into {}
                       (for [[k v] (:nutrients db-food)] [k (* factor v)])))))


(defn add-db-data-to-foods
  {:malli/schema [:=> [:cat [:sequential Food] FoodDB] [:sequential Food]]}
  [foods food-db]
  (for [food foods
        :let [db-food (-> food-db
                          (get (most-closely-matching-food (:name food)
                                                           food-db))
                          (get (simplify-db-unit (:quantity-units food))))]]
    (if (nil? db-food)
      food
      (get-scaled-db-food food db-food))))

(assert= (add-db-data-to-foods
           [{:name "potato" :quantity-units "medium" :quantity 0.5}]
           test-food-db)
         '({:name           "potato"
            :quantity-units "medium"
            :quantity       0.5
            :db-name        "Potatoes, Russet, Flesh and Skin, Baked"
            :category       "Vegetables and Vegetable Products"
            :nutrients      {"Energy (kcal)" 82.175 "Carbs (g)" 18.545}}))

(assert=
  (add-db-data-to-foods [{:name "potato" :quantity-units "cups" :quantity 0.5}]
                        test-food-db)
  '({:name           "potato"
     :quantity-units "cups"
     :quantity       0.5
     :db-name        "Potatoes, Russet, Flesh and Skin, Baked"
     :category       "Vegetables and Vegetable Products"
     :nutrients      {"Energy (kcal)" 0 "Carbs (g)" 0}}))

(defn add-db-data-to-meals
  {:malli/schema [:=> [:cat [:sequential Meal] FoodDB]
                  [:sequential Meal]]}
  [meals food-db]
  (for [meal meals]
    (update meal :foods #(add-db-data-to-foods % food-db))))
