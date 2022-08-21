(ns autojournal.food-summary
  (:require [autojournal.sheets :as sheets]
            [autojournal.gmail :as gmail]
            [autojournal.drive :as drive]
            [autojournal.testing-utils :refer [assert=]]
            [clojure.string :refer [split split-lines lower-case replace join]]))

(def food-sheet-id
  "1t0jgdXPyQesMadTbnIbQmYKxVJRxme9JlZkKy_gr-BI")
(def cronometer-export-filename
  "cronometer.csv")
  

(def FoodName :string)
(def Food
  [:map [:quantity :double]
        [:quantity-units :string]
        [:name FoodName]
        [:nutrients [:map-of :string :double]]])

(def Meal
  [:map [:timestamp :any]  ; js datetime
        [:foods [:sequential Food]]
        [:oil [:enum "None" "Light" "Medium" "Heavy"]]
        [:picture :string]]) 

(defn todays-meals
  {:malli/schema [:=> [:cat [:sequential Meal]] [:sequential Meal]]}
  [food-data]
  (let [today (js/Date.)]
    (filter #(= (. today toDateString)
                (. (:Timestamp %) toDateString))
            food-data)))


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

(doseq [[input expected]
        [["three small potatoes" []]
         ["half cup craisins" []]
         ["1/3 cup popcorn kernels" []]
         ["nectarine" []]]]
  (assert= expected (parse-food input)))
     

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
  




(defn generate-alt-names
  {:malli/schema [:=> [:cat FoodName] [:sequential FoodName]]}
  [food-name]
  (let [no-punc (replace food-name #",|\." "")
        no-descrip (first (split food-name #","))]
    [no-punc no-descrip]))

(def QuantityUnits :string)
(def FoodDB [:map-of FoodName [:map-of QuantityUnits Food]])
  

(defn parse-cronometer-db
  {:malli/schema [:=> [:cat] FoodDB]}
  []
  ; https://groups.google.com/g/clojure/c/UdFLYjLvNRs
  (reduce #(merge-with merge %1 %2)
          (for [row (drive/get-files cronometer-export-filename)
                :let [food-name (lower-case (get row "Food Name"))
                      [quantity units] (split (get row "Amount") #" " 2)]]
            [food-name
             {units (-> row
                        (assoc :name food-name
                               :quantity (js/parseFloat quantity)
                               :quantity-units units)
                        (dissoc "Day" "Time" "Group" "Food Name"))}])))


(defn add-alt-names-to-food-db
  {:malli/schema [:=> [:cat FoodDB] FoodDB]}
  [db]
  (reduce
    merge
    (for [[food-name food] db]
      (into {} (for [alt-name (generate-alt-names food-name)]
                 [alt-name food])))))


(defn get-food-db
  {:malli/schema [:=> [:cat] FoodDB]}
  []
  (add-alt-names-to-food-db (parse-cronometer-db)))


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
  


(defn build-report-email
  {:malli/schema [:=> [:cat [:sequential Meal]]
                  :string]}
  [meals])
  
  


(defn send-daily-report
  []
  (let [food-db (get-food-db)
        all-meals (map row->meal (sheets/sheet-to-maps food-sheet-id))
        email-body (-> (todays-meals all-meals)
                       (add-all-nutrients-to-meals food-db)
                       (build-report-email))]
    (gmail/send-self-mail "Daily Report" email-body)))
