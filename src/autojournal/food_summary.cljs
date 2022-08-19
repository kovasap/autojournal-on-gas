(ns autojournal.food-summary
  (:require [autojournal.sheets :as sheets]
            [autojournal.get-files :as drive]
            [autojournal.testing-utils :refer [assert=]]
            [clojure.string :refer [split split-lines lower-case replace join]]))

(def food-sheet-id
  "1t0jgdXPyQesMadTbnIbQmYKxVJRxme9JlZkKy_gr-BI")
(def cronometer-export-filename
  "cronometer.csv")
  

(defn todays-foods
  [food-data]
  (let [today (js/Date.)]
    (filter #(= (. today toDateString)
                (. (:Timestamp %) toDateString))
            food-data)))

(def FoodName :string)
(def Food
  [:map [:quantity :double]
        [:quantity-units :string]
        [:name FoodName]
        [:nutrients [:map-of :string :string]]])

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

(defn generate-alt-names
  {:malli/schema [:=> [:cat FoodName] [:sequential FoodName]]}
  [food-name]
  (let [no-punc (replace food-name #",|\." "")
        no-descrip (first (split food-name #","))]
    [no-punc no-descrip]))

(defn parse-cronometer-db
  {:malli/schema [:=> [:cat] [:map-of FoodName Food]]}
  []
  (into {} (for [row (drive/get-files cronometer-export-filename)
                 :let [food-name (lower-case (get row "Food Name"))
                       [quantity units] (split (get row "Amount") #" " 2)]]
             [food-name
              (-> row
                  (assoc :name food-name
                         :quantity (js/parseFloat quantity)
                         :quantity-units units)
                  (dissoc "Day" "Time" "Group" "Food Name"))])))


(defn get-food-db
  []
  (let [db (parse-cronometer-db)]
    (reduce
      merge
      (for [[food-name food] db]
        (into {} (for [alt-name (generate-alt-names food-name)]
                   [alt-name food]))))))

(defn summarize-food
  []
  (let [food-data (sheets/sheet-to-maps food-sheet-id)
        foods (todays-foods food-data)]
    (prn "Today's Foods")
    (prn foods)))
    
