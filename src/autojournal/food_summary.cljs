(ns autojournal.food-summary
  (:require [autojournal.sheets :as sheets]
            [clojure.string :refer [split-lines includes? lower-case starts-with?
                                    replace]]))

(def food-sheet-id
  "1t0jgdXPyQesMadTbnIbQmYKxVJRxme9JlZkKy_gr-BI")
  

(defn todays-foods
  [food-data]
  (let [today (js/Date.)]
    (filter #(= (. today toDateString)
                (. (:Timestamp %) toDateString))
            food-data)))

(def Food
  [:map [:quantity :double]
        [:quantity-units :string]
        [:name :string]])

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
  [[#"(\w+)\s+cup|cups\s+(.+)" "cups"]])

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
     

(defn parse-foods
  {:malli/schema [:=> [:cat :string] [:sequential Food]]}
  [foods]
  (map parse-food (split-lines foods)))

(defn summarize-food
  []
  (let [food-data (sheets/sheet-to-maps food-sheet-id)]
    (prn "Today's Foods")
    (prn (todays-foods food-data))))
    
