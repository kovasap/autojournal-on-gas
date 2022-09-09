(ns autojournal.food.food-parsing
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.food.common :refer [units-map Food -singular-fixed]]
            [clojure.string :as st
             :refer [split split-lines lower-case join
                     starts-with? includes? trim]]))

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
  {:datetime (:Timestamp row)
   :foods     (parse-foods (:Foods row))
   :oil       ((keyword "Oil Amount") row)
   :picture   (:Picture.http row)})
