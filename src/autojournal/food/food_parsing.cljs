(ns autojournal.food.food-parsing
  (:require [autojournal.testing-utils :refer [assert=]]
            [autojournal.schemas :refer [Event]]
            [autojournal.food.common :refer [units-map Meal Food singular-fixed]]
            [cljs-time.core :refer [plus minutes to-default-time-zone
                                    from-default-time-zone to-utc-time-zone]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.string :as st
             :refer [split-lines lower-case trim join]]))

; ------------------------- Food Parsing ---------------------------------


(defn numberify
  {:malli/schema [:=> [:cat :string] :string]}
  [s]
  (-> s
      (st/replace #"quarter" "0.25 ")
      (st/replace #"one half " "0.5 ")
      ; (st/replace #"one and a half " "1.5 ")
      (st/replace #"1/3 " "0.333 ")
      (st/replace #"2/3 " "0.666 ")
      (st/replace #"1/2 " "0.5 ")
      (st/replace #"1/4 " "0.25 ")
      (st/replace #"3/4 " "0.75 ")
      (st/replace #"one " "1 ")
      (st/replace #"a half " "0.5 ")
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

; https://regexr.com/2vckg
(def -decimal-regex
  #"(?!.*([0-9]+\.?[0-9]*|\.[0-9]+))\s+(.+)")

(defn -food-regex
  [raw-unit]
  (re-pattern (str "(.*)\\s*(" raw-unit ")\\s+(.+)")))

(defn extract-unitless-quantity
  "Returns [amount \"unit\" food] for a food string without any units in it."
  [food-str]
  (let [food (last (re-find -decimal-regex food-str))]
    (if (nil? food)
      ["1" "unit" food-str]
      [(trim (st/replace food-str food "")) "unit" food])))

(defn extract-units
  "Returns [amount units food]."
  [food-str]
  (let [matches
        (remove empty?
                (reduce union
                        (for [[std-unit raw-units] units-map]
                          (set (for [raw-unit (conj raw-units std-unit)]
                                 (replace {raw-unit std-unit}
                                          (rest (re-matches (-food-regex raw-unit)
                                                            food-str))))))))]
    (cond
      (< 1 (count matches)) (do (prn "Multiple matches for " food-str) nil)
      (= 0 (count matches)) (extract-unitless-quantity food-str)
      :else (first matches))))

(assert=
  '("40 " "calories" "roasted seaweed")
  (extract-units "40 calories roasted seaweed"))
    
(defn -remove-of
  [of-food?]
  (let [clean-of-food? (trim of-food?)
        no-of-food (last (re-matches #"of\s+(.+)" clean-of-food?))]
    (if (nil? no-of-food) of-food? no-of-food)))

(defn -sum-and-quantity
  [quantity]
  (let [quantities (rest (re-matches #"(.+)\s+and\s+(.+)" quantity))]
    (if (empty? quantities)
      (js/parseFloat quantity)
      (let [[quantity1 quantity2] quantities]
        (+ (js/parseFloat quantity1) (js/parseFloat quantity2))))))

(defn parse-food
  {:malli/schema [:=> [:cat :string] Food]}
  [raw-food]
  (let [[quantity units food] (extract-units (numberify (lower-case
                                                          (trim raw-food))))
        summed-quantity       (-sum-and-quantity quantity)]
    {:name           (singular-fixed (-remove-of food))
     :quantity       summed-quantity
     :quantity-units units}))
  
(assert= {:name "potato" :quantity 3 :quantity-units "small"}
         (parse-food "three small potatoes"))
(assert= {:name "craisin" :quantity 0.5 :quantity-units "cup"}
         (parse-food "half cup craisins"))
(assert= {:name "popcorn kernel" :quantity 0.333 :quantity-units "cup"}
         (parse-food "1/3 cup popcorn kernels"))
(assert= {:name "nectarine" :quantity 1 :quantity-units "unit"}
         (parse-food "nectarine"))
(assert= {:name "carrot" :quantity 1 :quantity-units "unit"}
         (parse-food "one carrot"))
(assert= {:name "carrot" :quantity 1 :quantity-units "cup"}
         (parse-food "one cup of carrots"))
(assert= {:name "carrot" :quantity 1.5 :quantity-units "unit"}
         (parse-food "one and one half carrot"))
(assert= {:name "almond" :quantity 6 :quantity-units "unit"}
         (parse-food "six almond"))
(assert= {:name "green bean" :quantity 2 :quantity-units "cup"}
         (parse-food "two cups green beans"))
(assert= {:name "tea" :quantity 1.5 :quantity-units "cup"}
         (parse-food "one and a half cup  tea"))
(assert= {:name "tea" :quantity 0 :quantity-units "calories"}
         (parse-food "0 cal tea"))


(defn parse-foods
  {:malli/schema [:=> [:cat :string] [:sequential Food]]}
  [foods]
  (if (= foods "")
    []
    (map parse-food (split-lines foods))))


(defn row->meal
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Meal]}
  [row]
  {:datetime (:Timestamp row)
   :foods    (parse-foods (:Foods row))
   :oil      ((keyword "Oil Amount") row)
   :picture  (:Picture.http row)})

; Will probably need to roundtrip through strings to get daylight savings time
; right.
; The problem is that the timestamps in the momentodb table are local times
; without a timezone label, so when we convert them to unix timestamp longs
; here they are not actually correct, since unix timestamps are UTC.
(def pdt-offset (* 25200 1000))

(defn meal->event
  {:malli/schema [:=> [:cat Meal] Event]}
  [meal]
  {:start       (+ pdt-offset (to-long (:datetime meal)))
   :end         (+ pdt-offset
                   (to-long (plus (from-date (:datetime meal)) (minutes 30))))
   :summary     (join ", " (map :name (:foods meal)))
   :foods       (:foods meal)
   :description (with-out-str (pprint (dissoc meal :datetime)))})
