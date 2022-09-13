(ns autojournal.time)

(defn days-between
  [date1 date2]
  (let [diff_ms (Math/abs (- (.getTime date1) (.getTime date2)))]
    (/ diff_ms 1000.0 60 60 24)))

(def JsDate :any)

(defn recent-items
  {:malli/schema [:=> [:cat [:sequential [:map [:datetime JsDate]]]
                       :integer]
                  [:sequential [:map [:datetime JsDate]]]]}
  [food-data days]
  (let [today (js/Date.)]
    (filter #(< (days-between (:datetime %) today) days)
            food-data)))
