(ns autojournal.time
  (:require
    [cljs-time.format :refer [unparse formatter]]
    [cljs-time.core :refer [date-time year month day hour minute second]]))

(defn days-between
  [date1 date2]
  (let [diff_ms (Math/abs (- (.getTime date1) (.getTime date2)))]
    (/ diff_ms 1000.0 60 60 24)))

(def JsDate :any)

(defn recent-items
  {:malli/schema [:=> [:cat [:sequential [:map [:datetime JsDate]]]
                       :integer]
                  [:sequential [:map [:datetime JsDate]]]]}
  [data days]
  (let [today (js/Date.)]
    (filter #(< (days-between (:datetime %) today) days)
            data)))

(defn get-day-str
  [datetime]
  (unparse (formatter "MM/dd") datetime))

(defn get-day
  [datetime]
  (date-time (year datetime) (month datetime) (day datetime)))

(defn get-day-tuple
  [datetime]
  [(year datetime) (month datetime) (day datetime)])
