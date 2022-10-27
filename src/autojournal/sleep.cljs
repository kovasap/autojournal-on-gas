(ns autojournal.sleep
  (:require [autojournal.calendar :refer [get-cal-by-name-cached]]
            [autojournal.html-utils :refer [Hiccup]]
            [cljs-time.coerce :refer [from-long to-date to-long]]
            [cljs-time.core :refer [now days ago]]))


; TODO populate and use
(def Night
  [:map
   [:activity :string]
   [:raw-data :any]])

(defn get-sleep-events
  {:malli/schema [:=> [:cat :int] [:sequential Night]]}
  [days-to-summarize]
  (let [calendar (get-cal-by-name-cached "Sleep as Android")]
    (js->clj (. calendar
                (getEvents (to-date (-> days-to-summarize days ago))
                           (to-date (now))))
             :keywordize-keys true)))

(defn report
  "Generates a hiccup report section with sleep data."
  {:malli/schema [:=> [:cat :int] Hiccup]}
  [days-to-summarize]
  (let [events (get-sleep-events days-to-summarize)]
    (into [:div] (for [event events]
                   [:p ((:getDescription event))]))))
