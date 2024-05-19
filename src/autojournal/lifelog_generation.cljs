(ns autojournal.lifelog-generation
  (:require [autojournal.time :refer [get-day-tuple]]
            [cljs-time.core :refer [before?]]
            [clojure.string :as st]))


(defn entries->sentence-summaries-by-day
  "Translator takes an entry and turns it into a string."
  ([[entries translator]]
   (entries->sentence-summaries-by-day entries translator))
  ([entries translator]
   (into {}
         (for [[datetime-day daily-entries] (group-by #(get-day-tuple
                                                         (:datetime %))
                                                      entries)]
           [datetime-day
            (->> daily-entries
                 ; (filter #(not (contains? (:tags %) "sleep")))
                 (sort-by :datetime before?)
                 (map translator)
                 (st/join ".  "))]))))

(defn build-lifelog-html
  "Takes in a list of (entries, translator) pairs (e.g. a map)."
  [entries-with-translators]
  (let [sentence-summaries (apply merge-with
                             str
                             (map entries->sentence-summaries-by-day
                               entries-with-translators))]
    [:html
     [:head]
     [:body
      "Check out https://kovasap.github.io/docs/lifestyle-optimizations/daily-physiological-tricks/. "
      [:br]
      "This tries to emulate the format at https://github.com/kovasap/life-timeline/blob/main/timeline.md."
      [:br]
      (into [:ul]
            (for [[[year month day] sentences] (sort-by first sentence-summaries)]
              [:li [:strong (str month "/" day)] ": " sentences]))]]))
