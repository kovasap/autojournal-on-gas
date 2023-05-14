(ns autojournal.continuous-glucose-monitoring
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [Event]]
            [cljs-time.core :refer [plus minutes]]
            [cljs-time.coerce :refer [to-long from-date]]
            [cljs-time.format :refer [parse formatter]]
            [cljs.pprint :refer [pprint]]))

(def time-formatter (formatter "%m-%d-%Y %I:%M %p"))

(defn row->event
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Event]}
  [row]
  (prn row)
  (let [time-string ((keyword "Device Timestamp") row)]
        ; timestamp (parse time-formatter time-string)]
    {:start       time-string
     :end         time-string 
     :glucose     (js/parseFloat ((keyword "Historic Glucose mg/dL") row))
     :summary     ""
     :description ""})) ;(with-out-str (pprint row))}))

(defn get-data
  [filename]
  (reduce concat
    (for [file-data (drive/get-files filename)]
      (mapv row->event file-data))))
