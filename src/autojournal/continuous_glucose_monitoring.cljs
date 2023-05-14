(ns autojournal.continuous-glucose-monitoring
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [Event]]
            [cljs.pprint :refer [pprint]]))

(defn row->event
  {:malli/schema [:=> [:cat [:map-of :keyword :string]] Event]}
  [row]
  (prn row)
  (let [time-string ((keyword "Device Timestamp") row)]
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
