(ns autojournal.location
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [EventFetcher Event Date]]))
  
(prn (drive/get-files "journal.txt"))

(defn -date-to-file
  {:malli/schema [:=> [:cat Date] :string]}
  [{:keys [day month year]}]
  (str year month day ".zip"))
  

(defn -get-files
  {:malli/schema [:=> [:cat [:sequential Date]] ; Start time
                  [:sequential Event]]}
  [days])
  

(defn get-events
  EventFetcher
  [start-time])
  
  
