(ns autojournal.location
  (:require [autojournal.drive :as drive]
            [autojournal.schemas :refer [EventFetcher Event Date]]))
  
(prn (drive/get-files "journal.txt"))

(def -get-files
  {:malli/schema [:=> [:cat [:sequential Date]] ; Start time
                  [:sequential Event]]}
  [days]
  )

(defn get-events
  EventFetcher
  [start-time])
  
  
