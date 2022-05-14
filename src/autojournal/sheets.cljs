(ns autojournal.sheets
  (:require [autojournal.env-switching :refer-macros [env-switch]]))

(defn append!
  [id row] 
  (env-switch
    {:node (prn id row)
     :app-script (.. js/SpreadsheetApp
                   (openById id)
                   (appendRow (clj->js row)))}))
