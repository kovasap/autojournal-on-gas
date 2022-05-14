(ns autojournal.sheets)

(defn append!
  [id row] 
  (prn id row))  
  ; (.. js/SpreadsheetApp
  ;   (openById id)
  ;   (appendRow (clj->js row))])
