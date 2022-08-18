(ns autojournal.sheets
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.testing-utils :refer [assert=]]))

(defn append!
  [id row]
  (env-switch
    {:node #(prn id row)
     :app-script #(.. js/SpreadsheetApp
                    (openById id)
                    (appendRow (clj->js row)))}))

(defn get-sheet-data
  [id]
  (env-switch
    {:node #(prn id)
     :app-script #(js->clj
                    (.. js/SpreadsheetApp
                      (openById id)
                      (getSheetByName "Food")
                      (getDataRange)
                      (getValues)))}))
 

(def lifelog-name "autojournal-lifelog")

(defn -get-lifelog-sheet
  "Creates the sheet if it doesn't exist."
  []
  (let [existing-files (.. js/DriveApp (getFilesByName lifelog-name))]
    (if (. existing-files hasNext) 
      (. js/SpreadsheetApp open (. existing-files next))
      (. js/SpreadsheetApp create lifelog-name))))


(defn -update-headers
  [maps headers]
  (let [header-set (set headers)
        all-keys (set (map name (reduce concat (map keys maps))))
        new-headers (filter #(not (header-set %)) all-keys)]
    (vec (concat headers new-headers))))

(assert=
  ["a" "b"]
  (-update-headers
    [{:a 1 :b 2}]
    ["a"]))

  
(defn -map-to-vec
  [m headers]
  (into [] (for [h headers]
             (if (contains? m (keyword h))
               ((keyword h) m)
               ""))))

(assert=
  [1 "" 3]
  (-map-to-vec
    {:a 1 :b 3}
    ["a" "c" "b"]))


(defn -pad-headers
  [desired-size headers]
  (concat headers (repeat (- desired-size (count headers))
                          "")))

(assert=
  '("a" "b" "" "" "")
  (-pad-headers
    5 ["a" "b"]))


(defn update-events!
  [new-events]
  (let [sheet (first (. (-get-lifelog-sheet) getSheets))
        header-range (. sheet getRange "1:1")
        headers (->> (. header-range getValues)
                  (js->clj)
                  (first)
                  (filter #(not (= % "")))
                  (-update-headers new-events)
                  (-pad-headers (. header-range getWidth)))]
    (prn "HEADERS")
    (prn headers)
    (. header-range setValues (clj->js [headers]))
    (. sheet insertRows 2 (count new-events))
    (. (. sheet getRange 2 1 (count new-events) (count headers))
       setValues (clj->js (into [] (for [e new-events]
                                     (-map-to-vec e headers)))))))
    
  
(defn sheet-to-maps
  "Converts a Google Sheet into a seq of maps"
  [sheet-id]
  (let [sheet-data (get-sheet-data sheet-id)
        headers (map keyword (first sheet-data))
        rows (rest sheet-data)]
    (for [row rows]
      (zipmap headers row))))

