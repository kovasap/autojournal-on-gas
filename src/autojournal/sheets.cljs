(ns autojournal.sheets
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.testing-utils :refer [assert=]]
            [clojure.string :refer [ends-with?]]))

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
                      (getActiveSheet)
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

(defn append-row
  "Row is a vector of primitives."
  [sheet row]
  (. sheet (appendRow (clj->js row)))) 

(defn all-keys
  [maps]
  (distinct (reduce concat (map keys maps))))

(defn maps-to-sheet
  "We assume that the maps are not nested, and that the keys are strings."
  [maps sheet-name]
  (let [sheet (.. js/SpreadsheetApp
                  (create sheet-name)
                  (getActiveSheet))
        headers (sort-by #(cond
                            (ends-with? % ")") (str "z" %)
                            (= % "Food Name") (str "AA" %)
                            :else %)
                         (all-keys maps))]
    (append-row sheet headers)
    (doseq [m maps]
      (append-row sheet (into [] (for [h headers] (get m h)))))))


(defn sheet-to-maps
  "Converts a Google Sheet into a seq of maps"
  [sheet-id]
  (let [sheet-data (get-sheet-data sheet-id)
        headers (map keyword (first sheet-data))
        rows (rest sheet-data)]
    (for [row rows]
      (zipmap headers row))))


(defn transpose [m]
  (apply mapv vector m))

(defn transposed-sheet-to-maps
  "Converts a Google Sheet into a seq of maps where the keys are the first
  column."
  [sheet-id]
  (let [sheet-data (transpose (get-sheet-data sheet-id))
        headers (map keyword (first sheet-data))
        rows (rest sheet-data)]
    (for [row rows]
      (zipmap headers row))))
