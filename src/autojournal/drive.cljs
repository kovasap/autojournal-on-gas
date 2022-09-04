(ns autojournal.drive
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.sheets :refer [sheet-to-maps]]
            [clojure.string :refer [ends-with?]]))

(defn -get-file
  [filename]
  (let [file-iterator (.. js/DriveApp (getFilesByName filename))
        files (loop [acc []]
                (if (.hasNext file-iterator)
                  (recur (conj acc (.next file-iterator)))
                  acc))
        num-files (count files)]
    (cond
     (= 0 num-files) (prn (str "No files with name " filename))
     (< 1 num-files) (prn (str num-files " files with name " filename))
     :else (first files))))

(defn -two-d-array-to-maps
  [two-d-array]
  (let [header (first two-d-array)
        rows (rest two-d-array)]
    (into [] (for [row rows]
               (into {} (for [[k v] (partition 2 (interleave header row))]
                          [(keyword k) v]))))))
    
  
(-two-d-array-to-maps
  [["time" "lat" "lon" "elevation" "accuracy" "bearing" "speed" "satellites" "provider" "hdop" "vdop" "pdop" "geoidheight" "ageofdgpsdata" "dgpsid" "activity" "battery" "annotation"]
   ["2022-05-15T07:00:28.000Z" "47.66866721" "-122.31444872" "44.99261474609375" "9.648001" "" "0.0" "0" "gps" "" "" "" "" "" "" "" "71" ""]
   ["2022-05-15T07:01:03.000Z" "47.66866769" "-122.31444892" "45.0035400390625" "9.648001" "" "0.0" "0" "gps" "" "" "" "" "" "" "" "72" ""]
   ["2022-05-15T07:01:37.000Z" "47.66866761" "-122.31444817" "44.888916015625" "7.504" "" "0.0" "0" "gps" "" "" "" "" "" "" "" "72" ""]
   ["2022-05-15T07:02:10.000Z" "47.66866714" "-122.3144491" "44.9139404296875" "6.432" "" "0.0" "0" "gps" "" "" "" "" "" "" "" "73" ""]])


(defn -parse-csv
  [blob]
  (let [two-d-array (js->clj (.parseCsv js/Utilities
                                        (.getDataAsString blob)))]
    (-two-d-array-to-maps two-d-array)))

(declare -get-blob-contents)

(defn -get-zipped-files
  [zip-blob]
  (reduce concat
          (mapv -get-blob-contents
                (.unzip js/Utilities zip-blob))))

(defn -get-blob-contents
  [blob]
  (cond 
    (ends-with? (.getName blob) ".zip") (-get-zipped-files blob)
    (ends-with? (.getName blob) ".csv") [(-parse-csv blob)]
    :else [(.getDataAsString blob)]))

(defn -get-file-contents
  [file]
  (prn file)
  (prn (.getName file))
  (cond
    (nil? file) []
    (= (.getMimeType file)
     "application/vnd.google-apps.spreadsheet") [(sheet-to-maps
                                                   (.getId file))]
    :else (-get-blob-contents (.getBlob file))))
       
  
(defn get-files
  [filename]
  (prn (str "Getting " filename))
  (env-switch
    {:node #(prn (str "get-files called with " filename))
     :app-script #(-get-file-contents
                    (-get-file filename))}))
