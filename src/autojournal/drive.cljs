(ns autojournal.drive
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.sheets :refer [sheet-to-maps]]
            [autojournal.testing-utils :refer [assert=]]
            [cljs.nodejs :as nodejs]
            [clojure.string :refer [ends-with?]]
            [testdouble.cljs.csv :as csv]))

(def fs 
  (env-switch {:node #(nodejs/require "fs")
               :app-script #()}))

(defn -get-files
  [filename]
  (let [file-iterator (.. js/DriveApp (getFilesByName filename))]
    (loop [acc []]
       (if (.hasNext file-iterator)
         (recur (conj acc (.next file-iterator)))
         acc))))

(defn -get-file
  [filename]
  (let [files (-get-files filename)
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
  [s]
  (-two-d-array-to-maps (csv/read-csv s)))
    ; This function is only available in Apps Script.
    ; (js->clj (.parseCsv js/Utilities s))))

(declare -get-blob-contents)

(defn -get-zipped-files
  [zip-blob]
  (reduce concat
          (mapv -get-blob-contents
                (.unzip js/Utilities zip-blob))))

(defn -get-contents
  [filename filedata]
  (cond 
    (ends-with? filename ".csv") [(-parse-csv filedata)]
    (ends-with? filename ".json") [(js->clj (.parse js/JSON filedata)
                                     :keywordize-keys true)]
    :else [filedata]))
  

(defn -get-blob-contents
  [blob]
  (cond 
    (ends-with? (.getName blob) ".zip") (-get-zipped-files blob)
    :else (-get-contents (.getName blob) (.getDataAsString blob))))

(defn -get-file-contents
  [file]
  (cond
    (nil? file) []
    (= (.getMimeType file)
     "application/vnd.google-apps.spreadsheet") [(sheet-to-maps
                                                   (.getId file))]
    :else (-get-blob-contents (.getBlob file))))
  
(defn get-files
  "Gets data for all files with the given filename.

  Returns a list of data with an element for each file.  If there is only one
  file, make sure to call `first` or iterate to get your data!."
  [filename]
  (prn (str "Getting " filename))
  (time (env-switch
          {:node       #(-get-contents filename
                                       (try
                                         (.readFileSync fs filename "utf8")
                                         (catch js/Error e
                                           (prn e))))
           :app-script #(reduce concat
                          (map -get-file-contents (-get-files filename)))})))

(defn stringify-keys
  [m]
  (into {} (for [[k v] m]
             [(name k) v])))

(defn floatify-vals
  [m]
  (into {} (for [[k v] m
                 :let [floatv (js/parseFloat v)]]
             [k (if (or (float? v)
                        (js/Number.isNaN floatv)
                        ; parseFloat will take any leading numbers and make
                        ; them a float, so we explicitly make sure no letters
                        ; are in the string
                        (not (nil? (re-matches #".*[a-zA-Z]+.*" v))))
                    v
                    floatv)])))

(assert=
  {"a" "1.0 g"
   "b" 0
   "c" 1}
  (floatify-vals {"a" "1.0 g"
                  "b" "0.00"
                  "c" 1.00}))

(defn get-raw-rows
  [filename]
  (map (comp floatify-vals stringify-keys)
       (first (get-files filename))))

(defn trash-files
  [filename]
  (env-switch
    {:node #(prn (str "trash-files called with " filename))
     :app-script #(doseq [file (-get-files filename)]
                    (prn (str "trashing " filename))
                    (.setTrashed file true))}))

(defn overwrite-file
  [filename content]
  (env-switch
    {:node #(do
              (prn (str "Writing file to " filename))
              (.writeFileSync fs filename content))
     :app-script (fn []
                   (trash-files filename)
                   (prn (str "writing " filename))
                   (.. js/DriveApp (createFile filename content)))}))
