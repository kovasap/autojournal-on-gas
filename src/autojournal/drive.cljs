(ns autojournal.drive
  (:require [autojournal.env-switching :refer [env-switch]]))

(defn -get-file
  [name]
  (.. js/DriveApp
      (getFilesByName name)
      (next)))

(defn get-files
  [name]
  (env-switch
    {:node #(prn name)
     :app-script #(.. (-get-file name)
                      (getBlob)
                      (getDataAsString))}))
