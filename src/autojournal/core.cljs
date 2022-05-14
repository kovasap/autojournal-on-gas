(ns autojournal.core
  (:require [autojournal.entrypoints]))

(defn ^:export update-lifelog []
  (.. js/SpreadsheetApp
    (openById "1ZDPrV6ZngilK00Pb0DVs64yAVs6YQtiLr_vE5-YCiLc")
    (appendRow #js ["hello", "world"])))
; https://lambdaisland.com/blog/2016-10-01-clojurescript-and-google-apps-script

(+ 1 1)

(defn t []
  "hello")

(t)

(update-lifelog)

(defn main [] (t))
