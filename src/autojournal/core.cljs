(ns autojournal.core
  (:require [autojournal.sheets :as sheets]
            [autojournal.drive :as drive]
            [autojournal.env-switching :refer [env-switch]]
            [malli.core :as m]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export update-lifelog []
  (prn (into [] (for [file (drive/get-files "20220515.zip")]
                  file)))
  (sheets/append! "1ZDPrV6ZngilK00Pb0DVs64yAVs6YQtiLr_vE5-YCiLc"
                  ["hello" "world"]))

(defn main [] (update-lifelog))

(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      #(do (prn "Hot code Remount")
           (dev/start! {:report (pretty/reporter)}))  ; Check all malli function schemas
     :app-script #()}))
