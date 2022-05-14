(ns autojournal.core
  (:require [autojournal.sheets :as sheets]
            [autojournal.env-switching :refer-macros [env-switch]]
            [malli.core :as m]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export update-lifelog []
  (sheets/append! "1ZDPrV6ZngilK00Pb0DVs64yAVs6YQtiLr_vE5-YCiLc"
                  ["hello" "world"]))

(update-lifelog)

(+ 1 1)

(defn t
  {:malli/schema [:=> [:cat :int]
                  [:sequential :keyword]]}
  [i]
  (repeat i :hello))

(t 5)

(defn main [] (t 2))

(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      (do (prn "Hot code Remount")
          (dev/start! {:report (pretty/reporter)}))  ; Check all malli function schemas
     :app-script nil}))
