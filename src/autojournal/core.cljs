(ns autojournal.core
  ; We import all custom malli schemas to avoid undeclared-var error when malli
  ; goes to do the schema checking in the refresh function.
  (:require [autojournal.sheets :as sheets]
            [autojournal.calendar :as calendar]
            [autojournal.sleep :as sleep]
            [autojournal.gmail :as gmail]
            [autojournal.html-utils :refer [Hiccup]]
            [autojournal.schemas :refer [Timestamp EventFetcher Event Date]]
            [autojournal.food.main :as food]
            [autojournal.drive :as drive]
            [autojournal.mood :as mood]
            [autojournal.journal3 :as journal3]
            [autojournal.location :as location :refer [Reading TallyFunction]]
            [autojournal.env-switching :refer [env-switch]]
            [cljs-time.core :refer [date-time today minus days]]
            [malli.core :as m]
            [cljs-time.coerce :refer [to-long]]
            [malli.dev.cljs :as dev]
            [malli.dev.pretty :as pretty]))

(defn ^:export update-lifelog []
  (let [days-to-update 5]
    (mood/update-calendar! days-to-update)
    (journal3/update-calendar! days-to-update)
    (food/update-calendar! days-to-update)
    #_(let [today (today)
            yesterday (minus today (days 1))
            events (location/get-events
                     (to-long yesterday)
                     (to-long today))]
        (calendar/add-event! (first events))
        (sheets/update-events! events))))


(defn ^:export send-report-email
  {:malli/schema [:=> [:cat :int] Hiccup]}
  [days-to-summarize]
  (gmail/send-self-mail "Daily Report"
    [:html
     [:head]
     [:body
      (journal3/report days-to-summarize)
      (sleep/report days-to-summarize)
      (food/report days-to-summarize)]]))


(defn main [])

(defn ^:dev/after-load refresh []
  (env-switch
    {:node
      #(do (prn "Hot code Remount")
           ; Check all malli function schemas
           (dev/start! {:report (pretty/reporter)}))
     :app-script #()}))
