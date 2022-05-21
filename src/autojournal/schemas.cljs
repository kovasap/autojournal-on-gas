(ns autojournal.schemas
  (:require [malli.core :as m]
            ; https://github.com/metosin/malli#programming-with-schemas
            [malli.util :as mu]
            [cljs-time.coerce :refer [to-long from-long to-string]]
            [clojure.string :as st]))


(def Timestamp [:and :int [:>= 0]])

(def Date
  [:map [:day :int]
        [:month :int]
        [:year :int]])


(def WeighInEvent
  [:map [:weight-lbs :double]
        [:body-fat-percentable :double]])

(def LocationEvent
  [:map [:speed-m/s :double]])


(def Event
  (m/form
    (reduce mu/merge [WeighInEvent
                      LocationEvent
                       [:map [:start Timestamp]
                             [:end Timestamp]
                             [:feelings [:sequential :string]]
                             [:summary :string]
                             [:description :string]]])))

(def EventFetcher
  [:=> [:cat Timestamp] ; Start time
   [:sequential Event]])
