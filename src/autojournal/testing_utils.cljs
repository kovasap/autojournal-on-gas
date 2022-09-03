(ns autojournal.testing-utils
  (:require [autojournal.env-switching :refer [env-switch]]))

(defn node-only
  [func]
  (env-switch
    {:node func
     :app-script (fn [] nil)}))

(defn assert=
  [actual expected]
  (node-only
    #(assert
       (= actual expected)
       (str "Actual: " actual "\n\nExpected: " expected))))
