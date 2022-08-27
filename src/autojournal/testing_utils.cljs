(ns autojournal.testing-utils)

(defn pr
  "Print + Return"
  [x]
  (prn x)
  x)

(defn assert=
  [actual expected]
  (assert
    (= actual expected)
    (str "Actual: " actual "\n\nExpected: " expected)))
