(ns autojournal.testing-utils)

(defn assert=
  [actual expected]
  (assert
    (= actual expected)
    (str "Actual: " actual "\n\nExpected: " expected)))
