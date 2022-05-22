(ns autojournal.location-test
  (:require [autojournal.location :as loc]
            [cljs.test :refer (deftest is)]))

(deftest are-same-place
  (is (loc/-are-same-place 
        (loc/-row-to-reading
          {:elevation "44.99261474609375",
           :accuracy "9.648001",
           :time "2022-05-15T07:00:28.000Z",
           :lon "-122.31444872",
           :lat "47.66866721"})
        (loc/-row-to-reading
          {:elevation "45.0035400390625",
           :accuracy "9.648001",
           :time "2022-05-15T07:01:03.000Z",
           :lon "-122.31444892",
           :lat "47.66866769"}))))

(deftest are-not-same-place
  (is (not (loc/-are-same-place 
             (loc/-row-to-reading
               {:elevation "44.99261474609375",
                :accuracy "9.648001",
                :time "2022-05-15T07:00:28.000Z",
                :lon "-122.31444872",
                :lat "47.66866721"})
             (loc/-row-to-reading
               {:elevation "45.0035400390625",
                :accuracy "9.648001",
                :time "2022-05-15T07:01:03.000Z",
                :lon "-142.31444892",
                :lat "47.66866769"})))))


(deftest many-readings-to-events
  (is (= []
         (loc/-readings-to-events []))))
