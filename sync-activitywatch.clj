#!/usr/bin/env bb

(require '[clojure.java.io]
         '[cheshire.core :as json]
         '[clojure.string :as string]
         '[clojure.tools.cli :refer [parse-opts]])

(import 'java.time.format.DateTimeFormatter
        'java.time.LocalDateTime
        'java.time.ZonedDateTime
        'java.time.ZoneOffset)

(def cli-options
  ;; An option with a required argument
  [#_["-o" "--offset NUM" "Offset to add to each track number."
      :default 0
      :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])
#_(def offset (:offset (:options (parse-opts *command-line-args* cli-options))))

(def buckets
  (json/parse-string (slurp "http://localhost:5600/api/0/buckets/")
                     true))

(prn buckets)

(def date-formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
; Activitywatch works in GMT/UTC
(def today (ZonedDateTime/now ZoneOffset/UTC))

(def yesterday-str (.format (.minusDays today 1) date-formatter))
(def today-str (.format today date-formatter))
(def tomorrow-str (.format (.plusDays today 1) date-formatter))

(prn today-str)
(prn yesterday-str)
(prn tomorrow-str)

(def all-data
  (vec
    (reduce concat
      (for [[bucket-name bucket] buckets]
        #_(let [last-update-date (first (string/split (:last_updated bucket)
                                                      #"T"))])
        ; use http://localhost:5600/api/ to play with this
        (json/parse-string (slurp (str "http://localhost:5600/api/0/buckets/"
                                       (name bucket-name)
                                       "/events?"
                                       ; exclusive
                                       "end="    tomorrow-str
                                       ; inclusive
                                       "&start=" today-str)))))))
    
(spit (str "activitywatch-" today-str ".edn") (str all-data))
