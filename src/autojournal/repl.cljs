; This file should only be used to run code on the command line or via repl,
; NOT imported by anything running in apps script.

(ns autojournal.repl
  (:require [autojournal.core :refer [write-report]]
            [autojournal.drive :refer [get-files]]
            [autojournal.food.main :as food]))

(write-report)
; (food/get-last-meal-events 200)
