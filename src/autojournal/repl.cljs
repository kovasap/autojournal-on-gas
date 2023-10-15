; This file should only be used to run code on the command line or via repl,
; NOT imported by anything running in apps script.

(ns autojournal.repl
  (:require [autojournal.core :refer [write-report]]))

(write-report)
