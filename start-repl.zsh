#!/bin/zsh

(echo "(require 'cljs.repl.node)"\
      "(cider.piggieback/cljs-repl (cljs.repl.node/repl-env))"\
      "\n(require 'autojournal.core)"\
      ; cat <&0) | lein repl
