; A failed attempt at using https://github.com/emezeske/lein-cljsbuild/issues/81
; Doesn't work because the :nrepl-middleware [cider.piggieback/wrap-cljs-repl]
; isn't included (I think).
(require 'cljs.repl)
(require 'cider.piggieback)
(require 'cljs.build.api)
(require 'cljs.repl.node)

(cljs.build.api/build "src"
  {:main 'autojournal.core
   :output-to "out/main.js"
   :verbose true})

(cljs.repl/repl (cljs.repl.node/repl-env)
; (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)
 :watch "src"
 :output-dir "out")
