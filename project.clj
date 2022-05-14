(defproject autojournal "0.1.0-SNAPSHOT"
  :plugins [[lein-cljsbuild "1.1.7"]
            [reifyhealth/lein-git-down "0.4.1"]]
  :description "A tool for collecting life events into a single log."
  :url "https://github.com/kovasap/autojournal-on-gas"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 ; [org.clojure/clojure "1.9.0"]
                 ; [org.clojure/clojurescript "1.10.439"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.773"]
                 [com.google.javascript/closure-compiler-unshaded "v20200830"]
                 [thheller/shadow-cljs "2.11.23"]
                 ; [cider/piggieback "0.5.3"]
                 [metosin/malli "d6a8371fb50b86979d74ebc483c458d4d6181e3d"]]
  :repositories [["public-github" {:url "git://github.com"}]]
  :repl-options {}; :init (do (require 'cljs.repl.node)
                 ;           (cider.piggieback/cljs-repl (cljs.repl.node/repl-env))
                 ; ^ TODO uncomment when
                 ; https://github.com/nrepl/piggieback/issues/124 is fixed
                 ; :nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
                 ; :init-ns autojournal.core}
  :aliases {"noderepl" ["run" "-m" "clojure.main" "repl.clj"]}
  :source-paths ["src"]
  :cljsbuild {:builds [{
                        :source-paths ["src"]
                        :compiler {:main autojournal.core
                                   :optimizations :whitespace
                                   :output-to "./Code.js"
                                   :output-dir "target"
                                   :pretty-print true
                                   :externs ["resources/gas.ext.js"]}}]})
