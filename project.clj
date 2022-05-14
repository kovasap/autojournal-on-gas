(defproject autojournal "0.1.0-SNAPSHOT"
  :plugins [[lein-cljsbuild "1.1.7"]
            [reifyhealth/lein-git-down "0.4.1"]]
  :repositories [["public-github" {:url "git://github.com"}]]
  :description "A tool for collecting life events into a single log."
  :url "https://github.com/kovasap/autojournal-on-gas"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 ; These older versions of clojure and clojurescript are
                 ; necessary for some reason with this build configuration. If
                 ; I use the newer ones in shadow-cljs.edn, I get errors when I
                 ; try to push to google apps script.
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [metosin/malli "0.8.4"]]
  :cljsbuild {:builds [{
                        :source-paths ["src"]
                        :compiler {:main autojournal.core
                                   :optimizations :whitespace
                                   :output-to "./Code.js"
                                   :output-dir "target"
                                   :pretty-print true
                                   :externs ["resources/gas.ext.js"]}}]})
