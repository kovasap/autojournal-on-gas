(ns autojournal.vega
  (:require [autojournal.drive :refer [write-file]]
            [clojure.string :as st]))
  ; (:require ["vega" :as vega]))

; This commented code requires an npm library, which is not accessible from
; Apps Script (I don't think).
#_(defn make-vega-plot [spec]
    ; var view = new vega.View(vega.parse(spec), {renderer: 'none'});
    (let [view (new vega/View (vega/parse (clj->js spec)
                                          #js {"renderer" "none"}))]
      (.. view (toSVG) (then (clj->js (fn [svg] (prn svg)))))))

#_(make-vega-plot {:$schema "https://vega.github.io/schema/vega-lite/v5.json",
                   :description "A simple bar chart with embedded data.",
                   :data
                   {:values
                    [{:a "A", :b 28}
                     {:a "B", :b 55}
                     {:a "C", :b 43}
                     {:a "D", :b 91}
                     {:a "E", :b 81}
                     {:a "F", :b 53}
                     {:a "G", :b 19}
                     {:a "H", :b 87}
                     {:a "I", :b 52}]},
                   :mark "bar",
                   :encoding
                   {:x {:field "a", :type "nominal", :axis {:labelAngle 0}},
                    :y {:field "b", :type "quantitative"}}})

(defn plot-events
  "Returns vega-data."
  [events fields-to-plot]
  {:$schema     "https://vega.github.io/schema/vega-lite/v5.json"
   :description "events"
   :data        {:values events}
   ; Makes a stack of plots, one for each field.
   ; See https://stackoverflow.com/a/62460026 for scrolling
   ; example/explanation.
   :vconcat     (into
                  []
                  (for [field fields-to-plot]
                    {:mark      "line"
                     :width     1000
                     :height    400
                     :encoding  {:x       {:field "start" :type "temporal"}
                                 :y       {:field field :type "quantitative"}
                                 :tooltip {:field field :type "quantitative"}}
                     :selection {:x_scroll {:type      "interval"
                                            :bind      "scales"
                                            :encodings ["x"]}}}))
   ; Lets us scroll on the multiple plots with the x axis moving together and x
   ; independently.
   :resolve     {:scale {:x "shared" :y "independent"}}})


(defn plot-events-hourly
  [events fields-to-plot]
  {:$schema     "https://vega.github.io/schema/vega-lite/v5.json"
   :description "hourly_events"
   :data        {:values events}
   ; Makes a stack of plots, one for each field.
   ; See https://stackoverflow.com/a/62460026 for scrolling
   ; example/explanation.
   :vconcat     (into
                  []
                  (for [field fields-to-plot]
                    {:mark      "bar"
                     :width     500
                     :height    400
                     :encoding  {:x       {:timeUnit "hours"
                                           :field    "start"
                                           :type     "temporal"}
                                 :y       {:aggregate "mean" :field field}}
                     :selection {:x_scroll {:type      "interval"
                                            :bind      "scales"
                                            :encodings ["x"]}}}))
   ; Lets us scroll on the multiple plots with the x axis moving together and x
   ; independently.
   :resolve     {:scale {:x "shared" :y "independent"}}})
  

(defn make-all-event-plots
  [events fields-to-plot]
  [(plot-events events fields-to-plot)
   (plot-events-hourly events fields-to-plot)])
  
       
(defn vega-chart
  [vega-data]
  (let [chart-name (:description vega-data)]
    ; 2 in stringify call means pretty print with spacing of 2
    (str "<div id='"chart-name"'></div>
     <script type='text/javascript'>
      vegaEmbed('#"chart-name"', "(.stringify js/JSON (clj->js vega-data) 2)");
     </script>")))

(defn vega-page
  "See https://vega.github.io/vega-lite/usage/embed.html"
  [vega-charts]
  (str "<!DOCTYPE html>
<html>
  <head>
    <title>Embedding Vega-Lite</title>
    <script src='https://cdn.jsdelivr.net/npm/vega@5.25.0'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-lite@5.9.0'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-embed@6.22.1'></script>
  </head>
  <body>
  " (st/join "\n" vega-charts) "
  </body>
</html>
  "))

(defn write-vega-page
  [filename vega-datas]
  (write-file filename (vega-page (map vega-chart vega-datas))))
