(ns autojournal.vega
  (:require [autojournal.drive :refer [write-file]]))
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
  [events]
  {:$schema     "https://vega.github.io/schema/vega-lite/v5.json"
   :description "Chart of events."
   :data        {:values events}
   :mark        "line"
   :encoding    {:x {:field "start" :type "temporal"}
                 :y {:field "glucose" :type "quantitative"}}})

(defn vega-page
  "See https://vega.github.io/vega-lite/usage/embed.html"
  [vega-data]
  (str "<!DOCTYPE html>
<html>
  <head>
    <title>Embedding Vega-Lite</title>
    <script src='https://cdn.jsdelivr.net/npm/vega@5.25.0'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-lite@5.9.0'></script>
    <script src='https://cdn.jsdelivr.net/npm/vega-embed@6.22.1'></script>
  </head>
  <body>
    <div id='vis'></div>

    <script type='text/javascript'>
      var yourVlSpec = "
      (.stringify js/JSON (clj->js vega-data) 2)  ; 2 means pretty print with spacing of 2
      "
      // Example:
      // {
      //   $schema: 'https://vega.github.io/schema/vega-lite/v5.json',
      //   description: 'A simple bar chart with embedded data.',
      //   data: {
      //     values: [
      //       {a: 'A', b: 28},
      //       {a: 'B', b: 55},
      //       {a: 'C', b: 43},
      //       {a: 'D', b: 91},
      //       {a: 'E', b: 81},
      //       {a: 'F', b: 53},
      //       {a: 'G', b: 19},
      //       {a: 'H', b: 87},
      //       {a: 'I', b: 52}
      //     ]
      //   },
      //   mark: 'bar',
      //   encoding: {
      //     x: {field: 'a', type: 'ordinal'},
      //     y: {field: 'b', type: 'quantitative'}
      //   }
      // };
      vegaEmbed('#vis', yourVlSpec);
    </script>
  </body>
</html>
  "))

(defn write-vega-page
  [filename vega-data]
  (write-file filename (vega-page vega-data)))
