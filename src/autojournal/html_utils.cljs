(ns autojournal.html-utils)

(def Hiccup
  [:schema
   {:registry {"hiccup" [:orn
                         [:node [:catn
                                 [:name keyword?]
                                 [:props [:? [:map-of keyword? any?]]]
                                 [:children [:* [:schema [:ref "hiccup"]]]]]]
                         [:primitive [:orn
                                      [:nil nil?]
                                      [:boolean boolean?]
                                      [:number number?]
                                      [:text string?]]]]}}
   "hiccup"])


(defn make-table
  {:malli/schema [:=> [:cat [:sequential :string]
                            [:sequential [:sequential :any]]]
                   Hiccup]}
  [headers rows]
  [:table
   [:tbody
    ; https://www.w3schools.com/html/html_table_headers.asp
    (into [:tr] (for [h headers] [:th h]))
    (for [row rows]
      (into [:tr] (for [i row] [:td i])))]])


