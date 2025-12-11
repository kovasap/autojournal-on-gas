(ns autojournal.cool-stuff
  (:require [autojournal.drive :as drive]
            [clojure.string :as st]))

(def sheet-name "Cool Stuff")

(def num-random-things 5)
(def num-priority-things 1)

(defn row->li
  [{:keys [Name Tags Active Notes]}]
  [:li [:strong Name] ", " (st/join ", " Tags) ". " Notes])

(defn build-reminder-email
  []
  (let [sheet-data (->> sheet-name
                        (drive/get-files)
                        (first)
                        (remove #(= (:Name %) ""))
                        (remove #(= (:Active %) "Inactive"))
                        (map #(update %
                                      :Tags
                                      (fn [tags]
                                        (set (map st/trim
                                               (st/split tags ","))))))
                        (shuffle))]
    {:title (str "Cool stuff!  Like " (:Name (first sheet-data)))
     :html
     [:html
      [:head]
      [:body
       "Priority:"
       (into [:ul]
             (->> sheet-data
                  (drop 1)
                  (filter #(= (:Active %) "Priority"))
                  (take num-priority-things)
                  (map row->li)))
       "Music:"
       (into [:ul]
             (->> sheet-data
                  (drop 1)
                  (filter #(contains? (:Tags %) "Music"))
                  (take num-random-things)
                  (map row->li)))
       (str num-random-things " more random things:")
       (into [:ul]
             (->> sheet-data
                  (drop 1)
                  (remove #(contains? (:Tags %) "Music"))
                  (take num-random-things)
                  (map row->li)))
       "See full sheet at https://docs.google.com/spreadsheets/d/1jnb_Yg9BYhG2O-RGe822YODZSJu2LcjvORL4Q-zDqxA/edit."]]}))
