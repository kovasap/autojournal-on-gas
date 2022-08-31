(ns autojournal.gmail
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.testing-utils :refer [assert=]]
            [hiccups.runtime :refer [render-html]]))

(defn send-self-mail
  [subject hiccup]
  (let [html-contents (str "<!DOCTYPE html>" (render-html hiccup))]
    (env-switch
      {:node #(do (prn subject html-contents) html-contents)
       :app-script #(.. js/GmailApp
                      (sendEmail
                        (.. js/Session
                          (getActiveUser)
                          (getEmail))
                        subject
                        ""
                        (clj->js {:htmlBody html-contents})))})))

