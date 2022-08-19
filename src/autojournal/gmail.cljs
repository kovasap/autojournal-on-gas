(ns autojournal.gmail
  (:require [autojournal.env-switching :refer [env-switch]]
            [autojournal.testing-utils :refer [assert=]]))

(defn send-self-mail
  [subject contents]
  (env-switch
    {:node #(prn subject contents)
     :app-script #(.. js/GmailApp
                    (sendEmail 
                      (.. js/Session
                          (getActiveUser)
                          (getEmail))
                      subject
                      contents))}))

