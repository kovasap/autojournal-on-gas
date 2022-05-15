(ns autojournal.env-switching)

(defn env-switch [{:keys [node app-script]}]
  (if (resolve 'js/SpreadsheetApp)
    (app-script)
    (node)))
