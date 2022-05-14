(ns autojournal.env-switching)

(defmacro env-switch [{:keys [node app-script]}]
  (case "APP_SCRIPT"
    "NODE" node
    "APP_SCRIPT" app-script))
