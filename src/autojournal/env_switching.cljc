(ns autojournal.env-switching)

#?(:clj (defmacro env-switch [{:keys [node app-script]}]
          (case "APP_SCRIPT"
            "NODE" node
            "APP_SCRIPT" app-script)))
