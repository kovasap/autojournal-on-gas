(ns autojournal.env-switching)

#?(:clj (defmacro env-switch [{:keys [node app-script]}]
          (println (System/getenv "APPS_SCRIPT_BUILD"))
          (if (System/getenv "APPS_SCRIPT_BUILD")
            app-script
            node)))
