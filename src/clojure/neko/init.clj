(ns neko.init
  "Contains functions for neko initialization and setting runtime options."
  (:require [neko context resource compilation debug]
            [neko.tools.repl :refer [start-nrepl-server]])
  (:import android.content.Context))

(defmacro ^:private enable-dynamic-compilation
  "Expands into dynamic compilation initialization if conditions are met."
  [context]
  (when (or (not (::release-build *compiler-options*))
            (::start-nrepl-server *compiler-options*)
            (::enable-dynamic-compilation *compiler-options*))
    `(neko.compilation/init ~context)))

(defn enable-compliment-sources
  "Initializes compliment sources if theirs namespaces are present."
  []
  (try (require 'neko.compliment.android-resources)
       ((resolve 'neko.compliment.android-resources/init-source))
       (require 'neko.compliment.ui-widgets-and-attributes)
       ((resolve 'neko.compliment.ui-widgets-and-attributes/init-source))
       (catch Exception ex nil)))

(def ^{:doc "Represents if initialization was already performed."
       :private true}
  initialized? (atom false))

(defn init
  "Initializes neko library.

  Initializes compilation facilities and runs nREPL server if appropriate. Takes
  the application context and optional arguments in key-value fashion. Optional
  arguments are feeded to the nREPL's `start-server` function."
  [context & {:keys [port] :as args}]
  (when-not @initialized?
    (alter-var-root #'neko.context/context (constantly context))
    (.put neko.debug/all-activities :neko.context/context context)
    (alter-var-root #'neko.resource/package-name
                    (constantly (.getPackageName ^Context context)))
    (enable-dynamic-compilation context)
    ;; Ensure that `:port` is provided, pass all other arguments as-is.
    (start-nrepl-server port (mapcat identity (dissoc args :port)))
    (enable-compliment-sources)
    (reset! initialized? true)))
