(ns neko.init
  "Contains functions for neko initialization and setting runtime options."
  (:require [neko context resource compilation threading]
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

  Initializes compilation facilities and runs nREPL server if
  appropriate. Takes the application context and optional arguments in
  key-value fashion. The value of `:classes-dir` specifies the path
  where neko should store compiled files. Other optional arguments are
  directly feeded to the nREPL's `start-server` function. "
  [context & {:keys [port] :as args}]
  (when-not @initialized?
    (alter-var-root #'neko.context/context (constantly context))
    (alter-var-root #'neko.resource/package-name
                    (constantly (.getPackageName ^Context context)))
    (enable-dynamic-compilation context)
    ;; Ensure that `:port` is provided, pass all other arguments as-is.
    (start-nrepl-server port (mapcat identity (dissoc args :port)))
    (neko.threading/init-threading)
    (enable-compliment-sources)
    (reset! initialized? true)))
