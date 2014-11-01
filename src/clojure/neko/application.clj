(ns neko.application
  "Contains tools to create and manipulate Application instances. This
  namespace is deprecated and exists only for backward compatibility
  purposes."
  (:require neko.init)
  (:import android.app.Application
           android.content.Context))

(defmacro defapplication
  [& args]
  (throw (Exception. "defapplication is deprecated, please define
  Application class from Java. Default `:on-create` moved to
  `init-application`.")))

(defn init-application
  "DEPRECATED: Performs necessary preparations for Neko and REPL
  development. You should call `neko.init/init` instead."
  [context]
  (neko.init/init context))
