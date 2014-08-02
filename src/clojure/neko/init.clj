; Copyright © 2012 Alexander Yakushev.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.init
  "Contains functions for neko initialization and setting runtime options."
  (:require [neko context log resource compilation threading])
  (:import android.content.Context
           java.util.concurrent.atomic.AtomicLong
           java.util.concurrent.ThreadFactory))

(defmacro
  ^{:private true
    :doc "Expands into dynamic compilation initialization if
    conditions are met."}
  enable-dynamic-compilation
  [context classes-dir]
  (when (or (not (::release-build *compiler-options*))
            (::start-nrepl-server *compiler-options*)
            (::enable-dynamic-compilation *compiler-options*))
    `(neko.compilation/init ~context ~classes-dir)))

(defn android-thread-factory
  "Returns a new ThreadFactory with increased stack size. It is used
  to substitute nREPL's native `configure-thread-factory` on Android
  platform."
  []
  (let [counter (AtomicLong. 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. (.getThreadGroup (Thread/currentThread))
                       runnable
                       (format "nREPL-worker-%s" (.getAndIncrement counter))
                       1048576) ;; Hardcoded stack size of 1Mb
          (.setDaemon true))))))

(defn start-repl
  "Starts a remote nREPL server. Creates a `user` namespace because
  nREPL expects it to be there while initializing. References nrepl's
  `start-server` function on demand because the project can be
  compiled without nrepl dependency."
  [middleware & repl-args]
  (binding [*ns* (create-ns 'user)]
    (refer-clojure)
    (use 'clojure.tools.nrepl.server)
    (require '[clojure.tools.nrepl.middleware.interruptible-eval :as ie])
    (with-redefs-fn {(resolve 'ie/configure-thread-factory)
                     android-thread-factory}
      #(apply (resolve 'start-server)
              :handler (apply (resolve 'default-handler)
                              (map (fn [sym]
                                     (require (symbol (namespace sym)))
                                     (resolve sym))
                                   middleware))
              repl-args))))

(defmacro
  ^{:private true
    :doc "Expands into nREPL server initialization if conditions are met."}
  start-nrepl-server
  [port other-args]
  (when (or (not (::release-build *compiler-options*))
            (::start-nrepl-server *compiler-options*))
    (let [build-port (::nrepl-port *compiler-options*)
          mware (list `quote (::nrepl-middleware *compiler-options*))]
      `(let [port# (or ~port ~build-port 9999)]
         (apply start-repl ~mware :port port# ~other-args)
         (neko.log/i "Nrepl started at port" port#)))))

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
  [context & {:keys [classes-dir port] :or {classes-dir "classes"}
              :as args}]
  (when-not @initialized?
    (alter-var-root #'neko.context/context (constantly context))
    (alter-var-root #'neko.resource/package-name
                    (constantly (.getPackageName ^Context context)))
    (enable-dynamic-compilation context classes-dir)
    ;; Ensure that `:port` is provided, pass all other arguments as-is.
    (start-nrepl-server port (mapcat identity (dissoc args :classes-dir :port)))
    (neko.threading/init-threading)
    (enable-compliment-sources)
    (reset! initialized? true)))
