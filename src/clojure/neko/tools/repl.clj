(ns neko.tools.repl
  (:require neko.log)
  (:import android.content.Context
           java.util.concurrent.atomic.AtomicLong
           java.util.concurrent.ThreadFactory
           java.io.FileNotFoundException))

(def cider-middleware
  "A vector containing all CIDER middleware."
  '[cider.nrepl.middleware.apropos/wrap-apropos
    cider.nrepl.middleware.classpath/wrap-classpath
    cider.nrepl.middleware.complete/wrap-complete
    cider.nrepl.middleware.info/wrap-info
    cider.nrepl.middleware.inspect/wrap-inspect
    cider.nrepl.middleware.macroexpand/wrap-macroexpand
    cider.nrepl.middleware.ns/wrap-ns
    cider.nrepl.middleware.resource/wrap-resource
    cider.nrepl.middleware.stacktrace/wrap-stacktrace
    cider.nrepl.middleware.test/wrap-test
    cider.nrepl.middleware.trace/wrap-trace
    cider.nrepl.middleware.undef/wrap-undef])

(defn cider-available? []
  (try (require 'cider.nrepl.version)
       true
       (catch FileNotFoundException e false)))

(defn android-thread-factory
  "Returns a new ThreadFactory with increased stack size. It is used to
  substitute nREPL's native `configure-thread-factory` on Android platform."
  []
  (let [counter (AtomicLong. 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. (.getThreadGroup (Thread/currentThread))
                       runnable
                       (format "nREPL-worker-%s" (.getAndIncrement counter))
                       1048576) ;; Hardcoded stack size of 1Mb
          (.setDaemon true))))))

(defmacro stubify [ns-sym & empty-fns]
  `(let [curr-ns# (ns-name *ns*)]
     (ns ~ns-sym)
     ~@(map (fn [f] `(defn ~f [~'& ~'_])) empty-fns)
     (in-ns curr-ns#)))

(defn- patch-unsupported-dependencies
  "Some non-critical CIDER and nREPL dependencies cannot be used on Android
  as-is, so they have to be tranquilized."
  []
  (stubify dynapath.util add-classpath! addable-classpath))

(defn start-repl
  "Starts a remote nREPL server. Creates a `user` namespace because nREPL
  expects it to be there while initializing. References nrepl's `start-server`
  function on demand because the project can be compiled without nrepl
  dependency."
  [middleware & repl-args]
  (binding [*ns* (create-ns 'user)]
    (refer-clojure)
    (patch-unsupported-dependencies)
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

(defmacro start-nrepl-server
  "Expands into nREPL server initialization if conditions are met."
  [port other-args]
  (when (or (not (:neko.init/release-build *compiler-options*))
            (:neko.init/start-nrepl-server *compiler-options*))
    (let [build-port (:neko.init/nrepl-port *compiler-options*)
          mware (when (cider-available?)
                  (list `quote
                        (or (:neko.init/nrepl-middleware *compiler-options*)
                            cider-middleware)))]
      `(let [port# (or ~port ~build-port 9999)]
         (try (apply start-repl ~mware :port port# ~other-args)
              (neko.log/i "Nrepl started at port" port#)
              (catch Exception ex#
                (neko.log/e "Failed to start nREPL" :exception ex#)))))))
