(ns neko.debug
  "Contains useful tools to be used while developing the application."
  (:require [neko log notify])
  (:import android.app.Activity
           android.view.WindowManager$LayoutParams
           java.util.WeakHashMap))

;;; Simplify REPL access to Activity objects.

(def ^WeakHashMap all-activities
  "Weak hashmap that contains mapping of namespaces or
  keywords to Activity objects."
  (WeakHashMap.))

(defmacro ^Activity *a
  "If called without arguments, returns the activity for the current
  namespace. A version with one argument will return the activity for
  the given object (be it a namespace or any other object)."
  ([]
   `(get all-activities '~(.name *ns*)))
  ([key]
   `(get all-activities ~key)))

;;; Exception handling

;; This atom stores the last exception happened on the UI thread.
;;
(def ^:private ui-exception (atom nil))

(defn handle-exception-from-ui-thread
  "Displays an exception message using a Toast and stores the
  exception for the future reference."
  [e]
  (reset! ui-exception e)
  (neko.log/e "Exception raised on UI thread." :exception e)
  (when-let [ctx (:neko.context/context all-activities)]
    (neko.notify/toast ctx (str e) :long)))

(defn ui-e
  "Returns an uncaught exception happened on UI thread."
  [] @ui-exception)

(defmacro safe-for-ui
  "A conditional macro that will protect the application from crashing
  if the code provided in `body` crashes on UI thread in the debug
  build. If the build is a release one returns `body` in a `do`-form."
  [& body]
  (if (:neko.init/release-build *compiler-options*)
    `(do ~@body)
    `(try ~@body
          (catch Throwable e# (handle-exception-from-ui-thread e#)))))

(defn safe-for-ui*
  "Wraps the given zero-argument function in `safe-for-ui` call and returns it,
  without executing."
  [f]
  (fn [] (safe-for-ui (f))))

(defmacro keep-screen-on
  "A conditional macro that will enforce the screen to stay on while the
  application is run in the debug mode."
  [^Activity activity]
  (if (:neko.init/release-build *compiler-options*)
    nil
    `(.addFlags (.getWindow ~activity)
                WindowManager$LayoutParams/FLAG_KEEP_SCREEN_ON)))


