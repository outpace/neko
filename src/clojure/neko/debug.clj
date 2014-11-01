(ns neko.debug
  "Contains useful tools to be used while developing the application."
  (:require [neko log notify]))

;; This atom stores the last exception happened on the UI thread.
;;
(def ^:private ui-exception (atom nil))

(defn handle-exception-from-ui-thread
  "Displays an exception message using a Toast and stores the
  exception for the future reference."
  [e]
  (reset! ui-exception e)
  (neko.log/e "Exception raised on UI thread." :exception e)
  (neko.notify/toast (str e) :long))

(defn ui-e
  "Returns an uncaught exception happened on UI thread."
  [] @ui-exception)

(defmacro catch-all-exceptions [func]
  (if (:neko.init/release-build *compiler-options*)
    `(~func)
    `(try (~func)
          (catch Throwable e#
            (handle-exception-from-ui-thread e#)))))

(defn safe-for-ui*
  "Wraps the given function inside a try..catch block and notify user
  using a Toast if an exception happens."
  [f]
  (catch-all-exceptions f))

(defmacro safe-for-ui
  "A conditional macro that will protect the application from crashing
  if the code provided in `body` crashes on UI thread in the debug
  build. If the build is a release one returns `body` as is."
  [& body]
  `(safe-for-ui* (fn [] ~@body)))
