; Copyright © 2011 Sattvik Software & Technology Resources, Ltd. Co.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.threading
  "Utilities used to manage multiple threads on Android."
  {:author "Daniel Solano Gómez"}
  (:use [neko.debug :only [safe-for-ui]])
  (:import android.app.Activity
           android.view.View
           clojure.lang.IFn
           java.util.concurrent.TimeUnit
           android.os.Looper
           android.os.Handler))

;; ### UI thread utilities

(defn on-ui-thread?
  "Returns true if the current thread is a UI thread."
  []
  (identical? (Thread/currentThread)
              (.getThread ^Looper (Looper/getMainLooper))))

(defn on-ui*
  "Runs the given nullary function on the UI thread.  If this function is
  called on the UI thread, it will evaluate immediately."
  [f]
  (if (on-ui-thread?)
    (f)
    (.post (Handler. (Looper/getMainLooper)) (fn [] (safe-for-ui (f))))))

(defmacro on-ui
  "Runs the macro body on the UI thread.  If this macro is called on the UI
  thread, it will evaluate immediately."
  [& body]
  `(on-ui* (fn [] ~@body)))

(defn ^{:deprecated "3.1.0"} post*
  "Causes the function to be added to the message queue.  The function will
  execute on the UI thread.  Returns true if successfully placed in the message
  queue."
  [^View view, f]
  (.post view f))

(defmacro post
  "Causes the macro body to be added to the message queue.  It will execute on
  the UI thread.  Returns true if successfully placed in the message queue."
  [view & body]
  `(post* ~view (fn [] ~@body)))

(defn ^{:deprecated "3.1.0"} post-delayed*
  "Causes the function to be added to the message queue, to be run after the
  specified amount of time elapses.  The function will execute on the UI
  thread.  Returns true if successfully placed in the message
  queue."
  [^View view, millis f]
  (.postDelayed view f millis))

(defmacro post-delayed
  "Causes the macro body to be added to the message queue.  It will execute on
  the UI thread.  Returns true if successfully placed in the message queue."
  [view millis & body]
  `(post-delayed* ~view ~millis (fn [] ~@body)))

(def ^{:doc "A map of unit keywords to TimeUnit instances."
       :private true}
  unit-map
  {; days/hours/minutes added in API level 9
   ;:days    TimeUnit/DAYS
   ;:hours   TimeUnit/HOURS
   ;:minutes TimeUnit/MINUTES
   :seconds TimeUnit/SECONDS
   :millis  TimeUnit/MILLISECONDS
   :micros  TimeUnit/MICROSECONDS
   :nanos   TimeUnit/NANOSECONDS})
