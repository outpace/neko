(ns neko.find-view
  "Home of the ViewFinder protocol, which should simplify and unify
  obtaining UI elements by their :id trait. To use the protocol an
  object has to keep a mapping of IDs to UI widgets. Currently the
  protocol is supported by Activity and View objects."
  (:require [neko.activity :refer [get-decor-view]])
  (:import android.view.View
           android.app.Activity))

(defn- nil-or-view?
  [x]
  (or (nil? x)
      (instance? android.view.View x)))

(defprotocol ViewFinder
  "Protocol for finding child views by their `:id` trait."
  (find-view [container id]))

(extend-protocol ViewFinder
  View
  (find-view [^View view, id]
    {:post [(nil-or-view? %)]}
    (get (.getTag view) id))

  Activity
  (find-view [^Activity activity, id]
    {:post [(nil-or-view? %)]}
    (find-view (get-decor-view activity) id)))

(defn find-views
  "Same as `find-view`, but takes a variable number of IDs and returns
a vector of found views."
  [container & ids]
  (map (partial find-view container) ids))
