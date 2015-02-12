(ns neko.notify
  "Provides convenient wrappers for Toast and Notification APIs."
  (:use [neko.context :only [context]])
  (:import android.content.Context android.widget.Toast
           android.app.Notification android.content.Intent
           android.app.PendingIntent android.app.NotificationManager))

;; ### Toasts

(defn toast
  "Creates a Toast object using a text message and a keyword representing how
  long a toast should be visible (`:short` or `:long`). Two-argument version
  takes only message and assumes length to be :long."
  {:forms '([context message] [context message length])}
  ([message]
     (println "One-argument version is deprecated. Please use (toast context message)")
     (toast context message :long))
  ([arg1 arg2]
     (if (instance? Context arg1)
       (toast arg1 arg2 :long)
       (do (println "Context-less version is deprecated. Please use (toast context message length)")
           (toast context arg1 arg2))))
  ([^Context context, ^String message, length]
   {:pre [(or (= length :short) (= length :long))]}
   (.show
    ^Toast (Toast/makeText context message ^int (case length
                                                  :short Toast/LENGTH_SHORT
                                                  :long Toast/LENGTH_LONG)))))

;; ### Notifications

(def ^:private default-notification-icon (atom nil))

(defn set-default-notification-icon! [icon]
  (reset! default-notification-icon icon))

(defn- ^NotificationManager notification-manager
  "Returns the notification manager instance."
  [^Context context]
  (.getSystemService context Context/NOTIFICATION_SERVICE))

(defn construct-pending-intent
  "Creates a PendingIntent instance from a vector where the first
  element is a keyword representing the action type, and the second
  element is a action string to create an Intent from."
  ([context [action-type, ^String action]]
     (let [^Intent intent (Intent. action)]
       (case action-type
         :activity (PendingIntent/getActivity context 0 intent 0)
         :broadcast (PendingIntent/getBroadcast context 0 intent 0)
         :service (PendingIntent/getService context 0 intent 0)))))

(defn notification
  "Creates a Notification instance. If icon is not provided uses the
  default notification icon."
  [& args]
  (let [[context {:keys [icon ticker-text when content-title content-text action]
                  :or {icon @default-notification-icon, when (System/currentTimeMillis)}}]
        (if (instance? Context (first args))
          [(first args) (apply hash-map (rest args))]
          [context (apply hash-map args)])
        notification (Notification. icon ticker-text when)]
    (.setLatestEventInfo notification context content-title content-text
                         (construct-pending-intent context action))
    notification))

;; This atom stores the mapping of keywords to integer IDs that
;; represent the notification IDs.
;;
(def ^:private notification-ids (atom {}))

;; A simple counter that will increment by one after each call.
;;
(def ^:private new-id
  (let [ctr (atom 0)]
    (fn []
      (swap! ctr inc)
      @ctr)))

(defn fire
  "Sends the notification to the status bar. ID is optional and could be
  either an integer or a keyword."
  {:forms '([context notification] [context id notification])}
  ([notification]
     (println "One-argument version is deprecated. Please use (fire context notification)")
     (.notify (notification-manager context) (new-id) notification))
  ([arg1 arg2]
     (if (instance? Context arg1)
       (.notify (notification-manager arg1) (new-id) arg2)
       (do (println "Context-less version is deprecated. Please use (fire context id notification)")
           (fire context arg1 arg2))))
  ([context id notification]
     (let [id (if (keyword? id)
                (if (contains? @notification-ids id)
                  (@notification-ids id)
                  (let [number-id (new-id)]
                    (swap! notification-ids assoc id number-id)
                    number-id))
                id)]
       (.notify (notification-manager context) id notification))))

(defn cancel
  "Removes a notification by the given ID from the status bar."
  {:forms '([context id])}
  ([id]
     (println "One-argument version is deprecated. Please use (cancel context id)")
     (cancel context id))
  ([context id]
     (let [id (if (keyword? id)
                (@notification-ids id)
                id)]
       (.cancel (notification-manager context) id))))
