(ns neko.data
  "Contains utilities to manipulate data that is passed between
  Android entities via Bundles and Intents."
  (:refer-clojure :exclude [assoc!])
  (:use [neko.context :only [context]])
  (:import android.os.Bundle android.content.Intent
           android.content.SharedPreferences
           android.content.SharedPreferences$Editor
           android.content.Context))

(defprotocol GenericExtrasKey
  "If given a string returns itself, otherwise transforms a argument
  into a string."
  (generic-key [key]))

(extend-protocol GenericExtrasKey
  String
  (generic-key [s] s)

  clojure.lang.Keyword
  (generic-key [k] (.getName k)))

;; This type acts as a wrapper around Bundle instance to be able to
;; access it like an ordinary map.
;;
(deftype MapLikeBundle [^Bundle bundle]
  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey bundle (generic-key k)))
  (entryAt [this k]
    (clojure.lang.MapEntry. k (.get bundle (generic-key k))))
  (valAt [this k]
    (.get bundle (generic-key k)))
  (valAt [this k default]
    (let [key (generic-key k)]
      (if (.containsKey bundle key)
        (.get bundle (generic-key key))
        default)))
  (seq [this]
    (map (fn [k] [k (.get bundle k)])
         (.keySet bundle))))

;; This type wraps a HashMap just redirecting the calls to the
;; respective HashMap methods. The only useful thing it does is
;; allowing to use keyword keys instead of string ones.
;;
(deftype MapLikeHashMap [^java.util.HashMap hmap]
  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey hmap (generic-key k)))
  (entryAt [this k]
    (clojure.lang.MapEntry. k (.get hmap (generic-key k))))
  (valAt [this k]
    (.get hmap (generic-key k)))
  (valAt [this k default]
    (let [key (generic-key k)]
      (if (.containsKey hmap key)
        (.get hmap (generic-key key))
        default)))
  (seq [this]
    (map (fn [k] [k (.get hmap k)])
         (.keySet hmap))))

(defprotocol MapLike
  "A protocol that helps to wrap objects of different types into
  MapLikeBundle."
  (like-map [this]))

(extend-protocol MapLike
  Bundle
  (like-map [b]
    (MapLikeBundle. b))

  Intent
  (like-map [i]
    (if-let [bundle (.getExtras i)]
      (MapLikeBundle. bundle)
      {}))

  SharedPreferences
  (like-map [sp]
    (MapLikeHashMap. (.getAll sp)))

  nil
  (like-map [_] {}))

;; SharedPreferences utilities

(def ^:private sp-access-modes {:private Context/MODE_PRIVATE
                                :world-readable Context/MODE_WORLD_READABLE
                                :world-writeable Context/MODE_WORLD_WRITEABLE})

(defn get-shared-preferences
  "Returns the SharedPreferences object for the given name. Possible modes:
  `:private`, `:world-readable`, `:world-writeable`."
  {:forms '([context name mode])}
  ([name mode]
     (println "Two-argument version is deprecated. Please use (get-shared-preferences context name mode)"))
  ([^Context context name mode]
     {:pre [(or (number? mode) (contains? sp-access-modes mode))]}
     (let [mode (if (number? mode)
                  mode (sp-access-modes mode))]
       (.getSharedPreferences context name mode))))

(defn ^SharedPreferences$Editor assoc!
  "Puts the value into the SharedPreferences editor instance. Accepts
  limited number of data types supported by SharedPreferences."
  [^SharedPreferences$Editor sp-editor, key value]
  (let [key (generic-key key)]
    (condp #(= (type %2) %1) value
      java.lang.Boolean (.putBoolean sp-editor key value)
      java.lang.Float  (.putFloat sp-editor key value)
      java.lang.Double (.putFloat sp-editor key (float value))
      java.lang.Integer (.putInt sp-editor key value)
      java.lang.Long    (.putLong sp-editor key value)
      java.lang.String (.putString sp-editor key value)
      ;; else
      (throw (Exception. (str "SharedPreferences doesn't support type: "
                              (type value)))))))

(defn ^SharedPreferences$Editor assoc-arbitrary!
  "Puts `value` of an arbitrary Clojure data type into given
  SharedPreferences editor instance. Data is printed into a string and
  stored as a string value."
  [^SharedPreferences$Editor sp-editor key value]
  (let [key (generic-key key)]
    (.putString sp-editor key (pr-str value))))

(defn get-arbitrary
  "Gets a string by given key from a SharedPreferences
  HashMap (wrapped with `like-map`) and transforms it into a data
  value using Clojure reader."
  [sp-map key]
  (when-let [val (get sp-map key)]
   (read-string val)))
