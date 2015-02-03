(ns neko.-utils
  "Internal utilities used by Neko, not intended for external consumption."
  (:require [clojure.string :as string])
  (:import [java.lang.reflect Method Constructor Field]))

(defmacro memoized
  "Takes a `defn` definition and memoizes it preserving its metadata."
  [func-def]
  (let [fn-name (second func-def)]
    `(do ~func-def
         (let [meta# (meta (var ~fn-name))]
           (def ~fn-name (memoize ~fn-name))
           (reset-meta! (var ~fn-name) meta#)))))

(defn simple-name
  "Takes a possibly package-qualified class name symbol and returns a
  simple class name from it."
  [full-class-name]
  (nth (re-find #"(.*\.)?(.+)" (str full-class-name)) 2))

(defn capitalize
  "Takes a string and upper-cases the first letter in it."
  [s]
  (str (.toUpperCase (subs s 0 1)) (subs s 1)))

(defn unicaseize
  "Takes a string lower-cases the first letter in it."
  [s]
  (str (.toLowerCase (subs s 0 1)) (subs s 1)))

(memoized
 (defn keyword->static-field
   "Takes a keyword and transforms it into a static field name.

   All letters in keyword are capitalized, and all dashes are replaced
   with underscores."
   [kw]
   (.toUpperCase (string/replace (name kw) \- \_))))

(defn keyword->camelcase
  "Takes a keyword and transforms its name into camelCase."
  [kw]
  (let [[first & rest] (string/split (name kw) #"-")]
    (string/join (cons first (map capitalize rest)))))

(memoized
 (defn keyword->setter
   "Takes a keyword and transforms it into a setter method name.

   Transforms keyword name into camelCase, capitalizes the first
   character and appends \"set\" at the beginning."
   [kw]
   (->> (keyword->camelcase kw)
        capitalize
        (str "set"))))

(defmacro call-if-nnil
  "Expands into check whether function is defined, then executes it
  and returns true or just returns false otherwise."
  [f & arguments]
  `(if ~f
     (do (~f ~@arguments)
         true)
     false))

;; Reflection functions

(defn class-or-type [cl]
  (condp = cl
    Boolean Boolean/TYPE
    Integer Integer/TYPE
    Long Integer/TYPE
    Double Double/TYPE
    Float Float/TYPE
    Character Character/TYPE
    cl))

(defn ^Method reflect-setter
  "Returns a Method object for the given UI object class, method name
  and the first argument type."
  [^Class widget-type, ^String method-name, ^Class value-type]
  (if-not (= widget-type Object)
    (let [value-type (class-or-type value-type)
          all-value-types (cons value-type (supers value-type))]
      (loop [[t & r] all-value-types]
        (if t
          (if-let [method (try
                            (.getDeclaredMethod widget-type method-name
                                                (into-array Class [t]))
                            (catch NoSuchMethodException e nil))]
            method
            (recur r))
          (reflect-setter (.getSuperclass widget-type)
                          method-name value-type))))
    (throw
     (NoSuchMethodException. (format "Couldn't find method .%s for argument %s)"
                                     method-name (.getName value-type))))))

(defn ^Constructor reflect-constructor
  "Returns a Constructor object for the given UI object class "
  [^Class widget-type constructor-arg-types]
  (.getConstructor widget-type (into-array Class constructor-arg-types)))

(defn reflect-field
  "Returns a field value for the given UI object class and the name of
  the field."
  [^Class widget-type, ^String field-name]
  (.get ^Field (.getDeclaredField widget-type field-name) nil))
