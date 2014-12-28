(ns neko.resource
  "Provides utilities to resolve application resources."
  (:require [clojure.string :as string]
            [neko.context :as context])
  (:import android.content.Context android.graphics.drawable.Drawable))

;; ## Runtime resource resolution

(def package-name (:neko.init/package-name *compiler-options*))

(defn- kw-to-res-name
  "Takes the name of the keyword and turns all hyphens and periods to
  underscores."
  [kw]
  (-> (name kw)
      (string/replace \- \_)
      (string/replace \. \_)))

(defn get-resource
  "Resolves the resource ID of a given type with the given name.  For example,
  to refer to what in Java would be R.string.my_string, you can use:

    `(get-resource :string :my-string)`

  The type should be a keyword corresponding to a resource type such as
  :layout, :attr, or :id.

  The name should be a keyword.  If the keyword has a namespace, it will be
  used as the package from which to retrieve the resources.  Generally, this is
  not required as the default will be the application package.  However, this
  can be used to access the resources from the platform.  For example, the
  equivalent to android.R.layout.simple_list_item_1 is:

    `(get-resource :layout :android/simple-list-item-1)`

  The name portion of the name argument will be converted to a string and any
  hyphens or periods will be transformed to underscores.  Note that hyphens are
  not valid in Android names, but are allowed here to be Clojure friendly.

  If the name argument is an integer, it is assumed to be a valid resource ID
  and will be returned as is without any processing."
  ([res-type res-name]
     (println "Two-argument version is deprecated. Please use (get-resource context res-type res-name)")
     (get-resource context/context res-type res-name))
  ([^Context context, res-type res-name]
     (let [resid (if (keyword? res-name)
                      (.getIdentifier (.getResources context)
                                      (kw-to-res-name res-name)
                                      (name res-type)
                                      (or (namespace res-name)
                                          (.getPackageName context)))
                      res-name)]
       (if (= resid 0) nil resid))))

(defn get-id
  "Finds the ID for the XML item with the given name. This is simply a
  convenient way of calling `(get-resource context :id name)`."
  ([res-name]
     (println "One-argument version is deprecated. Please use (get-id context res-name)")
     (get-resource context/context :id res-name))
  ([^Context context, res-name]
     (get-resource context :id res-name)))

(defn get-layout
  "Finds the resource ID for the layout with the given name. This is simply a
  convenient way of calling `(get-resource context :layout name)`."
  ([res-name]
     (println "One-argument version is deprecated. Please use (get-layout context res-name)")
     (get-resource context/context :layout res-name))
  ([^Context context, res-name]
     (get-resource context :layout res-name)))

(defn get-string
  "Gets the localized string with the given ID or name from the context.
  The name will be resolved using get-resource. If res-name is a
  string, returns it unchanged.

  If additional arguments are supplied, the string will be interpreted as a
  format and the arguments will be applied to the format."
  {:forms '([context res-name & format-args?]) }
  [& args]
  (let [[^Context context args] (if (instance? Context (first args))
                                  [(first args) (rest args)]
                                  [context/context args])
        [res-name & format-args] args]
    (if (string? res-name)
      res-name
      (when-let [id (get-resource context :string res-name)]
        (if format-args
          (.getString context id (to-array format-args))
          (.getString context id))))))

(defn get-drawable
  "Gets a Drawable object associated with the given ID or name from
  the context. The name will be resolved using get-resource. If
  res-name is a Drawable, returns it unchanged."
  ([res-name]
     (println "One-argument version is deprecated. Please use (get-drawable context res-name)")
     (get-drawable context/context res-name))
  ([^Context context, res-name]
     (if (instance? Drawable res-name)
       res-name
       (when-let [id (get-resource context :drawable res-name)]
         (.getDrawable (.getResources context) id)))))

;; Compile time resource resolution

(defn- resource-symbol
  "Returns a symbol that represents a resource field specified by type
  and name keywords. If `name` is not a keyword, just returns it back."
  [res-type res-name]
  (if (not (keyword? res-name))
    name
    (let [package (or (namespace res-name) package-name)
          res-type (name res-type)
          res-name (kw-to-res-name res-name)]
      (symbol (str package ".R$" res-type "/" res-name)))))

(defmacro resolve-resource
  "Resolves a resource identifier by its type and name. This is the
  same as `get-resource`, but executes in compile-time."
  [type name]
  {:pre  [(keyword? type)]}
  (resource-symbol type name))

(defmacro resolve-id
  "Finds the resource ID for the XML item with the given name in
  compile time. This is simply a convenient way of calling
  `(resolve-resource :id name)`."
  [name]
  (resource-symbol :id name))

(defn resolve-id-reader
  [name]
  (resource-symbol :id name))

(defmacro resolve-string
  "Finds the resource ID for the string with the given name in compile
  time. This is simply a convenient way of calling `(resolve-resource
  :string name)`."
  [name]
  (resource-symbol :string name))

(defn resolve-string-reader
  [name]
  (resource-symbol :string name))

(defmacro resolve-layout
  "Finds the resource ID for the layout with the given name in compile
  time. This is simply a convenient way of calling `(resolve-resource
  :layout name)`."
  [name]
  (resource-symbol :layout name))

(defn resolve-layout-reader
  [name]
  (resource-symbol :layout name))

(defmacro resolve-drawable
  "Finds the resource ID for the Drawable with the given name in compile
  time. This is simply a convenient way of calling `(resolve-resource
  :drawable name)`."
  [name]
  (resource-symbol :drawable name))

(defn resolve-drawable-reader
  [name]
  (resource-symbol :drawable name))
