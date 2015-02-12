(ns neko.ui.mapping
  "This namespace provides utilities to connect the keywords to the
  actual UI classes, define the hierarchy relations between the
  elements and the values for the keywords representing values."
  (:require [clojure.string :as string])
  (:use [neko.-utils :only [keyword->static-field reflect-field]])
  (:import [android.widget LinearLayout Button EditText ListView SearchView
            ImageView ImageView$ScaleType RelativeLayout]
           android.app.ProgressDialog
           [android.view View ViewGroup$LayoutParams Gravity]))

;; This atom keeps all the relations inside the map.
(def ^{:private true} keyword-mapping
  (atom
   ;; UI widgets
   {:view {:traits [:def :id :padding :on-click :on-long-click :on-touch
                    :on-create-context-menu :on-key
                    :default-layout-params :linear-layout-params
                    :relative-layout-params :listview-layout-params]
           :value-namespaces
           {:text-alignment View
            :text-direction View
            :visibility View}}
    :view-group {:inherits :view
                 :traits [:container :id-holder]}
    :button {:classname android.widget.Button
             :inherits :text-view
             :attributes {:text "Default button"}}
    :linear-layout {:classname android.widget.LinearLayout
                    :inherits :view-group
                    :value-namespaces
                    {:gravity android.view.Gravity}}
    :relative-layout {:classname android.widget.RelativeLayout
                      :inherits :view-group}
    :edit-text {:classname android.widget.EditText
                :inherits :view}
    :text-view {:classname android.widget.TextView
                :inherits :view
                :value-namespaces
                {:ellipsize android.text.TextUtils$TruncateAt}
                :traits [:text :text-size :on-editor-action]}
    :list-view {:classname android.widget.ListView
                :inherits :view-group
                :traits [:on-item-click]}
    :search-view {:classname android.widget.SearchView
                  :inherits :view-group
                  :traits [:on-query-text]}
    :image-view {:classname android.widget.ImageView
                 :inherits :view
                 :traits [:image]
                 :value-namespaces
                 {:scale-type android.widget.ImageView$ScaleType}}
    :web-view {:classname android.webkit.WebView
               :inherits :view}

    ;; Other
    :layout-params {:classname ViewGroup$LayoutParams
                    :values {:fill ViewGroup$LayoutParams/FILL_PARENT
                             :wrap ViewGroup$LayoutParams/WRAP_CONTENT}
                    :value-namespaces
                    {:gravity android.view.Gravity}}
    :progress-dialog {:classname android.app.ProgressDialog
                      :values {:horizontal ProgressDialog/STYLE_HORIZONTAL
                               :spinner ProgressDialog/STYLE_SPINNER}}
    }))

(defn get-keyword-mapping
  "Returns the current state of `keyword-mapping`."
  []
  @keyword-mapping)

(def ^{:private true} reverse-mapping
  (atom
   {android.widget.Button :button
    android.widget.LinearLayout :linear-layout
    android.widget.RelativeLayout :relative-layout
    android.widget.EditText :edit-text
    android.widget.TextView :text-view
    android.widget.ListView :list-view
    android.app.ProgressDialog :progress-dialog}))

(defn set-classname!
  "Connects the given keyword to the classname."
  [kw classname]
  (swap! keyword-mapping assoc-in [kw :classname] classname)
  (swap! reverse-mapping assoc-in classname kw))

(defn classname
  "Gets the classname from the keyword-mapping map if the argument is
  a keyword. Otherwise considers the argument to already be a
  classname."
  [classname-or-kw]
  (if (keyword? classname-or-kw)
    (or (get-in @keyword-mapping [classname-or-kw :classname])
        (throw (Exception. (str "The class for " classname-or-kw
                                " isn't present in the mapping."))))
    classname-or-kw))

(defn keyword-by-classname
  "Returns a keyword name for the given UI widget classname."
  [classname]
  (@reverse-mapping classname))

(defn add-trait!
  "Defines the `kw` to implement trait specified with `trait-kw`."
  [kw trait-kw]
  (swap! keyword-mapping update-in [kw :traits] conj trait-kw))

(defn all-traits
  "Returns the list of all unique traits for `kw`. The list is built
  recursively."
  [kw]
  (let [own-traits (get-in @keyword-mapping [kw :traits])
        parent (get-in @keyword-mapping [kw :inherits])]
    (concat own-traits (when parent
                         (all-traits parent)))))

(defn set-value!
  "Associate the value keyword with the provided value for the given
  keyword representing the UI element."
  [element-kw value-kw value]
  (swap! keyword-mapping assoc-in [element-kw :values value-kw] value))

(defn- recursive-find
  "Searches in the keyword mapping for a value denoted by a list of
  keys. If value is not found, tries searching in a parent."
  [[element-kw & other :as keys]]
  (if element-kw
    (or (get-in @keyword-mapping keys)
        (recur (cons (get-in @keyword-mapping [element-kw :inherits]) other)))))

(defn value
  "If the value is a keyword then returns the value for it from the
  keyword-mapping. The value is sought in the element itself and all
  its parents. If the value-keyword isn't present in any element's
  keyword-mapping, form the value as
  `classname-for-element-kw/CAPITALIZED-VALUE-KW`. Classname for
  keyword can be extracted from :value-namespaces map for element's
  mapping."
  [element-kw value & [attribute]]
  (let [mapping @keyword-mapping]
    (if-not (keyword? value)
      (cond
       (integer? value) (int value)
       (float? value) (float value)
       :else value)
      (or (recursive-find (list element-kw :values value))
          (reflect-field
           (classname
            (or (and attribute
                     (recursive-find (list element-kw
                                           :value-namespaces attribute)))
                element-kw))
           (keyword->static-field value))))))

(defn add-default-atribute-value!
  "Adds a default attribute value for the given element."
  [element-kw attribute-kw value]
  (swap! keyword-mapping
         update-in [element-kw :attributes attribute-kw] value))

(defn default-attributes
  "Returns a map of default attributes for the given element keyword
  and all its parents."
  [element-kw]
  (merge (when element-kw
           (default-attributes (get-in @keyword-mapping
                                       [element-kw :inherits])))
         (get-in @keyword-mapping [element-kw :attributes])))

(defn defelement
  "Defines the element of the given class with the provided name to
  use in the UI construction. Takes the element's classname, a parent
  it inherits, a list of traits and a map of specific values as
  optional arguments.

  Optional arguments
  - :classname, :inherits, :traits, :values, :attributes, :container-type."
  [kw-name & {:as args}]
  (swap! keyword-mapping assoc kw-name
         (if-not (contains? args :inherits)
           (assoc args :inherits :view)
           args))
  (if-let [classname (:classname args)]
    (swap! reverse-mapping assoc classname kw-name)))
