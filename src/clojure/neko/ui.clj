(ns neko.ui
  "Tools for defining and manipulating Android UI elements."
  (:use [neko.-utils :only [keyword->setter reflect-setter reflect-constructor]]
        [neko.listeners.view :only [on-click-call]]
        [neko.ui.traits :only [apply-trait]])
  (:require [neko.ui.mapping :as kw]
            [neko.context :as context]))

;; ## Attributes

(defn apply-default-setters-from-attributes
  "Takes widget keywords name, UI widget object and attributes map
  after all custom attributes were applied. Transforms each attribute
  into a call to (.set_CapitalizedKey_ widget value). If value is a
  keyword then it is looked up in the keyword-mapping or if it is not
  there, it is perceived as a static field of the class."
  [widget-kw widget attributes]
  (doseq [[attribute value] attributes]
    (let [real-value (kw/value widget-kw value attribute)]
      (.invoke (reflect-setter (type widget)
                               (keyword->setter attribute)
                               (type real-value))
               widget (into-array (vector real-value))))))

(defn apply-attributes
  "Takes UI widget keyword, a widget object, a map of attributes and
  options. Consequently calls `apply-trait` on all element's traits,
  in the end calls `apply-default-setters-from-attributes` on what is
  left from the attributes map. Returns the updated options map.

  Options is a map of additional arguments that come from container
  elements to their inside elements. Note that all traits of the
  current element will receive the initial options map, and
  modifications will only appear visible to the subsequent elements."
  [widget-kw widget attributes options]
  (loop [[trait & rest] (kw/all-traits widget-kw),
         attrs attributes, new-opts options]
    (if trait
      (let [[attributes-fn options-fn]
            (apply-trait trait widget attrs options)]
        (recur rest (attributes-fn attrs) (options-fn new-opts)))
      (do
        (apply-default-setters-from-attributes widget-kw widget attrs)
        new-opts))))

;; ## Widget creation

(defn construct-element
  "Constructs a UI widget by a given keyword. Infers a correct
  constructor for the types of arguments being passed to it."
  ([kw context constructor-args]
     (let [element-class (kw/classname kw)]
       (.newInstance (reflect-constructor element-class
                                          (cons android.content.Context
                                                (map type constructor-args)))
                     (to-array (cons context constructor-args))))))

(defn make-ui-element
  "Creates a UI widget based on its keyword name, applies attributes
  to it, then recursively create its subelements and add them to the
  widget."
  [context tree options]
  (if (sequential? tree)
    (let [[widget-kw attributes & inside-elements] tree
          _ (assert (and (keyword? widget-kw) (map? attributes)))
          attributes (merge (kw/default-attributes widget-kw) attributes)
          wdg (if-let [constr (:custom-constructor attributes)]
                (apply constr context (:constructor-args attributes))
                (construct-element widget-kw context
                                   (:constructor-args attributes)))
          new-opts (apply-attributes
                    widget-kw wdg
                    ;; Remove :custom-constructor and
                    ;; :constructor-args since they are not real
                    ;; attributes.
                    (dissoc attributes :constructor-args :custom-constructor)
                    options)]
      (doseq [element inside-elements :when element]
        (.addView ^android.view.ViewGroup wdg
                  (make-ui-element context element new-opts)))
      wdg)
    tree))

(defn make-ui
  "Takes an activity instance, and a tree of elements and creates Android UI
  elements according to this tree. A tree has a form of a vector that looks like
  following:

  `[element-name map-of-attributes & subelements]`."
  {:forms '([activity tree])}
  ([tree]
     (println "One-argument version is deprecated. Please use (make-ui activity tree)")
     (make-ui-element context/context tree {}))
  ([activity tree]
     (make-ui-element activity tree {})))

(defn config
  "Takes a widget and key-value pairs of attributes, and applies these
  attributes to the widget."
  [widget & {:as attributes}]
  (apply-attributes (kw/keyword-by-classname (type widget))
                    widget attributes {}))
