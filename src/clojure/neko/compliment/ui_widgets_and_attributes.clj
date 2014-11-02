(ns neko.compliment.ui-widgets-and-attributes
  "Compliment source for keywords that represent UI widget keywords
  and their traits."
  (:require [neko.ui.mapping :as mapping]
            [neko.ui.traits :as traits]
            [neko.doc :as doc]
            [neko.resource :as droid-res]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn keyword-symbol?
  "Tests if prefix is a keyword."
  [x]
  (re-matches #":.*" x))

(defn process-context
  "Checks if the context is a widget definition, attribute map
  definition or neither. If context is an attribute map, tries finding
  current widget's name."
  [context]
  (let [[level1 level2] context]
    (cond (and (vector? (:form level1)) (= (:idx level1) 0))
          {:type :widget}

          (and (map? (:form level1)) (= (:map-role level1) :key))
          {:type :attr,
           :widget (when (and (vector? (:form level2))
                              (= (:idx level2) 1))
                     (let [w-name (first (:form level2))]
                       (when (and (keyword? w-name)
                                  ((mapping/get-keyword-mapping) w-name))
                         w-name)))})))

;; ## Candidates

(defn get-widget-attributes
  "Returns a list all possible attributes for the given widget keyword."
  [widget-kw]
  (let [attributes (:attributes (meta #'neko.ui.traits/apply-trait))]
    (if widget-kw
      (let [all-traits (mapping/all-traits widget-kw)]
        (for [[att w-traits] attributes
              :when (not (empty? (set/intersection w-traits (set all-traits))))]
          att))
      (keys attributes))))

(defn candidates
  "Returns a list of widget keywords or attribute keywords depending
  on context."
  [^String prefix, ns context]
  (when (keyword-symbol? prefix)
    (let [ctx (process-context context)
          cands (cond
                 (nil? ctx) []
                 (= (:type ctx) :widget) (keys (mapping/get-keyword-mapping))
                 (= (:type ctx) :attr) (get-widget-attributes (:widget ctx)))]
      (for [^String kw-str (map str cands)
            :when (.startsWith kw-str prefix)]
        kw-str))))

;; ## Documentation

(defn doc
  "Tries to get a docstring for the given completion candidate."
  [^String symbol-str, ns]
  (when (keyword-symbol? symbol-str)
    (let [kw (keyword (subs symbol-str 1))
          kw-mapping (mapping/get-keyword-mapping)
          attributes (:attributes (meta #'neko.ui.traits/apply-trait))]
      (cond (kw-mapping kw)
            (doc/get-element-doc kw kw-mapping false)

            (attributes kw)
            (->> (attributes kw)
                 (map doc/get-trait-doc)
                 (interpose "\n\n")
                 string/join)))))

;; ## Source definition

(defn init-source
  "Initializes this completion source if Compliment is available."
  []
  (try (require 'compliment.core)
       ((resolve 'compliment.sources/defsource) ::neko-ui-keywords
        :candidates #'candidates
        :doc #'doc)
       (catch Exception ex nil)))
