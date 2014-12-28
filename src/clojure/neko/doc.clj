(ns neko.doc
  "This namespace contains functions that help the developer with
  documentation for different parts of neko."
  (:require [neko.ui.traits :as traits]
            [neko.ui.mapping :as mapping])
  (:use [clojure.string :only [join]]))

(defn get-trait-doc
  "Returns a docstring for the given trait keyword."
  [trait]
  (when-let [doc (get-in (meta #'traits/apply-trait)
                            [:trait-doc trait])]
    (str trait " - " doc)))

(defn get-element-doc
  "Returns a docsting generated from the element mapping. Verbose flag
  switches the detailed description of element's traits."
  [el-type el-mapping verbose?]
  (let [{:keys [classname attributes values]} el-mapping
        traits (mapping/all-traits el-type)]
   (format "%s - %s\n%s%s%s\n"
           el-type
           (or (and classname (.getName ^Class classname))
               "no matching class")
           (if (empty? traits) ""
               (format "Implements traits: %s\n"
                       (if verbose?
                         (join (for [t traits]
                                 (str "\n" (get-trait-doc t))))
                         (join (map (partial str "\n  ") traits)))))
           (if (empty? attributes) ""
               (format "Default attributes: %s\n"
                       (pr-str attributes)))
           (if (empty? values) ""
               (format "Special values: %s\n"
                       (pr-str values))))))

(defn describe
  "Describes the given keyword. If it reprenents UI element's name
  then describe the element. If optional second argument equals
  `:verbose`, describe all its traits as well. If a trait keyword is
  given, describes the trait. No-arguments version briefly describes
  all available UI elements."
  ([]
     (let [all-elements (mapping/get-keyword-mapping)]
       (doseq [[el-type parameters] all-elements]
         (print (get-element-doc el-type parameters false)))))
  ([kw]
     (describe kw nil))
  ([kw modifier]
     (let [parameters ((mapping/get-keyword-mapping) kw)
           trait-doc (get-trait-doc kw)]
       (cond
        parameters (print "Elements found:\n"
                          (get-element-doc kw parameters (= modifier :verbose)))
        trait-doc (print "\nTraits found:\n" trait-doc)
        :else (print (str "No elements or traits were found for " kw))))))
