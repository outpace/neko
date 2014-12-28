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

(ns neko.compilation
  "Utility functions for managing the compilation environment when using
  version of Clojure that supports dynamic compilation on the Dalvik virtual
  machine.

  To use this namespace, you need to call init with a context, such as an
  activity object.  This will create a cache directory where temporary files
  will be placed and will set the 'clojure.compile.path' system property and
  the '*compile-path*' var.  If the cache directory already exists, it will be
  cleaned out.

  Note that additional invocations to init within the same process will not have
  any effect."
  {:author "Daniel Solano Gómez"}
  (:use [neko.resource :only [get-resource]])
  (:import android.content.Context
           [java.io File FileNotFoundException]))

(def #^{:doc "Whether or not compilation has been initialized."
        :private true}
  cache-dir (atom nil))

(defn clear-cache
  "Clears all files from the cache directory."
  []
  (monitor-enter cache-dir)
  (try (when-let [^File dir @cache-dir]
         (doseq [^File f (.listFiles dir)]
           (.delete f)))
       (finally (monitor-exit cache-dir))))

(defn get-data-readers [^Context context]
  (when-let [readers-file (try (.open (.getAssets context) "data_readers.clj")
                               (catch FileNotFoundException e nil))]
    (->> readers-file
         slurp
         read-string
         (map (fn [[k v]] [k (resolve v)]))
         (into {}))))

(defn init
  "Initializes the compilation path, creating or cleaning cache directory as
  necessary."
  ([^Context context]
     (when-not @cache-dir
       (let [^File dir  (File. (.getCacheDir context) "clojure_repl")
             path (.getAbsolutePath dir)]
         (reset! cache-dir dir)
         (.mkdir dir)
         (clear-cache)
         (System/setProperty "clojure.compile.path" path)
         (alter-var-root #'clojure.core/*data-readers*
                         (constantly (get-data-readers context)))
         (alter-var-root #'clojure.core/*compile-path* (constantly path))))))
