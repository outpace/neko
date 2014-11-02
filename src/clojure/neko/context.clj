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

(ns neko.context
  "Utilities to aid in working with a context."
  {:author "Daniel Solano Gómez"}
  (:require [neko.-utils :as u])
  (:use [clojure.string :only [upper-case]])
  (:import android.content.Context))

(def ^{:doc "Stores Application instance that acts as context."}
  ^Context context)

(defmacro get-service
  "Gets a system service from the context.  The type argument is a keyword that
  names the service type.  Examples include :alarm for the alarm service and
  :layout-inflater for the layout inflater service."
  {:pre [(keyword? type)]
   :forms '([context type])}
  ([type]
     (println "One-argument version is deprecated. Please use (get-service context type)")
     `(get-service ~context ~type))
  ([context type]
     `(.getSystemService
       context
       ~(symbol (str (.getName Context) "/"
                     (u/keyword->static-field (name type)) "_SERVICE")))))

(defn inflate-layout
  "Inflates the layout with the given ID."
  {:forms '([context id])}
  ([id]
     (println "One-argument version is deprecated. Please use (inflate-layout context id)")
     (inflate-layout context id))
  ([context id]
     {:pre [(integer? id)]
      :post [(instance? android.view.View %)]}
     (.. android.view.LayoutInflater
         (from context)
         (inflate ^Integer id nil))))
