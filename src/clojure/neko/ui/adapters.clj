(ns neko.ui.adapters
  "Contains custom adapters for ListView and Spinner."
  (:require [neko.debug :refer [safe-for-ui]]
            [neko.threading :refer [on-ui]]
            [neko.ui :refer [make-ui-element]])
  (:import android.view.View
           neko.ui.adapters.InterchangeableListAdapter))

(defn ref-adapter
  "Takes a function that creates a View, a function that updates a
  view according to the element and a reference type that stores the
  data. Returns an Adapter object that displays ref-type contents.
  When ref-type is updated, Adapter gets updated as well.

  `create-view-fn` is a function of no arguments. `update-view-fn` is
  a function of four arguments: element position, view to update,
  parent view container and the respective data element from the
  ref-type. `access-fn` argument is optional, it is called on the
  value of ref-type to get the list to be displayed."
  ([create-view-fn update-view-fn ref-type]
   (ref-adapter create-view-fn update-view-fn ref-type identity))
  ([create-view-fn update-view-fn ref-type access-fn]
   {:pre [(fn? create-view-fn) (fn? update-view-fn)
          (instance? clojure.lang.IFn access-fn)
          (instance? clojure.lang.IDeref ref-type)]}
   (let [create-fn (fn [context]
                     (or (safe-for-ui
                          (let [view (create-view-fn)]
                            (if (instance? View view)
                              view
                              (make-ui-element
                               context view
                               {:container-type :abs-listview-layout}))))
                         (android.view.View. context)))
         adapter (InterchangeableListAdapter.
                  create-fn
                  (fn [pos view parent data]
                    (safe-for-ui (update-view-fn pos view parent data)))
                  (access-fn @ref-type))]
     (add-watch ref-type ::adapter-watch
                (fn [_ __ ___ new-state]
                  (on-ui (.setData adapter (access-fn new-state)))))
     adapter)))
