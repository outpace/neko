(ns neko.ui.traits
  "Contains trait declarations for various UI elements."
  (:require [neko.ui.mapping :as kw]
            [neko.resource :as res]
            [neko.listeners.view :as view-listeners]
            [neko.listeners.text-view :as text-view-listeners]
            [neko.listeners.adapter-view :as adapter-view]
            neko.listeners.search-view)
  (:use [neko.-utils :only [memoized]])
  (:import [android.widget LinearLayout$LayoutParams ListView TextView SearchView
            ImageView RelativeLayout RelativeLayout$LayoutParams
            AbsListView$LayoutParams]
           [android.view View ViewGroup$LayoutParams
            ViewGroup$MarginLayoutParams]
           android.graphics.Bitmap android.graphics.drawable.Drawable
           android.net.Uri
           android.util.TypedValue
           android.content.Context
           java.util.HashMap
           clojure.lang.Keyword))

;; ## Infrastructure for traits and attributes

(defmulti apply-trait
  "Transforms the given map of attributes into the valid Java-interop
code of setters.

`trait` is the keyword for a transformer function over the attribute map.

`object-symbol` is an symbol for the UI element to apply setters to.

`attributes-map` is a map of attributes to their values.

`generated-code` is an attribute-setter code generated so far. The
code this method generates should be appended to it.

`options-map` is a map of additional options that come from higher
level elements to their inside elements. A transformer can use this
map to provide some arguments to its own inside elements.

 Returns a vector that looks like `[new-generated-code
attributes-update-fn options-update-fn]`. `attributes-update-fn`
should take attributes map and remove processed attributes from it.
`options-update-fn` should remove old or introduce new options for
next-level elements."
  (fn [trait widget attributes-map options-map]
    trait))

(defn add-attributes-to-meta
  "Appends information about attribute to trait mapping to `meta`."
  [meta attr-list trait]
  (reduce (fn [m att]
            (update-in m [:attributes att]
                       #(if %
                          (conj % trait)
                          #{trait})))
          meta attr-list))

(defmacro deftrait
  "Defines a trait with the given name.

  `match-pred` is a function on attributes map that should return a
  logical truth if this trait should be executed against the widget
  and the map. By default it checks if attribute with the same name as
  trait's is present in the attribute map.

  The parameter list is the following: `[widget attributes-map
  options-map]`.

  Body of the trait can optionally return a map with the following
  keys: `:attribute-fn`, `:options-fn`, which values are functions to
  be applied to attributes map and options map respectively after the
  trait finishes its work. If they are not provided, `attribute-fn`
  defaults to dissoc'ing trait's name from attribute map, and
  `options-fn` defaults to identity function."
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (next args)]
                           [nil args])
        [param-map args] (if (map? (first args))
                           [(first args) (next args)]
                           [{} args])
        attrs-sym (gensym "attributes")
        match-pred (cond (:applies? param-map)
                         (:applies? param-map)

                         (:attributes param-map)
                         `(some ~attrs-sym ~(:attributes param-map))

                         :else `(~name ~attrs-sym))
        [arglist & codegen-body] args
        dissoc-fn (if (:attributes param-map)
                    `(fn [a#] (apply dissoc a# ~(:attributes param-map)))
                    `(fn [a#] (dissoc a# ~name)))]
    `(do
       (alter-meta! #'apply-trait
                    (fn [m#]
                      (-> m#
                          (assoc-in [:trait-doc ~name] ~docstring)
                          (add-attributes-to-meta
                           (or ~(:attributes param-map) [~name]) ~name))))
       (defmethod apply-trait ~name
         [trait# widget# ~attrs-sym options#]
         (let [~arglist [widget# ~attrs-sym options#]]
           (if ~match-pred
             (let [result# (do ~@codegen-body)
                   attr-fn# ~dissoc-fn]
               (if (map? result#)
                 [(:attributes-fn result# attr-fn#)
                  (:options-fn result# identity)]
                 [attr-fn# identity]))
             [identity identity]))))))

(alter-meta! #'deftrait
             assoc :arglists '([name docstring? param-map? [params*] body]))

;; ## Utility functions

(defn to-id
  "Makes an ID from arbitrary object by calling .hashCode on it.
  Returns the absolute value."
  [obj]
  (Math/abs (.hashCode ^Object obj)))

;; ## Implementation of different traits

;; ### Def attribute

(deftrait :def
  "Takes a symbol provided to `:def` and binds the widget to it.

  Example: `[:button {:def ok}]` defines a var `ok` which stores the
  button object."
  [wdg {:keys [def]} _]
  (assert (and (symbol? def) (namespace def)))
  (intern (symbol (namespace def)) (symbol (name def)) wdg))

;; ### Basic traits

(deftrait :text
  "Sets widget's text to a string, integer ID or a keyword
  representing the string resource provided to `:text` attribute."
  [^TextView wdg, {:keys [text] :or {text ""}} _]
  (.setText wdg ^CharSequence (res/get-string text)))

(defn- kw->unit-id [unit-kw]
  (case unit-kw
    :px TypedValue/COMPLEX_UNIT_PX
    :dp TypedValue/COMPLEX_UNIT_DIP
    :dip TypedValue/COMPLEX_UNIT_DIP
    :sp TypedValue/COMPLEX_UNIT_SP
    :pt TypedValue/COMPLEX_UNIT_PT
    :in TypedValue/COMPLEX_UNIT_IN
    :mm TypedValue/COMPLEX_UNIT_MM
    TypedValue/COMPLEX_UNIT_PX))

(memoized
 (defn- get-display-metrics
   "Returns Android's DisplayMetrics object from application context."
   [^Context context]
   (.. context (getResources) (getDisplayMetrics))))

(defn to-dimension [context value]
  (if (vector? value)
    (Math/round
     ^float (TypedValue/applyDimension
             (kw->unit-id (second value))
             (first value) (get-display-metrics context)))
    value))

(deftrait :text-size
  "Takes `:text-size` attribute which should be either integer or a
  dimension vector, and sets it to the widget."
  [^TextView wdg, {:keys [text-size]} _]
  (if (vector? text-size)
    (.setTextSize wdg (kw->unit-id (second text-size)) (first text-size))
    (.setTextSize wdg text-size)))

(deftrait :image
  "Takes `:image` attribute which can be a resource ID, resource
  keyword, Drawable, Bitmap or URI and sets it ImageView widget's
  image source." [^ImageView wdg, {:keys [image]} _]
  (condp instance? image
    Bitmap (.setImageBitmap wdg image)
    Drawable (.setImageDrawable wdg image)
    Keyword (.setImageDrawable wdg (neko.resource/get-drawable image))
    Uri (.setImageURI wdg image)
    ;; Otherwise assume `image` to be resource ID.
    :else (.setImageResource wdg image)))

;; ### Layout parameters attributes

(def ^:private margin-attributes [:layout-margin
                                  :layout-margin-left :layout-margin-top
                                  :layout-margin-right :layout-margin-bottom])

(defn- apply-margins-to-layout-params
  "Takes a LayoutParams object that implements MarginLayoutParams
  class and an attribute map, and sets margins for this object."
  [context, ^ViewGroup$MarginLayoutParams params, attribute-map]
  (let [common (to-dimension context (attribute-map :layout-margin 0))
        [l t r b] (map #(to-dimension context (attribute-map % common))
                       (rest margin-attributes))]
    (.setMargins params l t r b)))

(deftrait :default-layout-params
  "Takes `:layout-width` and `:layout-height` attributes and sets
   LayoutParams, if the container type is not specified."
  {:attributes [:layout-width :layout-height]
   :applies? (and (or layout-width layout-height) (nil? container-type))}
  [^View wdg, {:keys [layout-width layout-height]} {:keys [container-type]}]
  (let [^int width  (kw/value :layout-params (or layout-width  :wrap))
        ^int height (kw/value :layout-params (or layout-height :wrap))]
   (.setLayoutParams wdg (ViewGroup$LayoutParams. width height))))

(deftrait :linear-layout-params
  "Takes `:layout-width`, `:layout-height`, `:layout-weight`,
  `:layout-gravity` and different layout margin attributes and sets
  LinearLayout.LayoutParams if current container is LinearLayout.
  Values could be either numbers of `:fill` or `:wrap`."
  {:attributes (concat margin-attributes [:layout-width :layout-height
                                          :layout-weight :layout-gravity])
   :applies? (= container-type :linear-layout)}
  [^View wdg, {:keys [layout-width layout-height layout-weight layout-gravity]
               :as attributes}
   {:keys [container-type]}]
  (let [width  (kw/value :layout-params (or layout-width  :wrap))
        height (kw/value :layout-params (or layout-height :wrap))
        weight (or layout-weight 0)
        params (LinearLayout$LayoutParams. width height weight)]
    (apply-margins-to-layout-params (.getContext wdg) params attributes)
    (when layout-gravity
      (set! (. params gravity)
            (kw/value :layout-params layout-gravity :gravity)))
    (.setLayoutParams wdg params)))

;; #### Relative layout

(def ^:private relative-layout-attributes
  ;; Hard-coded number values are attributes that appeared since
  ;; Android Jellybean.
  {:standalone {:layout-align-parent-bottom  RelativeLayout/ALIGN_PARENT_BOTTOM
                :layout-align-parent-end     21 ; RelativeLayout/ALIGN_PARENT_END
                :layout-align-parent-left    RelativeLayout/ALIGN_PARENT_LEFT
                :layout-align-parent-right   RelativeLayout/ALIGN_PARENT_RIGHT
                :layout-align-parent-start   20 ; RelativeLayout/ALIGN_PARENT_START
                :layout-align-parent-top     RelativeLayout/ALIGN_PARENT_TOP
                :layout-center-horizontal    RelativeLayout/CENTER_HORIZONTAL
                :layout-center-vertical      RelativeLayout/CENTER_VERTICAL
                :layout-center-in-parent     RelativeLayout/CENTER_IN_PARENT}
   :with-id    {:layout-above                RelativeLayout/ABOVE
                :layout-align-baseline       RelativeLayout/ALIGN_BASELINE
                :layout-align-bottom         RelativeLayout/ALIGN_BOTTOM
                :layout-align-end            19 ; RelativeLayout/ALIGN_END
                :layout-align-left           RelativeLayout/ALIGN_LEFT
                :layout-align-right          RelativeLayout/ALIGN_RIGHT
                :layout-align-start          18 ; RelativeLayout/ALIGN_START
                :layout-align-top            RelativeLayout/ALIGN_TOP
                :layout-below                RelativeLayout/BELOW
                :layout-to-end-of            17 ; RelativeLayout/END_OF
                :layout-to-left-of           RelativeLayout/LEFT_OF
                :layout-to-right-of          RelativeLayout/RIGHT_OF
                :layout-to-start-of          16 ; RelativeLayout/START_OF
                }})

(def ^:private all-relative-attributes
  (apply concat [:layout-width :layout-height
                 :layout-align-with-parent-if-missing]
         (map keys (vals relative-layout-attributes))))

(deftrait :relative-layout-params
  {:attributes (concat all-relative-attributes margin-attributes)
   :applies? (= container-type :relative-layout)}
  [^View wdg, {:keys [layout-width layout-height
                      layout-align-with-parent-if-missing] :as attributes}
   {:keys [container-type]}]
  (let [^int width  (kw/value :layout-params (or layout-width  :wrap))
        ^int height (kw/value :layout-params (or layout-height :wrap))
        lp (RelativeLayout$LayoutParams. width height)]
    (when-not (nil? layout-align-with-parent-if-missing)
      (set! (. lp alignWithParent) layout-align-with-parent-if-missing))
    (doseq [[attr-name attr-id] (:standalone relative-layout-attributes)]
      (when (= (attr-name attributes) true)
        (.addRule lp attr-id)))
    (doseq [[attr-name attr-id] (:with-id relative-layout-attributes)]
      (when (contains? attributes attr-name)
        (.addRule lp attr-id (to-id (attr-name attributes)))))
    (apply-margins-to-layout-params (.getContext wdg) lp attributes)
    (.setLayoutParams wdg lp)))

(deftrait :listview-layout-params
  {:attributes [:layout-width :layout-height :layout-view-type]
   :applies? (= container-type :abs-listview-layout)}
  [^View wdg, {:keys [layout-width layout-height layout-view-type]
               :as attributes}
   {:keys [container-type]}]
  (let [^int width  (kw/value :layout-params (or layout-width  :wrap))
        ^int height (kw/value :layout-params (or layout-height :wrap))]
    (.setLayoutParams
     wdg (if layout-view-type
           (AbsListView$LayoutParams. width height layout-view-type)
           (AbsListView$LayoutParams. width height)))))

(deftrait :padding
  "Takes `:padding`, `:padding-bottom`, `:padding-left`,
  `:padding-right` and `:padding-top` and set element's padding
  according to their values. Values might be either integers or
  vectors like `[number unit-kw]`, where unit keyword is one of the
  following: :px, :dip, :sp, :pt, :in, :mm."
  {:attributes [:padding :padding-bottom :padding-left
                :padding-right :padding-top]}
  [^View wdg {:keys [padding padding-bottom padding-left
                     padding-right padding-top]} _]
  (let [ctx (.getContext wdg)]
    (.setPadding wdg
                 (to-dimension ctx (or padding-left padding 0))
                 (to-dimension ctx (or padding-top padding 0))
                 (to-dimension ctx (or padding-right padding 0))
                 (to-dimension ctx (or padding-bottom padding 0)))))

(deftrait :container
  "Puts the type of the widget onto the options map so subelement can
  use the container type to choose the correct LayoutParams instance."
  {:applies? (constantly true)}
  [wdg _ __]
  {:options-fn #(assoc % :container-type (kw/keyword-by-classname (type wdg)))})

;; ### Listener traits

(deftrait :on-click
  "Takes :on-click attribute, which should be function of one
  argument, and sets it as an OnClickListener for the widget."
  [^View wdg, {:keys [on-click]} _]
  (.setOnClickListener wdg (view-listeners/on-click-call on-click)))

(deftrait :on-create-context-menu
  "Takes :on-create-context-menu attribute, which should be function
  of three arguments, and sets it as an OnCreateContextMenuListener
  for the object."
  [^View wdg, {:keys [on-create-context-menu]} _]
  (.setOnCreateContextMenuListener
   wdg (view-listeners/on-create-context-menu-call on-create-context-menu)))

(deftrait :on-focus-change
  "Takes :on-focus-change attribute, which should be function of two
  arguments, and sets it as an OnFocusChangeListener for the object."
  [^View wdg, {:keys [on-focus-change]} _]
  (.setOnFocusChangeListener
   wdg (view-listeners/on-focus-change-call on-focus-change)))

(deftrait :on-key
  "Takes :on-key attribute, which should be function of three
  arguments, and sets it as an OnKeyListener for the widget."
  [^View wdg, {:keys [on-key]} _]
  (.setOnKeyListener wdg (view-listeners/on-key-call on-key)))

(deftrait :on-long-click
  "Takes :on-long-click attribute, which should be function of one
  argument, and sets it as an OnLongClickListener for the widget."
  [^View wdg, {:keys [on-long-click]} _]
  (.setOnLongClickListener
   wdg (view-listeners/on-long-click-call on-long-click)))

(deftrait :on-touch
  "Takes :on-touch attribute, which should be function of two
  arguments, and sets it as an OnTouchListener for the widget."
  [^View wdg, {:keys [on-touch]} _]
  (.setOnTouchListener wdg (view-listeners/on-touch-call on-touch)))

(deftrait :on-query-text
  "Takes `:on-query-text-change` and `:on-query-text-submit`
  attributes, which should be functions of one or two arguments,
  depending on the context of usage. If widget is used as an action
  item in a menu, two arguments are passed to the function - query
  text and the menu item, for which widget is being action item to.
  Otherwise only query text is passed to the functions.

  Then OnQueryTextListener object is created from the functions and
  set to the widget."
  {:attributes [:on-query-text-change :on-query-text-submit]}
  [^SearchView wdg, {:keys [on-query-text-change on-query-text-submit]}
   {:keys [menu-item]}]
  (.setOnQueryTextListener
   wdg (neko.listeners.search-view/on-query-text-call
        (if (and menu-item on-query-text-change)
          (fn [q] (on-query-text-change q menu-item))
          on-query-text-change)
        (if (and menu-item on-query-text-submit)
          (fn [q] (on-query-text-submit q menu-item))
          on-query-text-submit))))

(deftrait :on-editor-action
  "Takes :on-editor-action attribute, which should be function
  of three arguments, and sets it as OnEditorAction for the
  TexView widget"
  [^TextView wdg, {:keys [on-editor-action]}]
  (.setOnEditorActionListener wdg (text-view-listeners/on-editor-action-call on-editor-action)))

;; ### ID storing traits

(deftrait :id-holder
  "Takes `:id-holder` attribute which should equal true and marks the
  widget to be a holder of lower-level elements. Elements are stored
  by their IDs as keys in a map, which is accessible by calling
  `.getTag` on the holder widget.

  Example:

  (def foo (make-ui [:linear-layout {:id-holder true}
                     [:button {:id ::abutton}]]))
  (::abutton (.getTag foo)) => internal Button widget."
  [^View wdg, _ __]
  (.setTag wdg (HashMap.))
  {:options-fn #(assoc % :id-holder wdg)})

(deftrait :id
  "Takes `:id` attribute, which can either be an integer or a
  keyword (that would be transformed into integer as well) and sets it
  as widget's ID attribute. Also, if an ID holder was declared in
  this tree, stores the widget in id-holder's tag (see docs for
  `:id-holder`trait)."
  [^View wdg, {:keys [id]} {:keys [^View id-holder]}]
  (.setId wdg (to-id id))
  (when id-holder
    (.put ^HashMap (.getTag id-holder) id wdg)))

(deftrait :on-item-click
  "Takes :on-item-click attribute, which should be function of four arguments

    parent   AdapterView of the originating click
    view     Item view
    position Item view position
    id       Item view row id

  and sets it as an OnItemClickListener for the widget."
  [^ListView wdg, {:keys [on-item-click]} _]
  (.setOnItemClickListener wdg (adapter-view/on-item-click-call on-item-click)))

