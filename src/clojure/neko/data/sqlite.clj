(ns neko.data.sqlite
  "Alpha - subject to change.

  Contains convenience functions to work with SQLite databases Android
  provides."
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string])
  (:use [neko.context :only [context]])
  (:import [android.database.sqlite SQLiteDatabase SQLiteOpenHelper]
           neko.data.sqlite.SQLiteHelper
           [android.database Cursor CursorIndexOutOfBoundsException SQLException]
           [android.content ContentValues Context]
           [clojure.lang Keyword PersistentVector]))

;; ### Database initialization

(def ^:private supported-types
  "Mapping of SQLite types to respective Java classes. Byte actually stands for
  array of bytes, or Blob in SQLite."
  {"integer" Integer
   "long" Long
   "text" String
   "boolean" Boolean
   "double" Double
   "blob" Byte})

(defn make-schema
  "Creates a schema from arguments and validates it."
  [& {:as schema}]
  (assert (string? (:name schema)) ":name should be a String.")
  (assert (number? (:version schema)) ":version should be a number.")
  (assert (map? (:tables schema)) ":tables should be a map.")
  (assoc schema
    :tables
    (into
     {} (for [[table-name params] (:tables schema)]
          (do
            (assert (keyword? table-name)
                    (str "Table name should be a keyword: " table-name))
            (assert (map? params)
                    (str "Table parameters should be a map: " table-name))
            (assert (map? (:columns params))
                    (str "Table parameters should contain columns map: " table-name))
            [table-name
             (assoc params
               :columns
               (into
                {} (for [[column-name col-params] (:columns params)]
                     (do
                       (assert (keyword? column-name)
                               (str "Column name should be a keyword: " column-name))
                       (assert (or (map? col-params) (class? Integer))
                               (str "Column type should be a map or a string:"
                                    column-name))
                       (let [col-type (if (string? col-params)
                                        col-params
                                        (:sql-type col-params))
                             java-type (-> (re-matches #"(\w+).*" col-type)
                                           second supported-types)
                             col-params {:type java-type
                                         :sql-type col-type}]
                         (assert java-type
                                 (str "Type is not supported: " (:sql-type col-params)))
                         [column-name col-params])))))])))))

(defn- db-create-query
  "Generates a table creation query from the provided schema and table
  name."
  [schema table-name]
  (->> (get-in schema [:tables table-name :columns])
       (map (fn [[col params]]
              (str (name col) " " (:sql-type params))))
       (interpose ", ")
       string/join
       (format "create table %s (%s);" (name table-name))))

(defn ^SQLiteOpenHelper create-helper
  "Creates a SQLiteOpenHelper instance for a given schema.

  Helper will recreate database if the current schema version and
  database version mismatch."
  {:forms '([context schema])}
  ([schema]
   (println "One-argument version is deprecated. Please use (create-helper context schema)")
   (create-helper context schema))
  ([^Context context, {:keys [name version tables] :as schema}]
   (SQLiteHelper. (.getApplicationContext context) name version
                  (for [table (keys tables)]
                    (db-create-query schema table))
                  (for [^Keyword table (keys tables)]
                    (str "drop table if exists " (.getName table))))))

;; A wrapper around SQLiteDatabase to keep database and its schema
;; together.
;;
(deftype TaggedDatabase [^SQLiteDatabase db, schema])

(defn get-database
  "Returns SQLiteDatabase instance for the given schema. Access-mode can be
  either `:read` or `:write`."
  {:forms '([context schema access-mode])}
  ([schema access-mode]
   (println "Two-argument version is deprecated. Please use (get-database context schema access-mode)")
   (get-database context schema mode))
  ([context schema access-mode]
   {:pre [(#{:read :write} access-mode)]}
   (let [helper (create-helper context schema)]
     (TaggedDatabase. (case access-mode
                        :read (.getReadableDatabase helper)
                        :write (.getWritableDatabase helper))
                      schema))))

;; ### Data-SQL transformers

(defn- map-to-content
  "Takes a map of column keywords to values and creates a
  ContentValues instance from it."
  [^TaggedDatabase tagged-db table data-map]
  (let [^ContentValues cv (ContentValues.)]
    (doseq [[col {type :type}] (get-in (.schema tagged-db)
                                       [:tables table :columns])
            :when (contains? data-map col)]
      (let [value (get data-map col)]
        (condp = type
          Integer (.put cv (name col) ^Integer (int value))
          Long (.put cv (name col) ^Long value)
          Double (.put cv (name col) ^Double value)
          String (.put cv (name col) ^String value)
          Boolean (.put cv (name col) ^Boolean value)
          Byte (.put cv (name col) ^bytes value))))
    cv))

(defn- get-value-from-cursor
  "Gets a single value out of the cursor from the specified column."
  [^Cursor cur i type]
  (condp = type
    Boolean (= (.getInt cur i) 1)
    Integer (.getInt cur i)
    Long (.getLong cur i)
    String (.getString cur i)
    Double (.getDouble cur i)
    Byte (.getBlob cur i)))

(defn- keyval-to-sql
  "Transforms a key-value pair into a proper SQL comparison/assignment
  statement.

  For example, it will put single quotes around String value. The
  value could also be a vector that looks like `[:or value1 value2
  ...]`, in which case it will be transformed into `key = value1 OR
  key = value2 ...`. Nested vectors is supported."
  [k v]
  (let [k (name k)]
    (condp #(= % (type %2)) v
      PersistentVector (let [[op & values] v]
                         (->> values
                              (map (partial keyval-to-sql k))
                              (interpose (str " " (name op) " "))
                              string/join))
      String (format "(%s = '%s')" k v)
      Boolean (format "(%s = %s)" k (if v 1 0))
      nil (format "(%s is NULL)" k)
      (format "(%s = %s)" k v))))

;; ### SQL operations

(defn- where-clause
  "Takes a map of column keywords to values and generates a WHERE
  clause from it."
  [where]
  (if (string? where)
    where
    (->> where
         (map (partial apply keyval-to-sql))
         (interpose " AND ")
         string/join)))

(defn query
  "Executes SELECT statement against the database and returns a Cursor
  object with the results. `where` argument should be a map of column
  keywords to values."
  [^TaggedDatabase tagged-db table-name where]
  (let [columns (->> (get-in (.schema tagged-db) [:tables table-name :columns])
                     keys
                     (map name)
                     into-array)]
    (.query (.db tagged-db) (name table-name) columns
            (where-clause where) nil nil nil nil)))
(def db-query query)

(defn seq-cursor
  "Turns data from Cursor object into a lazy sequence. Takes database
  argument in order to get schema from it."
  [^TaggedDatabase tagged-db, table-name, ^Cursor cursor]
  (.moveToFirst cursor)
  (let [columns (get-in (.schema tagged-db) [:tables table-name :columns])
        seq-fn (fn seq-fn []
                 (lazy-seq
                  (if (.isAfterLast cursor)
                    (.close cursor)
                    (let [v (reduce-kv
                             (fn [data i [column-name {type :type}]]
                               (assoc data column-name
                                      (get-value-from-cursor cursor i type)))
                             {} (vec columns))]
                      (.moveToNext cursor)
                      (cons v (seq-fn))))))]
    (seq-fn)))

(defn query-seq
  "Executes a SELECT statement against the database and returns the
  result in a sequence. Same as calling `seq-cursor` on `query` output."
  [^TaggedDatabase tagged-db table-name where]
  (seq-cursor tagged-db table-name (db-query tagged-db table-name where)))
(def db-query-seq query-seq)

(defn query-scalar
  "Executes a SELECT statement against the database on a column and returns a
  scalar value. `column` can be either a keyword or string-keyword pair where
  string denotes the aggregation function."
  [^TaggedDatabase tagged-db table-name column where]
  (let [[aggregator column] (if (vector? column)
                              column [nil column])
        type (get-in (.schema tagged-db)
                     [:tables table-name :columns column :type])
        where-cl (where-clause where)
        query (format "select %s from %s %s"
                      (if aggregator
                        (str aggregator "(" (name column) ")")
                        (name column))
                      (name table-name)
                      (if (seq where-cl)
                        (str "where " where-cl) ""))]
    (with-open [cursor (.rawQuery (.db tagged-db) query nil)]
      (try (.moveToFirst cursor)
           (get-value-from-cursor cursor 0 type)
           (catch CursorIndexOutOfBoundsException e nil)))))

(defn update
  "Executes UPDATE query against the database generated from set and
  where clauses given as maps where keys are column keywords."
  [^TaggedDatabase tagged-db table-name set where]
  (.update (.db tagged-db) (name table-name)
           (map-to-content tagged-db table-name set)
           (where-clause where) nil))
(def db-update update)

(defn insert
  "Executes INSERT query against the database generated from data-map
  where keys are column keywords."
  [^TaggedDatabase tagged-db table-name data-map]
  (.insert (.db tagged-db) (name table-name) nil
           (map-to-content tagged-db table-name data-map)))
(def db-insert insert)

(defmacro transact
  "Wraps the code in beginTransaction-endTransaction calls for batch query
  execution."
  [db & body]
  `(try (.beginTransaction (.db ~db))
        ~@body
        (.setTransactionSuccessful (.db ~db))
        (finally (.endTransaction (.db ~db)))))
