(ns neko.ui.listview
  "Contains utilities to work with ListView."
  (:import android.util.SparseBooleanArray
           android.widget.ListView))

(defn get-checked
  "Returns a vector of indices for items being checked in a ListView.
  The two-argument version additionally takes a sequence of data
  elements of the ListView (usually the data provided to the adapter)
  and returns the vector of only those elements that are checked. "
  ([^ListView lv]
     (let [^SparseBooleanArray bool-array (.getCheckedItemPositions lv)
           count (.getCount lv)]
       (loop [i 0, result []]
         (if (= i count)
           result
           (if (.get bool-array i)
             (recur (inc i) (conj result i))
             (recur (inc i) result))))))
  ([^ListView lv, items]
     (let [^SparseBooleanArray bool-array (.getCheckedItemPositions lv)
           count (.getCount lv)]
       (loop [i 0, [curr & rest] items, result []]
         (if (= i count)
           result
           (if (.get bool-array i)
             (recur (inc i) rest (conj result curr))
             (recur (inc i) rest result)))))))

(defn set-checked!
  "Given a sequence of numbers checks the respective ListView
  elements."
  [^ListView lv, checked-ids]
  (doseq [i checked-ids]
    (.setItemChecked lv i true)))
