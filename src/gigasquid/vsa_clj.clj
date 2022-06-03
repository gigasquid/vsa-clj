(ns gigasquid.vsa-clj
  (:require
    [tech.v3.datatype.functional :as dtype-fn]
    [tech.v3.tensor :as dtt]))

;;; Following examples in "An Introduction to Hyperdimensional Computing for Robotics"
;;https://link.springer.com/article/10.1007/s13218-019-00623-z

;;; Uses Gaylor Method for HDV Operations


(def size 1e6)


(defn binary-rand
  "Choose a random binary magnitude for the vector +1 or -1"
  []
  (if (> (rand) 0.5) -1 1))


(defn hdv
  "Create a random hyperdimensional vector of default size"
  []
  (dtt/->tensor (repeatedly size #(binary-rand)) :datatype :int8))


(defn bundle-op
  "The bundle operation of addition"
  [v1 v2]
  (dtype-fn/+ v1 v2))


(defn clip
  "Clips the hyperdimensional vector magnitude to 1 or -1.
   We can discard these because of the nature of the large vectors
   that the mangitudes do not matter"
  [v]
  (-> v
      (dtype-fn/min 1)
      (dtype-fn/max -1)))


(defn bundle
  "Adds two hyperdimensional vectors together into a single bundle"
  [v1 v2]
  (-> (bundle-op v1 v2)
      (clip)))


(defn bind
  "Binds two HDVs using the multiplication operator. This binding is akin to assigning a symbol to a value. "
  [v1 v2]
  (dtype-fn/* v1 v2))


(defn protect
  "Protects a bundle value from additional bundling by rotating the hdv. Akin to a list / array function. More akin to push in a stack"
  [v]
  (dtt/rotate v [1]))


(defn unprotect
  "Reverse the rotation of the bundle. Like pop of a stack"
  [v]
  (dtt/rotate v [-1]))



;;; The cleanup memory stores the hdv without any noise. When a hdv
;; is retrieved from the bundle it has some amount of noise associated with it. It helps to use the cleaned version after retrieving it for futher operations
(def cleanup-mem (atom {}))

(defn reset-mem!
  "Resets the cleanup memory"
  []
  (reset! cleanup-mem {}))


(defn add-to-cleanup-mem
  "Adds a new symbol/value pair to the cleanup memory"
  [k v]
  (swap! cleanup-mem merge {k v}))


(defn query-cleanup-mem
  "Finds the nearest neighbor to the hdv by using the dot product.
   Then returns the cleaned vector"
  [query-v]
  (->> @cleanup-mem
       (map (fn [[k v]]
              {k v :dot (dtype-fn/dot-product v query-v)}))
       (sort-by :dot)
       last
       first))


(defn get-hdv
  "Gets the clean hdv from the best match nosiy one"
  [k]
  (get @cleanup-mem k))


(defn add-hdv!
  "Adds the hdv to the cleanup memory"
  [k]
  (add-to-cleanup-mem k (hdv)))


(comment

  (reset-mem!)

  (add-hdv! :name)
  (add-hdv! "Alice")
  (add-hdv! :yob)
  (add-hdv! 1980)
  (add-hdv! :high-score)
  (add-hdv! 1000)

  (def H (-> (bind (get-hdv :name) (get-hdv "Alice"))
             (bundle
              (bind (get-hdv :yob) (get-hdv 1980)))
             (bundle
              (bind (get-hdv :high-score) (get-hdv 1000)))))

  (query-cleanup-mem (bind H (get-hdv :name)))
  ;; ["Alice" #tech.v3.tensor<int8>[1000000]
  ;;  [-1 1 1 ... 1 -1 -1]]

  (query-cleanup-mem (bind H (get-hdv :yob)))
 ;;  [1980 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 -1 -1]]

  (query-cleanup-mem (bind H (get-hdv :high-score)))
 ;;  [1000 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 1 1]]

  (query-cleanup-mem (bind H (get-hdv "Alice")))
 ;; [:name #tech.v3.tensor<int8>[1000000]
;; [-1 1 -1 ... -1 -1 -1]]
  )
