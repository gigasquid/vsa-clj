(ns gigasquid.vsa-base
  (:require
    [tech.v3.datatype.functional :as dtype-fn]
    [tech.v3.tensor :as dtt]))


;; Following examples in "An Introduction to Hyperdimensional Computing for Robotics"
;; https://link.springer.com/article/10.1007/s13218-019-00623-z

;; Uses Gaylor Method for HDV Operations

(def size 1e6)  ; big enough for the "Blessing of Dimensionality"

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
  "Protects a bundle value from additional bundling by rotating the hdv. Akin to a list / array function. More akin to push in a stack."
  [v]
  (dtt/rotate v [1]))


(defn protect-n
  "Calls protect n times"
  [v n]
  (loop [new-v v
         i n]
    (if (zero? i)
      new-v
      (recur (protect new-v) (dec i)))))


(defn unprotect
  "Reverse the rotation of the bundle. Like pop of a stack"
  [v]
  (dtt/rotate v [-1]))


(defn unprotect-n
  "Calls unprotect n times"
  [v n]
  (loop [new-v v
         i n]
    (if (zero? i)
      new-v
      (recur (unprotect new-v) (dec i)))))


;; The cleanup memory stores the hdv without any noise. When a hdv
;; is retrieved from the bundle it has some amount of noise associated with it. It helps to use the cleaned version after retrieving it for futher operations
(def cleanup-mem (atom {}))


(defn reset-mem!
  "Resets the cleanup memory"
  []
  (reset! cleanup-mem {}))


(defn add-to-cleanup-mem
  "Adds a new symbol/value pair to the cleanup memory"
  [k v]
  (swap! cleanup-mem merge {k v})
  v)


(defn similarity-score
  "Find the similarity between two hdvs by dot product and also
  by cosine similarity"
  [query-v v]
  (let [dotx (dtype-fn/dot-product v query-v)]
    {:dot dotx
     :cos-sim (/ dotx
                 (* (dtype-fn/magnitude v)
                    (dtype-fn/magnitude query-v)))}))


(defn query-cleanup-mem-verbose
  "Finds the nearest neighbor to the hdv by using the dot product and cosine.
   Then returns the cleaned vector - returns all results and score"
  [query-v]
  (->> @cleanup-mem
       (map (fn [[k v]]
              (merge {k v} (similarity-score query-v v))))
       (sort-by :dot)))


(defn query-cleanup-mem
  "Finds the nearest neighbor to the hdv by using the dot product.
   Then returns the cleaned vector. If given a theshold, uses cosine simalarity (0-1) and returns all the possible matches if they are greather than or equal to the threshold. Or none if there are no matches."
  ([query-v]
   (query-cleanup-mem nil nil query-v))
  ([threshold verbose? query-v]
   (let [sorted-dot (query-cleanup-mem-verbose query-v)]
     (cond->> (reverse sorted-dot)
       threshold
       (filterv (fn [{:keys [cos-sim] :as result}]
                  (when (>= cos-sim threshold)
                    result)))

       (not verbose?)
       (mapv (fn [result] (dissoc result :dot :cos-sim)))

       (nil? threshold)
       (ffirst)))))


(defn get-hdv
  "Gets the clean hdv from the memory by key"
  [k]
  (get @cleanup-mem k))


(defn add-hdv!
  "Adds the hdv to the cleanup memory"
  ([k]
   (add-to-cleanup-mem k (hdv)))
  ([k v]
   (add-to-cleanup-mem k v)))


(defn unbind-get
  "Gets the key hdv from the memory and unbinds
  (bind is the inverse of itself) the
   value from the bundle hdv. Queries the cleaned up vector
   from memory. With the parameter of a threshold, uses a cosine
   similatrity score (0 -> 1.0) for exact match.
   Example a threshold of 0.1 would return a result if the score
   was greater than or equal to 0.1, otherwise nil.
   If no key hdv from mem was found it will return an exception
   that no key was found"
  ([hdv k]
   (unbind-get hdv k nil nil))
  ([hdv k threshold verbose?]
   (let [key-v (get-hdv k)]
     (if key-v
       (->> key-v
            (bind hdv)
            (query-cleanup-mem threshold verbose?))
       (throw (ex-info "No key found in memory" {:key-value k}))))))


(defn reset-hdv-mem!
  []
  (reset-mem!))
