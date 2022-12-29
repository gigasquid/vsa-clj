(ns gigasquid.vsa-data
  "clojure data structures to hyperdimensional vectors"
  (:require
    [gigasquid.vsa-base :as vb]))


(def STACK_COUNT_KEY :STACK_COUNT_KEY)


(defn vsa-assoc
  "Like clojure assoc but result is bundled hdv. For non nested kvs"
  ([hdv k v]
   (let [mem-k (or (vb/get-hdv k) (vb/add-hdv! k))
         mem-v (or (vb/get-hdv v) (vb/add-hdv! v))
         mem-hdv (or hdv (vb/hdv))
         kv (vb/bind mem-k mem-v)]
     (vb/bundle mem-hdv kv)))
  ([hdv k v & kvs]
   (let [ret (vsa-assoc hdv k v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (ex-info "vsa-assoc expects even number of args"
                         {:args kvs})))
       ret))))


(defn vsa-get
  "Like clojure get with a map but with hdv also works with a value
   instead of a k. If passed an idx it will pop the pushed map value. If called with a threshold, it will return a vector of all items in memory that have a cosine simalarity threshold greater than or equal to it (range 0-1) example 0.1
  A verbose? key of true will return the actual scores for cosine similarity and dot"
  ([hdv k]
   (vsa-get hdv k {}))
  ([hdv k {:keys [idx threshold verbose?]}]
   (if (nil? idx)
     (vb/unbind-get hdv k threshold verbose?)
     (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)
           unprotect-num (- (dec p-count) idx)
           new-v (vb/unprotect-n hdv unprotect-num)]
       (vsa-get new-v k {:threshold threshold})))))


(defn map->vsa
  "Turn a clojure map (not nested) into a hdv"
  [m]
  (when-not (empty? m)
    (reduce (fn [ret-hdv [k v]]
              (let [mem-k (or (vb/get-hdv k) (vb/add-hdv! k))
                    mem-v (or (vb/get-hdv v) (vb/add-hdv! v))
                    kv (vb/bind mem-k mem-v)]
                (vb/bundle ret-hdv kv)))
            (vb/hdv)
            m)))


(defn vsa-stack-vector
  "Inits a clojure like vector/stack into the HDV denoted by a key/value pair that indicates the number of elements in the stack - defaults to 0"
  []
  (vsa-assoc (vb/hdv) STACK_COUNT_KEY 0))


(defn vsa-conj
  "Adds (bundles) the target hdv to the base hdv in a stack context but first protects it by rotation. Need to call vsa-init-stack on the base-hdv first"
  [base-hdv target-hdv]
  (let [[p-count _] (vsa-get base-hdv STACK_COUNT_KEY)
        new-p-count (inc p-count)
        protected-base-hdv (vb/protect base-hdv)]
    (-> protected-base-hdv
        (vsa-assoc STACK_COUNT_KEY new-p-count)
        (vb/bundle target-hdv))))


(defn vector->vsa
  "Takes a clojure vector data structure filled with maps  (non-nested) and turns it into a vsa data structure"
  [v]
  (reduce (fn [hdv x]
            (vsa-conj hdv (map->vsa x)))
          (vsa-stack-vector)
          v))


(defn clj->vsa
  "Takes either a vector of maps or a map and turns it into a vsa high dimensional vector"
  [form]
  (cond (and (vector? form) (every? map? form)) (vector->vsa form)
        (map? form) (map->vsa form)
        :else (throw (new Exception "Data structure not supported"))))


(defn vsa-inspect
  "Find all keys embedded in the hdv from mem"
  ([hdv]
   (vsa-inspect hdv 0.1))
  ([hdv threshold]
   (->>  @vb/cleanup-mem
         (mapcat (fn [[k _]]
                   (vsa-get hdv k {:threshold threshold})))
         (mapcat keys)
         (into #{}))))


(defn vsa-map
  "Takes a hdv and maps the function across all the stack items in it. Unrolling stack from back to front"
  [f hdv]
  (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)]
    (loop [new-v hdv
           i p-count
           result []]
      (if (zero? i)
        (reverse result) ; reverse because the unprotect does it in stack order
        (recur (vb/unprotect new-v) (dec i) (conj result (f new-v)))))))


(defn vsa-filter
  "Takes a hdv and filters the predicate across all the stack items in it. Unrolling stack from back to front.
  Returns a vector of the hdvs in the stack that match the predicate"
  [pred hdv]
  (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)]
    (loop [new-v hdv
           i p-count
           result []]
      (if (zero? i)
        (->> (reverse result)
             (remove nil?)) ; reverse because the unprotect does it in stack order
        (do
          (recur (vb/unprotect new-v)
                 (dec i)
                 (conj result (when (not-empty (pred new-v)) new-v))))))))
