(ns gigasquid.vsa-data
  "clojure map data structures to hyperdimensional vectors"
  (:require
    [gigasquid.vsa-base :as vsa-base]))


(def STACK_COUNT_KEY :STACK_COUNT_KEY)


(defn vsa-assoc
  "Like clojure assoc but result is bundled hdv. For non nested kvs"
  ([hdv k v]
   (let [mem-k (or (vsa-base/get-hdv k) (vsa-base/add-hdv! k))
         mem-v (or (vsa-base/get-hdv v) (vsa-base/add-hdv! v))
         mem-hdv (or hdv (vsa-base/hdv))
         kv (vsa-base/bind mem-k mem-v)]
     (vsa-base/bundle mem-hdv kv)))
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
   instead of a k. If passed an idx it will pop the pushed map value."
  ([hdv k]
   (vsa-base/unbind-get hdv k))
  ([hdv k idx]
   (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)
         unprotect-num (- (dec p-count) idx)
         new-v (vsa-base/unprotect-n hdv unprotect-num)]
     (vsa-get new-v k))))


(defn map->vsa
  "Turn a clojure map (not nested) into a hdv"
  [m]
  (when-not (empty? m)
    (reduce (fn [ret-hdv [k v]]
              (let [mem-k (or (vsa-base/get-hdv k) (vsa-base/add-hdv! k))
                    mem-v (or (vsa-base/get-hdv v) (vsa-base/add-hdv! v))
                    kv (vsa-base/bind mem-k mem-v)]
                (vsa-base/bundle ret-hdv kv)))
            (vsa-base/hdv)
            m)))


(defn vsa-stack-vector
  "Inits a clojure like vector/stack into the HDV denoted by a key/value pair that indicates the number of elements in the stack - defaults to 0"
  []
  (vsa-assoc (vsa-base/hdv) STACK_COUNT_KEY 0))


(defn vsa-conj
  "Adds (bundles) the target hdv to the base hdv in a stack context but first protects it by rotation. Need to call vsa-init-stack on the base-hdv first"
  [base-hdv target-hdv]
  (let [[p-count _] (vsa-get base-hdv STACK_COUNT_KEY)
        new-p-count (inc p-count)
        protected-base-hdv (vsa-base/protect base-hdv)]
    (-> protected-base-hdv
        (vsa-assoc STACK_COUNT_KEY new-p-count)
        (vsa-base/bundle target-hdv))))


(defn vector->vsa
  "Takes a clojure vector data structure filled with maps  (non-nested) and turns it into a vsa data structure"
  [v]
  (reduce (fn [hdv x]
            (vsa-conj hdv (map->vsa x)))
          (vsa-stack-vector)
          v))


(defn vsa-map-get
  "Queries a HDV and returns all the stack items with the given k or v"
  [hdv k]
  (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)]
    (mapv (fn [i]
            (vsa-get hdv k i))
          (range p-count))))


(comment

  (def x (map->vsa {:x 1 :y 2 :z 3}))
  (vsa-base/query-cleanup-mem-verbose (last (vsa-get x :x)))

  )
