(ns gigasquid.vsa-maps
  "clojure map data structures to hyperdimensional vectors"
  (:require [gigasquid.vsa-clj :as vsa-clj]
            [clojure.test :refer [deftest is testing run-tests]]))

(defn vsa-assoc
  "Like clojure assoc but result is bundled hdv. For non nested kvs"
  ([hdv k v]
   (let [mem-k (or (vsa-clj/get-hdv k) (vsa-clj/add-hdv! k))
         mem-v (or (vsa-clj/get-hdv v) (vsa-clj/add-hdv! v))
         mem-hdv (or hdv (vsa-clj/hdv))
         kv (vsa-clj/bind mem-k mem-v)]
     (vsa-clj/bundle mem-hdv kv)))
  ([hdv k v & kvs]
   (let [ret (vsa-assoc hdv k v)]
     (if kvs
       (if (next kvs)
        (recur ret (first kvs) (second kvs) (nnext kvs))
        (throw (ex-info "vsa-assoc expects even number of args"
                        {:args kvs})))
       ret))))

(def STACK_COUNT_KEY :STACK_COUNT_KEY)

(defn vsa-get
  "Like clojure get with a map but with hdv also works with a value
   instead of a k. If passed an idx it will pop the pushed map value."
  ([hdv k]
   (vsa-clj/unbind-get hdv k))
  ([hdv k idx]
   (let [[p-count _] (vsa-get hdv STACK_COUNT_KEY)
         unprotect-num (- (dec p-count) idx)]
     (vsa-clj/unprotect-n hdv unprotect-num))))

(defn map->vsa
  "Turn a clojure map (not nested) into a hdv"
  [m]
  (when-not (empty? m)
    (reduce (fn [ret-hdv [k v]]
              (let [mem-k (or (vsa-clj/get-hdv k) (vsa-clj/add-hdv! k))
                    mem-v (or (vsa-clj/get-hdv v) (vsa-clj/add-hdv! v))
                    kv (vsa-clj/bind mem-k mem-v)]
                (vsa-clj/bundle ret-hdv kv)))
            (vsa-clj/hdv)
            m)))


(defn vsa-init-stack
  "Inits a clojure like vector/stack into the HDV denoted by a key/value pair that indicates the number of elements in the stack - defaults to 0"
  []
  (vsa-assoc (vsa-clj/hdv) STACK_COUNT_KEY 0))

(defn vsa-push
  "Adds (bundles) the target hdv to the base hdv in a stack context but first protects it by rotation. Need to call vsa-init-stack on the base-hdv first"
  [base-hdv target-hdv]
  (let [[p-count _] (vsa-get base-hdv STACK_COUNT_KEY)
        new-p-count (inc p-count)
        protected-base-hdv (vsa-clj/protect base-hdv)]
    (-> protected-base-hdv
        (vsa-assoc STACK_COUNT_KEY new-p-count)
        (vsa-clj/bundle target-hdv))))

;;;; Tests

(deftest test-vsa-assoc
  (testing "testing (assoc nil :x 1)"
      (vsa-clj/reset-hdv-mem!)
      (let [m (vsa-assoc nil :x 1)
            [v _] (vsa-get m :x)
            [k _] (vsa-get m 1)]
        (is (= {:x 1} {k v}))
        (is (= 2 (count @vsa-clj/cleanup-mem)))))

  (testing "testing (assoc hdv :y 1)"
    (vsa-clj/reset-hdv-mem!)
    (let [m1 (vsa-assoc nil :x 1)
          m2 (vsa-assoc m1 :y 2)
          [v1 _] (vsa-get m2 :x)
          [k1 _] (vsa-get m2 1)
          [v2 _] (vsa-get m2 :y)
          [k2 _] (vsa-get m2 2)]
      (is (= {:x 1 :y 2} {k1 v1 k2 v2}))
      (is (= 4 (count @vsa-clj/cleanup-mem)))))

  (testing "(assoc nil :x 1 :y 2 :z 3)"
    (vsa-clj/reset-hdv-mem!)
    (let [m (vsa-assoc nil :x 1 :y 2 :z 3)
          [v1 _] (vsa-get m :x)
          [k1 _] (vsa-get m 1)
          [v2 _] (vsa-get m :y)
          [k2 _] (vsa-get m 2)
          [v3 _] (vsa-get m :z)
          [k3 _] (vsa-get m 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-clj/cleanup-mem))))))


(deftest test-map->vsa
  (testing "{}"
    (vsa-clj/reset-mem!)
    (let [ret-hdv (map->vsa {})]
      (is (nil? ret-hdv))))

  (testing "{:x 1}"
    (vsa-clj/reset-mem!)
    (let [ret-hdv (map->vsa {:x 1})
          [v1 _] (vsa-get ret-hdv :x)
          [k1 _] (vsa-get ret-hdv 1)]
      (is (= {:x 1} {k1 v1}))
      (is (= 2 (count @vsa-clj/cleanup-mem)))))

  (testing "{:x 1 :y 2 :z 3}"
    (vsa-clj/reset-mem!)
    (let [ret-hdv (map->vsa {:x 1 :y 2 :z 3})
          [v1 _] (vsa-get ret-hdv :x)
          [k1 _] (vsa-get ret-hdv 1)
          [v2 _] (vsa-get ret-hdv :y)
          [k2 _] (vsa-get ret-hdv 2)
          [v3 _] (vsa-get ret-hdv :z)
          [k3 _] (vsa-get ret-hdv 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-clj/cleanup-mem))))))

(deftest test-vsa-init-stack
  (vsa-clj/reset-mem!)
  (let [h (map->vsa {:x 1})
        ret-hdv (vsa-clj/bundle (vsa-init-stack) h)
        [v _] (vsa-get ret-hdv STACK_COUNT_KEY)]
    (is (= 0 v))))

(deftest test-vsa-push
  (testing "[{:x 1} {:x 2}]"
    (vsa-clj/reset-mem!)
    (let [ret-v (-> (vsa-init-stack)
                     (vsa-push (map->vsa {:x 1}))
                     (vsa-push (map->vsa {:x 2}))
                     (vsa-push (map->vsa {:x 3})))
          [x3 _] (vsa-get ret-v :x)
          [x2 _] (-> ret-v
                     (vsa-clj/unprotect)
                     (vsa-get :x))
          [x1 _] (-> ret-v
                     (vsa-clj/unprotect)
                     (vsa-clj/unprotect)
                     (vsa-get :x))
          [stack-count _] (vsa-get ret-v STACK_COUNT_KEY)]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3))
      (is (= 3 stack-count))))


  #_(testing "[{:x 1} {:x 2}] with get lookup"
    (vsa-clj/reset-mem!)
    (let [ret-v (-> (vsa-init-stack)
                     (vsa-push (map->vsa {:x 1}))
                     (vsa-push (map->vsa {:x 2})))
          [x1 _] (vsa-get ret-v :x 0)
          [x2 _] (vsa-get ret-v :x 1)]
      (is (= [1 2] [x1 x2]))))


  )


