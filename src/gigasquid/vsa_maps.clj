(ns gigasquid.vsa-maps
  "clojure map data structures to hyperdimensional vectors"
  (:require [gigasquid.vsa-clj :as vsa-clj]
            [clojure.test :refer [deftest is testing run-tests]]))


;;; hdv to respresent a clojure nil
(vsa-clj/add-hdv! :nil)

(defn vsa-assoc
  "Like clojure assoc but result is bundled hdv"
  [hdv k v]
  (let [mem-k (or (vsa-clj/get-hdv k) (vsa-clj/add-hdv! k))
        mem-v (or (vsa-clj/get-hdv v) (vsa-clj/add-hdv! v))
        kv (vsa-clj/bind mem-k mem-v)]
    (vsa-clj/bundle (or hdv (vsa-clj/get-hdv :nil)) kv)))

(defn vsa-get
  "Like clojure get with a map but with hdv also works with a value
   instead of a k"
  [hdv k]
  (vsa-clj/unbind-get hdv k))



;;;; Tests

(deftest test-vsa-assoc
  (testing
      "testing (assoc nil :x 1)"
      (let [mem-count (count @vsa-clj/cleanup-mem)
         m (vsa-assoc nil :x 1)
         [v _] (vsa-get m :x)
         [k _] (vsa-get m 1)]
     (is (= {:x 1} {k v}))
     (is (= mem-count (count @vsa-clj/cleanup-mem)))))

  (testing
      "testing (assoc hdv :y 1)"
    (let [m1 (vsa-assoc nil :x 1)
          m2 (vsa-assoc m1 :y 2)
          [v1 _] (vsa-get m2 :x)
          [k1 _] (vsa-get m2 1)
          [v2 _] (vsa-get m2 :y)
          [k2 _] (vsa-get m2 2)]
      (is (= {:x 1 :y 2} {k1 v1 k2 v2}))))


  )






