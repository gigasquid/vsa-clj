(ns gigasquid.vsa-data-test
  (:require
    [clojure.test :refer :all]
    [gigasquid.vsa-base :as vsa-base]
    [gigasquid.vsa-data :as sut]))


(deftest test-vsa-assoc
  (testing "testing (assoc nil :x 1)"
    (vsa-base/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1)
          [v _] (sut/vsa-get m :x)
          [k _] (sut/vsa-get m 1)]
      (is (= {:x 1} {k v}))
      (is (= 2 (count @vsa-base/cleanup-mem)))))

  (testing "testing (assoc hdv :y 1)"
    (vsa-base/reset-hdv-mem!)
    (let [m1 (sut/vsa-assoc nil :x 1)
          m2 (sut/vsa-assoc m1 :y 2)
          [v1 _] (sut/vsa-get m2 :x)
          [k1 _] (sut/vsa-get m2 1)
          [v2 _] (sut/vsa-get m2 :y)
          [k2 _] (sut/vsa-get m2 2)]
      (is (= {:x 1 :y 2} {k1 v1 k2 v2}))
      (is (= 4 (count @vsa-base/cleanup-mem)))))

  (testing "(assoc nil :x 1 :y 2 :z 3)"
    (vsa-base/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1 :y 2 :z 3)
          [v1 _] (sut/vsa-get m :x)
          [k1 _] (sut/vsa-get m 1)
          [v2 _] (sut/vsa-get m :y)
          [k2 _] (sut/vsa-get m 2)
          [v3 _] (sut/vsa-get m :z)
          [k3 _] (sut/vsa-get m 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-base/cleanup-mem))))))


(deftest test-map-vsa
  (testing "{}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {})]
      (is (nil? ret-hdv))))

  (testing "{:x 1}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1})
          [v1 _] (sut/vsa-get ret-hdv :x)
          [k1 _] (sut/vsa-get ret-hdv 1)]
      (is (= {:x 1} {k1 v1}))
      (is (= 2 (count @vsa-base/cleanup-mem)))))

  (testing "{:x 1 :y 2 :z 3}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1 :y 2 :z 3})
          [v1 _] (sut/vsa-get ret-hdv :x)
          [k1 _] (sut/vsa-get ret-hdv 1)
          [v2 _] (sut/vsa-get ret-hdv :y)
          [k2 _] (sut/vsa-get ret-hdv 2)
          [v3 _] (sut/vsa-get ret-hdv :z)
          [k3 _] (sut/vsa-get ret-hdv 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-base/cleanup-mem))))))


(deftest test-vsa-stack-vector
  (vsa-base/reset-mem!)
  (let [h (sut/map->vsa {:x 1})
        ret-hdv (vsa-base/bundle (sut/vsa-stack-vector) h)
        [v _] (sut/vsa-get ret-hdv sut/STACK_COUNT_KEY)]
    (is (= 0 v))))


(deftest test-vsa-conj
  (testing "[{:x 1} {:x 2}]"
    (vsa-base/reset-mem!)
    (let [ret-v (-> (sut/vsa-stack-vector)
                    (sut/vsa-conj (sut/map->vsa {:x 1}))
                    (sut/vsa-conj (sut/map->vsa {:x 2}))
                    (sut/vsa-conj (sut/map->vsa {:x 3})))
          [x3 _] (sut/vsa-get ret-v :x)
          [x2 _] (-> ret-v
                     (vsa-base/unprotect)
                     (sut/vsa-get :x))
          [x1 _] (-> ret-v
                     (vsa-base/unprotect)
                     (vsa-base/unprotect)
                     (sut/vsa-get :x))
          [stack-count _] (sut/vsa-get ret-v sut/STACK_COUNT_KEY)]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3))
      (is (= 3 stack-count))))


  (testing "[{:x 1} {:x 2} {:x 3}] with get lookup"
    (vsa-base/reset-mem!)
    (let [ret-v (-> (sut/vsa-stack-vector)
                    (sut/vsa-conj (sut/map->vsa {:x 1}))
                    (sut/vsa-conj (sut/map->vsa {:x 2}))
                    (sut/vsa-conj (sut/map->vsa {:x 3})))
          [x1 _] (sut/vsa-get ret-v :x 0)
          [x2 _] (sut/vsa-get ret-v :x 1)
          [x3 _] (sut/vsa-get ret-v :x 2)]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))


(deftest test-vector->vsa
  (testing "empty vector"
    (vsa-base/reset-mem!)
    (let [ret-v (sut/vector->vsa [])]
      (is (= 0 (first (sut/vsa-get ret-v sut/STACK_COUNT_KEY))))))

  (testing "[{:x 1} {:x 2} {:x 3}]"
    (vsa-base/reset-mem!)
    (let [ret-v (sut/vector->vsa [{:x 1} {:x 2} {:x 3}])
          [x1 _] (sut/vsa-get ret-v :x 0)
          [x2 _] (sut/vsa-get ret-v :x 1)
          [x3 _] (sut/vsa-get ret-v :x 2)]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))