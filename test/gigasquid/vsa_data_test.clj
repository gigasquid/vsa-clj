(ns gigasquid.vsa-data-test
  (:require
    [clojure.test :refer :all]
    [gigasquid.vsa-base :as vsa-base]
    [gigasquid.vsa-data :as sut]))


(deftest test-vsa-assoc
  (testing "testing (assoc nil :x 1)"
    (vsa-base/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1)
          [v _] (sut/v-get m :x)
          [k _] (sut/v-get m 1)]
      (is (= {:x 1} {k v}))
      (is (= 2 (count @vsa-base/cleanup-mem)))))

  (testing "testing (assoc hdv :y 1)"
    (vsa-base/reset-hdv-mem!)
    (let [m1 (sut/vsa-assoc nil :x 1)
          m2 (sut/vsa-assoc m1 :y 2)
          [v1 _] (sut/v-get m2 :x)
          [k1 _] (sut/v-get m2 1)
          [v2 _] (sut/v-get m2 :y)
          [k2 _] (sut/v-get m2 2)]
      (is (= {:x 1 :y 2} {k1 v1 k2 v2}))
      (is (= 4 (count @vsa-base/cleanup-mem)))))

  (testing "(assoc nil :x 1 :y 2 :z 3)"
    (vsa-base/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1 :y 2 :z 3)
          [v1 _] (sut/v-get m :x)
          [k1 _] (sut/v-get m 1)
          [v2 _] (sut/v-get m :y)
          [k2 _] (sut/v-get m 2)
          [v3 _] (sut/v-get m :z)
          [k3 _] (sut/v-get m 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-base/cleanup-mem))))))


(deftest test-map->vsa
  (testing "{}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {})]
      (is (nil? ret-hdv))))

  (testing "{:x 1}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1})
          [v1 _] (sut/v-get ret-hdv :x)
          [k1 _] (sut/v-get ret-hdv 1)]
      (is (= {:x 1} {k1 v1}))
      (is (= 2 (count @vsa-base/cleanup-mem)))))

  (testing "{:x 1 :y 2 :z 3}"
    (vsa-base/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1 :y 2 :z 3})
          [v1 _] (sut/v-get ret-hdv :x)
          [k1 _] (sut/v-get ret-hdv 1)
          [v2 _] (sut/v-get ret-hdv :y)
          [k2 _] (sut/v-get ret-hdv 2)
          [v3 _] (sut/v-get ret-hdv :z)
          [k3 _] (sut/v-get ret-hdv 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vsa-base/cleanup-mem))))))


(deftest test-vsa-stack-vector
  (vsa-base/reset-mem!)
  (let [h (sut/map->vsa {:x 1})
        ret-hdv (vsa-base/bundle (sut/vsa-stack-vector) h)
        [v _] (sut/v-get ret-hdv sut/STACK_COUNT_KEY)]
    (is (= 0 v))))


(deftest test-vsa-conj
  (testing "[{:x 1} {:x 2}]"
    (vsa-base/reset-mem!)
    (let [ret-v (-> (sut/vsa-stack-vector)
                    (sut/vsa-conj (sut/map->vsa {:x 1}))
                    (sut/vsa-conj (sut/map->vsa {:x 2}))
                    (sut/vsa-conj (sut/map->vsa {:x 3})))
          [x3 _] (sut/v-get ret-v :x)
          [x2 _] (-> ret-v
                     (vsa-base/unprotect)
                     (sut/v-get :x))
          [x1 _] (-> ret-v
                     (vsa-base/unprotect)
                     (vsa-base/unprotect)
                     (sut/v-get :x))
          [stack-count _] (sut/v-get ret-v sut/STACK_COUNT_KEY)]
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
          [x1 _] (sut/v-get ret-v :x {:idx 0})
          [x2 _] (sut/v-get ret-v :x {:idx 1})
          [x3 _] (sut/v-get ret-v :x {:idx 2})]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))


(deftest test-vector->vsa
  (testing "empty vector"
    (vsa-base/reset-mem!)
    (let [ret-v (sut/vector->vsa [])]
      (is (= 0 (first (sut/v-get ret-v sut/STACK_COUNT_KEY))))))

  (testing "[{:x 1} {:x 2} {:x 3}]"
    (vsa-base/reset-mem!)
    (let [ret-v (sut/vector->vsa [{:x 1} {:x 2} {:x 3}])
          [x1 _] (sut/v-get ret-v :x {:idx 0})
          [x2 _] (sut/v-get ret-v :x {:idx 1})
          [x3 _] (sut/v-get ret-v :x {:idx 2})]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))


(deftest test-v-get
  (testing "with base vector and key"
    (vsa-base/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})]
      (is (= 1 (first (sut/v-get base :x))))))

  (testing "with a unknown key"
    (vsa-base/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})]
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"No key found in memory"
            (sut/v-get base :r)))))

  (testing "with a key and similarity"
    (vsa-base/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})
          ;; idx nil and sim = 0.2
          results (sut/v-get base :x {:threshold 0.1})]
      (is (= 1 (count results)))
      (is (= 1 (-> results first (dissoc :dot :cos-sim) ffirst)))
      ;; idx nil sim = 1
      (is (= [] (sut/v-get base :x {:threshold 1})))
      ;; idx nil sim=-1 (returns all possible items in mem)
      (is (= 6 (count (sut/v-get base :x {:threshold -1}))))))

  (testing "with a key,indx, and similarity"
    (vsa-base/reset-hdv-mem!)
    (let [base (sut/vector->vsa [{:x 1} {:x 2}])
          results (sut/v-get base :x {:idx 1 :threshold 0.1})]
      (is (= 1 (count results)))
      (is (= 2 (-> results first (dissoc :dot :cos-sim) ffirst)))
      ;; idx 1 sim = 1
      (is (= [] (sut/v-get base :x {:idx 1 :threshold 1})))
      ;; idx 1 sim=0 (returns all possible items in mem) 3 + stack count and 0
      (is (= 5 (count (sut/v-get base :x {:idx 1 :threshold -1}))))))

  (testing "comparing a compound value with the vector"
    (vsa-base/reset-hdv-mem!)
    (let [base (sut/clj->vsa {:x :yellow :y :blue :z :red})
          _ (vsa-base/add-hdv! :green (vsa-base/bundle
                                        (vsa-base/get-hdv :yellow)
                                        (vsa-base/get-hdv :blue)))
          results (sut/v-get base :green {:threshold 0.1})]
      (is (= [:x :y] (sut/sim-result-keys results))))))


(deftest test-clj->vsa
  (vsa-base/reset-hdv-mem!)
  (is (= 1 (-> (sut/clj->vsa {:x 1})
               (sut/v-get :x)
               first)))
  (is (= 1 (-> (sut/clj->vsa [{:x 1}])
               (sut/v-get :x 0)
               first)))
  (is (thrown-with-msg? Exception #"Data structure not supported"
        (sut/clj->vsa [{:x 1} 9])))
  (is (thrown-with-msg? Exception #"Data structure not supported"
        (sut/clj->vsa 3))))


(deftest test-inspect
  (vsa-base/reset-hdv-mem!)
  (let [v1 (sut/clj->vsa {:x 1 :y 2})
        v2 (sut/clj->vsa {:a 4 :b 3 :x 8})]
    (is (= #{:x :y 1 2} (sut/inspect v1)))
    (is (= #{:a :b :x 3 4 8} (sut/inspect v2)))))


(deftest test-v-map
  (vsa-base/reset-hdv-mem!)
  (let [v (sut/clj->vsa [{:a :blue} {:b :green}])]
    (is (= [[:blue] []]
           (sut/v-map #(->> (sut/v-get % :a {:threshold 0.1})
                            (sut/sim-result-keys))
                      v)))

    (is (= [#{1 :STACK_COUNT_KEY :blue :a}
            #{:green :STACK_COUNT_KEY 2 :b}]
           (sut/v-map sut/inspect v)))))
