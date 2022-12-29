(ns gigasquid.vsa-data-test
  (:require
    [clojure.test :refer :all]
    [gigasquid.vsa-base :as vb]
    [gigasquid.vsa-data :as sut]))


(deftest test-vsa-assoc
  (testing "testing (assoc nil :x 1)"
    (vb/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1)
          [v _] (sut/vsa-get m :x)
          [k _] (sut/vsa-get m 1)]
      (is (= {:x 1} {k v}))
      (is (= 2 (count @vb/cleanup-mem)))))

  (testing "testing (assoc hdv :y 1)"
    (vb/reset-hdv-mem!)
    (let [m1 (sut/vsa-assoc nil :x 1)
          m2 (sut/vsa-assoc m1 :y 2)
          [v1 _] (sut/vsa-get m2 :x)
          [k1 _] (sut/vsa-get m2 1)
          [v2 _] (sut/vsa-get m2 :y)
          [k2 _] (sut/vsa-get m2 2)]
      (is (= {:x 1 :y 2} {k1 v1 k2 v2}))
      (is (= 4 (count @vb/cleanup-mem)))))

  (testing "(assoc nil :x 1 :y 2 :z 3)"
    (vb/reset-hdv-mem!)
    (let [m (sut/vsa-assoc nil :x 1 :y 2 :z 3)
          [v1 _] (sut/vsa-get m :x)
          [k1 _] (sut/vsa-get m 1)
          [v2 _] (sut/vsa-get m :y)
          [k2 _] (sut/vsa-get m 2)
          [v3 _] (sut/vsa-get m :z)
          [k3 _] (sut/vsa-get m 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vb/cleanup-mem))))))


(deftest test-map->vsa
  (testing "{}"
    (vb/reset-mem!)
    (let [ret-hdv (sut/map->vsa {})]
      (is (nil? ret-hdv))))

  (testing "{:x 1}"
    (vb/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1})
          [v1 _] (sut/vsa-get ret-hdv :x)
          [k1 _] (sut/vsa-get ret-hdv 1)]
      (is (= {:x 1} {k1 v1}))
      (is (= 2 (count @vb/cleanup-mem)))))

  (testing "{:x 1 :y 2 :z 3}"
    (vb/reset-mem!)
    (let [ret-hdv (sut/map->vsa {:x 1 :y 2 :z 3})
          [v1 _] (sut/vsa-get ret-hdv :x)
          [k1 _] (sut/vsa-get ret-hdv 1)
          [v2 _] (sut/vsa-get ret-hdv :y)
          [k2 _] (sut/vsa-get ret-hdv 2)
          [v3 _] (sut/vsa-get ret-hdv :z)
          [k3 _] (sut/vsa-get ret-hdv 3)]
      (is (= {:x 1 :y 2 :z 3} {k1 v1 k2 v2 k3 v3}))
      (is (= 6 (count @vb/cleanup-mem))))))


(deftest test-vsa-stack-vector
  (vb/reset-mem!)
  (let [h (sut/map->vsa {:x 1})
        ret-hdv (vb/bundle (sut/vsa-stack-vector) h)
        [v _] (sut/vsa-get ret-hdv sut/STACK_COUNT_KEY)]
    (is (= 0 v))))


(deftest test-vsa-conj
  (testing "[{:x 1} {:x 2}]"
    (vb/reset-mem!)
    (let [ret-v (-> (sut/vsa-stack-vector)
                    (sut/vsa-conj (sut/map->vsa {:x 1}))
                    (sut/vsa-conj (sut/map->vsa {:x 2}))
                    (sut/vsa-conj (sut/map->vsa {:x 3})))
          [x3 _] (sut/vsa-get ret-v :x)
          [x2 _] (-> ret-v
                     (vb/unprotect)
                     (sut/vsa-get :x))
          [x1 _] (-> ret-v
                     (vb/unprotect)
                     (vb/unprotect)
                     (sut/vsa-get :x))
          [stack-count _] (sut/vsa-get ret-v sut/STACK_COUNT_KEY)]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3))
      (is (= 3 stack-count))))


  (testing "[{:x 1} {:x 2} {:x 3}] with get lookup"
    (vb/reset-mem!)
    (let [ret-v (-> (sut/vsa-stack-vector)
                    (sut/vsa-conj (sut/map->vsa {:x 1}))
                    (sut/vsa-conj (sut/map->vsa {:x 2}))
                    (sut/vsa-conj (sut/map->vsa {:x 3})))
          [x1 _] (sut/vsa-get ret-v :x {:idx 0})
          [x2 _] (sut/vsa-get ret-v :x {:idx 1})
          [x3 _] (sut/vsa-get ret-v :x {:idx 2})]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))


(deftest test-vector->vsa
  (testing "empty vector"
    (vb/reset-mem!)
    (let [ret-v (sut/vector->vsa [])]
      (is (= 0 (first (sut/vsa-get ret-v sut/STACK_COUNT_KEY))))))

  (testing "[{:x 1} {:x 2} {:x 3}]"
    (vb/reset-mem!)
    (let [ret-v (sut/vector->vsa [{:x 1} {:x 2} {:x 3}])
          [x1 _] (sut/vsa-get ret-v :x {:idx 0})
          [x2 _] (sut/vsa-get ret-v :x {:idx 1})
          [x3 _] (sut/vsa-get ret-v :x {:idx 2})]
      (is (= 1 x1))
      (is (= 2 x2))
      (is (= 3 x3)))))


(deftest test-vsa-get
  (testing "with base vector and key"
    (vb/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})]
      (is (= 1 (first (sut/vsa-get base :x))))))

  (testing "with a unknown key"
    (vb/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})]
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"No key found in memory"
            (sut/vsa-get base :r)))))

  (testing "with a key and similarity"
    (vb/reset-hdv-mem!)
    (let [base (sut/map->vsa {:x 1 :y 2 :z 3})
          ;; idx nil and sim = 0.2
          results (sut/vsa-get base :x {:threshold 0.1 :verbose? true})]
      (is (= 1 (count results)))
      (is (= 1 (-> results first (dissoc :dot :cos-sim) ffirst)))
      ;; idx nil sim = 1
      (is (= [] (sut/vsa-get base :x {:threshold 1})))
      ;; idx nil sim=-1 (returns all possible items in mem)
      (is (= 6 (count (sut/vsa-get base :x {:threshold -1}))))))

  (testing "with a key,indx, and similarity"
    (vb/reset-hdv-mem!)
    (let [base (sut/vector->vsa [{:x 1} {:x 2}])
          results (sut/vsa-get base :x {:idx 1 :threshold 0.1})]
      (is (= 1 (count results)))
      (is (= 2 (-> results first ffirst)))
      ;; idx 1 sim = 1
      (is (= [] (sut/vsa-get base :x {:idx 1 :threshold 1})))
      ;; idx 1 sim=0 (returns all possible items in mem) 3 + stack count and 0
      (is (= 5 (count (sut/vsa-get base :x {:idx 1 :threshold -1}))))))

  (testing "comparing a compound value with the vector"
    (vb/reset-hdv-mem!)
    (let [base (sut/clj->vsa {:x :yellow :y :blue :z :red})
          _ (vb/add-hdv! :green (vb/bundle
                                  (vb/get-hdv :yellow)
                                  (vb/get-hdv :blue)))
          results (sut/vsa-get base :green {:threshold 0.1})]
      (is (= #{:x :y} (->> results
                           (map ffirst)
                           (into #{})))))))


(deftest test-clj->vsa
  (vb/reset-hdv-mem!)
  (is (= 1 (-> (sut/clj->vsa {:x 1})
               (sut/vsa-get :x)
               first)))
  (is (= 1 (-> (sut/clj->vsa [{:x 1}])
               (sut/vsa-get :x 0)
               first)))
  (is (thrown-with-msg? Exception #"Data structure not supported"
        (sut/clj->vsa [{:x 1} 9])))
  (is (thrown-with-msg? Exception #"Data structure not supported"
        (sut/clj->vsa 3))))


(deftest test-vsa-inspect
  (vb/reset-hdv-mem!)
  (let [v1 (sut/clj->vsa {:x 1 :y 2})
        v2 (sut/clj->vsa {:a 4 :b 3 :x 8})]
    (is (= #{:x :y 1 2} (sut/vsa-inspect v1)))
    (is (= #{:a :b :x 3 4 8} (sut/vsa-inspect v2)))))


(deftest test-vsa-map
  (vb/reset-hdv-mem!)
  (let [v (sut/clj->vsa [{:a :blue} {:b :green}])]
    (is (= [[:blue] []]
           (sut/vsa-map #(->> (sut/vsa-get % :a {:threshold 0.1})
                              (mapv ffirst))
                        v)))

    (is (= [#{1 :STACK_COUNT_KEY :blue :a}
            #{:green :STACK_COUNT_KEY 2 :b}]
           (sut/vsa-map sut/vsa-inspect v)))))


(deftest test-vsa-filter
  (vb/reset-hdv-mem!)
  (testing "predicate returning true for 1 result"
    (let [v (sut/clj->vsa [{:a :blue} {:b :green}])
          result (sut/vsa-filter #(sut/vsa-get % :a {:threshold 0.1}) v)]
      (is (= 1 (count result)))
      (is (= [#{:a :blue 1 :STACK_COUNT_KEY}]
             (map sut/vsa-inspect result)))))

  (testing "predicate returning false for all"
    (let [v (sut/clj->vsa [{:a :blue} {:b :green}])
          result (sut/vsa-filter #(sut/vsa-get % :a {:threshold 1}) v)]
      (is (= [] result)))))
