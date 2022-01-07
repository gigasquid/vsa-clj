(ns gigasquid.vsa-clj
  (:require
    [tech.v3.datatype.functional :as dtype-fn]
    [tech.v3.tensor :as dtt]))

;;; Following examples in "An Introduction to Hyperdimensional Computing for Robotics"
;;https://link.springer.com/article/10.1007/s13218-019-00623-z


(def size 1e6)


(defn binary-rand
  []
  (if (> (rand) 0.5) -1 1))


(defn hdv
  []
  (dtt/->tensor (repeatedly size #(binary-rand)) :datatype :int8))


(defn bundle-op
  [v1 v2]
  (dtype-fn/+ v1 v2))


(defn clip
  [v]
  (-> v
      (dtype-fn/min 1)
      (dtype-fn/max -1)))


(defn bundle
  [v1 v2]
  (-> (bundle-op v1 v2)
      (clip)))


(defn bind
  [v1 v2]
  (dtype-fn/* v1 v2))


(defn protect
  [v]
  (dtt/rotate v [1]))


(defn unprotect
  [v]
  (dtt/rotate v [-1]))


(def cleanup-mem (atom {}))


(defn add-to-cleanup-mem
  [k v]
  (swap! cleanup-mem merge {k v}))


(defn query-cleanup-mem
  [query-v]
  (->> @cleanup-mem
       (map (fn [[k v]]
              {k v :dot (dtype-fn/dot-product v query-v)}))
       (sort-by :dot)
       last
       first))


(defn hdv-by-key
  [k]
  (get @cleanup-mem k))


(defn create-khdv
  [k]
  (add-to-cleanup-mem k (hdv)))


(comment

  (create-khdv :name)
  (create-khdv "Alice")
  (create-khdv :yob)
  (create-khdv 1980)
  (create-khdv :high-score)
  (create-khdv 1000)

  (def H (-> (bind (hdv-by-key :name) (hdv-by-key "Alice"))
             (bundle
              (bind (hdv-by-key :yob) (hdv-by-key 1980)))
             (bundle
              (bind (hdv-by-key :high-score) (hdv-by-key 1000)))))

  (query-cleanup-mem (bind H (hdv-by-key :name)))
  ;; ["Alice" #tech.v3.tensor<int8>[1000000]
  ;;  [-1 1 1 ... 1 -1 -1]]

  (query-cleanup-mem (bind H (hdv-by-key :yob)))
 ;;  [1980 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 -1 -1]]

  (query-cleanup-mem (bind H (hdv-by-key :high-score)))
 ;;  [1000 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 1 1]]

  (query-cleanup-mem (bind H (hdv-by-key "Alice")))
 ;; [:name #tech.v3.tensor<int8>[1000000]
;; [-1 1 -1 ... -1 -1 -1]]
  )
