(ns intro-to-hdv
  (:require
    [gigasquid.vsa-base :as vb]))


;; Following examples in "An Introduction to Hyperdimensional Computing for Robotics"
;; https://link.springer.com/article/10.1007/s13218-019-00623-z


(vb/reset-hdv-mem!)

(vb/add-hdv! :name)
(vb/add-hdv! "Alice")
(vb/add-hdv! :yob)
(vb/add-hdv! 1980)
(vb/add-hdv! :high-score)
(vb/add-hdv! 1000)


(def H
  (-> (vb/bind (vb/get-hdv :name) (vb/get-hdv "Alice"))
      (vb/bundle
        (vb/bind (vb/get-hdv :yob) (vb/get-hdv 1980)))
      (vb/bundle
        (vb/bind (vb/get-hdv :high-score) (vb/get-hdv 1000)))))


(vb/unbind-get H :name)


;; ["Alice" #tech.v3.tensor<int8>[1000000]
;;  [-1 1 1 ... 1 -1 -1]]

(vb/unbind-get H :yob)


;;  [1980 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 -1 -1]]

(vb/unbind-get H :high-score)


;;  [1000 #tech.v3.tensor<int8>[1000000]
;; [-1 -1 1 ... -1 1 1]]

(vb/unbind-get H "Alice")


;; [:name #tech.v3.tensor<int8>[1000000]
;; [-1 1 -1 ... -1 -1 -1]]
