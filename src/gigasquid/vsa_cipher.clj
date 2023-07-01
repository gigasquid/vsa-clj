(ns gigasquid.vsa-cipher
  (:require
    [gigasquid.image :as image]
    [gigasquid.vsa-base :as vb]
    [gigasquid.vsa-data :as vd]))


;; keywords for the alphabet and space and an end of message symbol
(def alphabet
  [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z :end-of-message])


(def max-num 4)
(def numbers (range 1 (inc max-num)))
(def key-codes (into alphabet numbers))


(defn add-keys-to-cleanup-mem!
  [seed-hdv]
  (vb/reset-hdv-mem!)
  (doall
    (reduce (fn [v k]
              (let [nv (vb/protect v)]
                (vb/add-hdv! k nv)
                nv))
            seed-hdv
            key-codes)))


(defn encode-message
  [message]
  (when (> (count message) max-num)
    (throw (ex-info "message too long" {:allowed-n max-num})))
  (let [ds (zipmap numbers
                   (conj (->> (mapv str message)
                              (mapv keyword))
                         :end-of-message))]
    (println "Encoding " message " into " ds)
    (vd/clj->vsa ds)))


(defn decode-message
  [msg]
  (let [message-v (reduce (fn [res x]
                            (conj res (first (vd/vsa-get msg x))))
                          []
                          numbers)
        _ (println "decoded message-v " message-v)
        decoded (->> message-v
                     (partition-by #(= % :end-of-message))
                     first
                     (mapv #(if (keyword? %)
                              (name %)
                              (str %)))
                     (apply str))]
    (println "Decoded message is " decoded)
    decoded))


(comment

  (vb/set-size! 1e4)
  (def seed-key-hdv (vb/hdv))
  (add-keys-to-cleanup-mem! seed-key-hdv)
  (image/write-image-hdv "seed-key-hdv" seed-key-hdv)
  
  (def message (encode-message "cats"))
  (image/write-image-hdv "secret-message" message)
  (decode-message message)
  ;=> "cats"

  ;;; reload the keys
  (add-keys-to-cleanup-mem! seed-key-hdv)
  (decode-message message)
  ;=> "cats"
  
  ;; Now loading up from the picture of the seed-key-hdv
  (def loaded-key (image/read-image-to-hdv "seed-key-hdv.png"))
  (add-keys-to-cleanup-mem! loaded-key)
  (decode-message message)
  ;=> "cats"

  ;;; Now loading up the from the picture of the encoded message
  (def loaded-message (image/read-image-to-hdv "secret-message.png"))
  (decode-message loaded-message)
  ;=> "cats"


)

