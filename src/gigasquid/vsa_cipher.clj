(ns gigasquid.vsa-cipher
  (:require
    [gigasquid.vsa-base :as vb]
    [gigasquid.vsa-data :as vd]))


;; keywords for the alphabet and space and an end of message symbol
(def alphabet
  [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z :end-of-message])


(def max-num 10)
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
                          numbers)]
    (->> message-v
         (partition-by #(= % :end-of-message))
         first
         (map name)
         (apply str))))


(comment

  (vb/set-size! 1e6)
  @vb/size



  (add-keys-to-cleanup-mem! (vb/hdv))
  (def message (encode-message "smilethere"))
  (decode-message message)
  "smilethere"
  

)

