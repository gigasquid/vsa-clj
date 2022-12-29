(ns vsa-clojure-data-structures
  (:require
    [gigasquid.vsa-base :as vb]
    [gigasquid.vsa-data :as vd]))


;; First thing is that we need a global memory to remember all of our hdv
(vb/reset-hdv-mem!)


;; Now we can encode a clojure map data structure into a vsa - in the end. Note that EVERYTHING is a hdv, all the symbols and the final data structure itself
(def our-first-vsa-map (vd/clj->vsa {:x 1 :y 2}))


;; What is actually in our memory now?
@vb/cleanup-mem


;; {:x #tech.v3.tensor<int8>[1000000][1 -1 1 ... 1 -1 -1],
;;  1 #tech.v3.tensor<int8>[1000000][1 1 -1 ... -1 -1 -1],
;;  :y #tech.v3.tensor<int8>[1000000][1 1 -1 ... 1 1 1],
;;  2 #tech.v3.tensor<int8>[1000000][-1 -1 -1 ... -1 -1 1]}

;; All the values are expressed as hyperdimensional vectors, they are then combined using vector symbolic algebra to a single vector of the same dimension that represents the map.

;; We can know get the value of a key by the same algebra. It uses the the cleanup memory to find the closest match (with cosine similarity)
(vd/vsa-get our-first-vsa-map :x)


;; =>  [1 #tech.v3.tensor<int8>[1000000][1 1 -1 ... -1 -1 -1]

;; It also works with keys as well
(vd/vsa-get our-first-vsa-map 2)


;; =>  [:y #tech.v3.tensor<int8>[1000000][1 1 -1 ... 1 1 1]

;; We can add onto the map with a new key value pair

(def our-second-vsa-map (vd/vsa-assoc our-first-vsa-map :z 3))

(vd/vsa-get our-second-vsa-map :z)


;; =>  [3 #tech.v3.tensor<int8>[1000000][1 -1 1 ... -1 -1 1]]


;; We can also represent Clojure vectors as vsa data structures as well this is done by rotating the vectors and adding that as a stack
(vb/reset-hdv-mem!)

(def our-first-vsa-vector-of-maps (vd/clj->vsa [{:x 1} {:x 2 :y 3}]))


;; We can get the value of x in the 2nd map by
(vd/vsa-get our-first-vsa-vector-of-maps :x {:idx 1})


;; [2 #tech.v3.tensor<int8>[1000000][-1 1 1 ... 1 -1 1]]

;; Or the first map
(vd/vsa-get our-first-vsa-vector-of-maps :x {:idx 0})


;; =>  [1 #tech.v3.tensor<int8>[1000000][-1 -1 1 ... 1 1 1]]

;; We can also build onto our vector with conj

(def our-second-vsa-vector-of-maps
  (vd/vsa-conj our-first-vsa-vector-of-maps (vd/clj->vsa {:z 5})))


(vd/vsa-get our-second-vsa-vector-of-maps :z {:idx 2})


;; =>  [5 #tech.v3.tensor<int8>[1000000][-1 1 1 ... -1 -1 -1]]


;; So what is cool about this other than everything is a hyperdimensional vector is that we can get fuzzy or simlarity matching inside our regular symbolic code.

(vb/reset-hdv-mem!)


;; For example with this map we have more than one possiblilty of matching
(def vsa-simple-map (vd/clj->vsa {:x 1 :y 1 :z 3}))


;; Let's see all the possible matches
(vd/vsa-get vsa-simple-map :x {:threshold -1 :verbose? true})


;; =>  [{1 #tech.v3.tensor<int8>[1000000]
;;      [1 -1 1 ... -1 -1 -1], :dot 125165.0, :cos-sim 0.1582533568106879}  {:x #tech.v3.tensor<int8>[1000000]
;;      [1 -1 -1 ... -1 -1 1], :dot 2493.0, :cos-sim 0.0031520442498225933} {:z #tech.v3.tensor<int8>[1000000]
;;      [-1 -1 1 ... 1 1 -1], :dot 439.0, :cos-sim 5.550531190020531E-4}    {3 #tech.v3.tensor<int8>[1000000]
;;      [-1 -1 1 ... -1 -1 1], :dot -443.0, :cos-sim -5.601105506102723E-4}  {:y #tech.v3.tensor<int8>[1000000]
;;      [-1 -1 1 ... 1 1 1], :dot -751.0, :cos-sim -9.495327844431478E-4}]

;; We can see that if we were to take the closest one it would be the value of 1 - but we can set the threshold to define our own closeness measure and get more than one result back

;; This can be very useful when defining compound symbolic values. Let's take a map of colors

(vb/reset-hdv-mem!)

(def primary-color-vsa-map (vd/clj->vsa {:x :red :y :yellow :z :blue}))


;; let's add a new compound value to the cleanup memory that is green
(vb/add-hdv! :green (vb/bundle
                      (vb/get-hdv :yellow)
                      (vb/get-hdv :blue)))


;; Now we can query the primary color map for something that is close to green. We get 2 results now.

(vd/vsa-get primary-color-vsa-map :green {:threshold 0.1})


;; =>  [{:z #tech.v3.tensor<int8>[1000000][1 -1 1 ... -1 1 1]}
;;     {:y #tech.v3.tensor<int8>[1000000] [-1 1 1 ... 1 1 1]}]

;; So we have a hdv of things, we can inspect it and compare it to our memory to see what's in it

(vd/vsa-inspect primary-color-vsa-map)


;; Note that it includes green in it since it is a compound value
;; =>  #{:y :yellow :green :z :red :blue :x}

(vd/vsa-inspect (vd/clj->vsa {:x :red}))


;; =>  #{:red :x}


;; Finally if we have a vector (stack) of these maps we can define map and filter functions on them

(def color-vsa-vector-map (vd/clj->vsa [{:x :yellow} {:x :green} {:z :red}]))


(vd/vsa-map #(->> (vd/vsa-get % :yellow {:threshold 0.01})
                  (mapv ffirst))
            color-vsa-vector-map)


;; =>  ([:x] [:x] [])


(->> color-vsa-vector-map
     (vd/vsa-filter #(vd/vsa-get % :yellow {:threshold 0.01}))
     count)


;; =>  2



