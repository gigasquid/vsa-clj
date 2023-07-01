(ns gigasquid.image
  (:require
    [gigasquid.vsa-base :as vb]
    [tech.v3.tensor :as dtt])
  (:import
    (java.awt
      Color)
    (java.awt.image
      BufferedImage)
    (java.io
      File)
    (javax.imageio
      ImageIO)))


(def dim 100)


;; reshape vector 1000000 into a 1000 by 1000 matix
(defn reshape
  [v]
  (vec (map #(vec %) (partition dim v))))


;; function that given an row and column gets the value from matrix-hdv
(defn get-matrix-value
  [hdv row col]
  (get-in hdv [row col]))


(defn make-image
  [hdv]
  (let [img (BufferedImage. dim dim BufferedImage/TYPE_INT_ARGB)]
    (doseq [i (range dim)
            j (range dim)]
      (let [v (get-matrix-value hdv i j)]
        (.setRGB img i j (cond
                           (pos? v) (.getRGB Color/WHITE)
                           (neg? v) (.getRGB Color/BLACK)
                           (zero? v) (.getRGB Color/GRAY)))))
    img))


(defn write-image-hdv
  [fname hdv]
  (let [img (make-image (reshape hdv))]
    (ImageIO/write img "png" (File. (str fname ".png")))))


;; read image from file into BufferedImage
(defn read-image
  [fname]
  (ImageIO/read (File. fname)))


;; Get the RGB Color from a position in a buffered image
(defn get-rgb
  [img x y]
  (let [img (read-image "seed-key-hdv.png")]))


;; Function to to a 1000x1000 BufferedImage and turn it into a vector of values
(defn image->vector
  [image]
  (vec (for [i (range dim)
             j (range dim)]
         (let [v (.getRGB image i j)]
           (cond (= v (.getRGB Color/WHITE)) 1
                 (= v (.getRGB Color/GRAY)) 0
                 (= v (.getRGB Color/BLACK)) -1)))))


(defn read-image-to-hdv
  [fname]
  (let [img (read-image fname)
        v (image->vector img)]
    (dtt/->tensor v)))


(comment

  (def test-hdv (vb/hdv))

  (write-image-hdv "test-hdv" test-hdv)


  (def seed-image (read-image "seed-key-hdv.png"))
  (image->vector seed-image)


  (read-image-to-hdv "seed-key-hdv.png")

  (write-image-hdv "round-trip-seed-hdv"
   (read-image-to-hdv "seed-key-hdv.png"))

)
