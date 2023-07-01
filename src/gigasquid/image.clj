(ns gigasquid.image
  (:require
    [gigasquid.vsa-base :as vb])
  (:import
    (java.awt
      Color)
    (java.awt.image
      BufferedImage)
    (java.io
      File)
    (javax.imageio
      ImageIO)))


(def test-hdv (vb/hdv))


;; reshape vector 1000000 into a 1000 by 1000 matix
(defn reshape
  [v]
  (vec (map #(vec %) (partition 1000 v))))


;; function that given an row and column gets the value from matrix-hdv
(defn get-matrix-value
  [hdv row col]
  (get-in hdv [row col]))


(defn make-image
  [hdv]
  (let [img (BufferedImage. 1000 1000 BufferedImage/TYPE_INT_ARGB)]
    (doseq [i (range 1000)
            j (range 1000)]
      (let [v (get-matrix-value hdv i j)]
        (.setRGB img i j (cond
                           (pos? v) (.getRGB Color/WHITE)
                           (neg? v) (.getRGB Color/BLACK)
                           (zero? v) (.getRGB Color/GRAY)))))
    img))


(comment

  
  (let [img (make-image (reshape test-hdv))]
    (ImageIO/write img "png" (File. "test-hdv.png")))
)
