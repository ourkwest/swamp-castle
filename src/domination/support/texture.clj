(ns domination.support.texture
  (:require [domination.support.draw :as draw])
  (:import (java.awt.image BufferedImage)
           (java.awt TexturePaint Graphics2D Color)
           (java.awt.geom Rectangle2D Rectangle2D$Double Ellipse2D Ellipse2D$Double)))


(defmacro new-texture [[graphics-symbol [width height]] & forms]
  `(let [rectangle# (Rectangle2D$Double. 0 0 ~width ~height)]
     (TexturePaint.
       (draw/with-new-image [~graphics-symbol (BufferedImage. ~width ~height BufferedImage/TYPE_INT_ARGB)]
         ~@forms)
       rectangle#)))

(def grass
  (let [rectangle (Rectangle2D$Double. 0 0 100 100)]
    (TexturePaint.
      (draw/with-new-image [^Graphics2D g2 (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB)]
        ;(draw/shape g2 (draw/fill-style Color/GREEN) rectangle)
        (doseq [x (range 0 100 2)
                y (range 0 100 2)]
          (let [color (draw/rgb 0 (+ 128 (rand-int 128)) 0 50)
                ;rnd #(rand-nth [-1.5 -1 0 1 1.5])
                xr (rand-nth [-1.5 -1 0 1 1.5])
                yr (rand-nth [-1.5 -1 0 1 1.5])]
            (doseq [x-wrap [-100 0 100]
                    y-wrap [-100 0 100]]
              (draw/line g2 (draw/line-style 1 color)
                         (+ x xr x-wrap) (+ y yr y-wrap)
                         (+ x xr x-wrap) (+ y yr y-wrap 10))))))
      rectangle)))

(def mud
  (let [rectangle (Rectangle2D$Double. 0 0 100 100)]
    (TexturePaint.
      (draw/with-new-image [^Graphics2D g2 (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB)]
        ;(draw/shape g2 (draw/fill-style Color/GREEN) rectangle)
        (doseq [x (range 0 100 2)
                y (range 0 100 2)]
          (let [color (draw/rgb-lerp (draw/rgb 100 75 15 128)
                                     (draw/rgb 0 0 0 128)
                                     (rand 0.5))
                ;rnd #(rand-nth [-1.5 -1 0 1 1.5])
                size (rand 20)
                xr (rand 10)
                yr (rand 10)]
            (doseq [x-wrap [-100 0 100]
                    y-wrap [-100 0 100]]
              (draw/shape g2 (draw/line-style (rand 4) color)
                          (Ellipse2D$Double. (+ x xr x-wrap) (+ y yr y-wrap) size (* size 1/2)))))))
      rectangle)))

(defn stone-style []
  (draw/shape-style (draw/rgb-lerp (draw/rgb 215 190 140) Color/BLACK 0.5) ;(draw/rgb 75 75 50)
                    1
                    (-> (draw/rgb 215 190 140)
                        (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                        (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.25)))))

(defn dark-stone-style []
  (draw/shape-style (draw/rgb 100 100 100)
                    1
                    (-> (draw/rgb 215 190 140)
                        (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.1))
                        (draw/rgb-lerp (draw/rgb 0 0 0) (+ 0.6 (rand 0.1))))))

(def stone-slabs
  (let [slab-width 10
        slab-height 5]
    (new-texture [^Graphics2D g [100 100]]
      (doseq [x (range (- slab-width) (+ 100 slab-width) slab-width)
              y (range (- slab-height) (+ 100 slab-height) slab-height)]
        (draw/shape g (dark-stone-style) (draw/rectangle (+ x (if (odd? (/ y slab-height)) slab-height 0)) y slab-width slab-height))))))
