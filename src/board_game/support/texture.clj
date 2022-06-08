(ns board-game.support.texture
  (:require [board-game.support.draw :as draw])
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
          (let [cap (+ 50 (rand-int 205))
                color (draw/rgb (rand-int (/ cap 2)) cap (rand-int (/ cap 2)) 50)
                ;rnd #(rand-nth [-1.5 -1 0 1 1.5])
                xr (rand-nth [-1.5 -1 0 1 1.5])
                yr (rand-nth [-1.5 -1 0 1 1.5])]
            (doseq [x-wrap [-100 0 100]
                    y-wrap [-100 0 100]]
              (draw/line g2 (draw/line-style 1 color)
                         (+ x xr x-wrap) (+ y yr y-wrap)
                         (+ x xr x-wrap) (+ y yr y-wrap 10))))))
      rectangle)))

(def grass-2
  (let [rectangle (Rectangle2D$Double. 0 0 100 100)]
    (TexturePaint.
      (draw/with-new-image [^Graphics2D g2 (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB)]
        ;(draw/shape g2 (draw/fill-style Color/GREEN) rectangle)
        (doseq [x (range 0 100 2)
                y (range 0 100 2)]
          (let [cap (+ 50 (rand-int 205))
                color (draw/rgb (rand-int (/ cap 2)) cap (rand-int (/ cap 2)) 50)
                ;rnd #(rand-nth [-1.5 -1 0 1 1.5])
                xr (rand-nth [-1.5 -1 0 1 1.5])
                yr (rand-nth [-1.5 -1 0 1 1.5])]
            (doseq [x-wrap [-100 0 100]
                    y-wrap [-100 0 100]]
              (draw/line g2 (draw/line-style 1 color)
                         (+ x xr x-wrap) (+ y yr y-wrap)
                         (+ x xr x-wrap) (+ y yr y-wrap 10)))))
        (dotimes [_ 10]
          (let [x (rand 100)
                y (rand 100)]
            (draw/line g2 (draw/line-style 5 (Color. 255 255 0 125)) x y x y)))
        (dotimes [_ 4]
          (let [x (rand 100)
                y (rand 100)]
            (draw/line g2 (draw/line-style 3 (Color. 255 140 0)) x y x y))
          )
        )
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

(defn cake [color-a color-b]
  (let [texture-size 200
        cake-color (draw/rgb 200 100 0)
        highlight-color (draw/rgb 255 200 0)
        icing-color (draw/rgb 150 75 0)
        detail-color (draw/rgb 75 50 0)]
    (new-texture [^Graphics2D g [texture-size texture-size]]
      (doseq [x (range 0 texture-size 2)
              y (range 0 texture-size 2)]
        (let [color (draw/rgb-lerp
                      (draw/with-alpha color-a 128)
                      (draw/with-alpha color-b 128)
                      ;(draw/with-alpha cake-color 128)
                      ;             (draw/with-alpha icing-color 128)
                                   ;(draw/with-alpha Color/BLACK 128)
                                   (rand 0.5))
              ;rnd #(rand-nth [-1.5 -1 0 1 1.5])
              size (rand 20)
              xr (rand 10)
              yr (rand 10)]
          (doseq [x-wrap [(- texture-size) 0 texture-size]
                  y-wrap [(- texture-size) 0 texture-size]]
            (draw/shape g (draw/line-style (rand 4) color)
                        (Ellipse2D$Double. (+ x xr x-wrap) (+ y yr y-wrap) size (* size 1/2)))))))))

(let [texture-size 200
      cake-color (draw/rgb 200 100 0)
      highlight-color (draw/rgb 255 200 0)
      icing-color (draw/rgb 150 75 0)
      detail-color (draw/rgb 75 50 0)]
  (def cake-1 (cake icing-color Color/BLACK))
  (def cake-2 (cake cake-color icing-color))
  (def cake-3 (cake cake-color detail-color))

  )

(defn stone-style []
  (draw/shape-style (draw/rgb-lerp (draw/rgb 215 190 140) Color/BLACK 0.5) ;(draw/rgb 75 75 50)
                    1
                    (-> (draw/rgb 215 190 140)
                        (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                        (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.25)))))

(defn shaded-stone-style [shade]
  (draw/shape-style (draw/rgb-lerp (draw/rgb 215 190 140) Color/BLACK 0.5) ;(draw/rgb 75 75 50)
                    1
                    (-> (draw/rgb 215 190 140)
                        (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                        (draw/rgb-lerp (draw/rgb 0 0 0) (+ shade (rand 0.25))))))

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

(def light-stone-slabs
  (let [slab-width 20
        slab-height 10]
    (new-texture [^Graphics2D g [200 200]]
      (doseq [x (range (- slab-width) (+ 200 slab-width) slab-width)
              y (range (- slab-height) (+ 200 slab-height) slab-height)]
        (draw/shape g (stone-style) (draw/rectangle (+ x (if (odd? (/ y slab-height)) slab-height 0)) y slab-width slab-height))))))

(do ; test block

  (require '[see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  (def width 400)
  (def height 400)

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))

  (def g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  (draw/shape g (draw/fill-style grass-2) (draw/rectangle 0 0 200 200))
  (draw/shape g (draw/fill-style cake-3) (draw/rectangle 200 0 200 200))

  (draw/shape g (draw/fill-style stone-slabs) (draw/rectangle 0 200 200 200))
  (draw/shape g (draw/fill-style light-stone-slabs) (draw/rectangle 200 200 200 200))


  (refresh-fn)

  )