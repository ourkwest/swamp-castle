(ns board-game.images.box
  (:require
    [board-game.support.util :as util]
    [board-game.support.draw :as draw]
    [board-game.support.symbol :as symbol]
    [clojure.java.io :as io])
  (:import (java.awt Graphics2D Color)
           (java.awt.geom RectangularShape AffineTransform)
           (javax.imageio ImageIO)))


; inner: 280mm x 190mm x 48mm
; outer: 287mm x 197mm x 51mm

(def box-length-mm 287)
(def box-width-mm 197)
(def box-height-mm 51)

(def box-length (util/mm->px box-length-mm))
(def box-width (util/mm->px box-width-mm))
(def box-height (util/mm->px box-height-mm))

(def margin (int (* box-height 1/3)))

(def width (+ box-length box-height box-height margin margin))
(def height (+ box-width box-height box-height margin margin))



(defn enlarge [^RectangularShape rectangle amount]
  (draw/rectangle (- (.getX rectangle) amount)
                  (- (.getY rectangle) amount)
                  (+ (.getWidth rectangle) amount amount)
                  (+ (.getHeight rectangle) amount amount)))

(defn draw-alignment-marks [^Graphics2D g]
  (let [style (draw/line-style 3 Color/RED)]
    (draw/line g style 0 (+ margin box-height) (* margin 1/2) (+ margin box-height))
    (draw/line g style 0 (+ margin box-height box-width) (* margin 1/2) (+ margin box-height box-width))
    (draw/line g style
               (+ margin box-height box-length box-height (* margin 1/2))
               (+ margin box-height)
               (+ margin box-height box-length box-height margin)
               (+ margin box-height))
    (draw/line g style
               (+ margin box-height box-length box-height (* margin 1/2))
               (+ margin box-height box-width)
               (+ margin box-height box-length box-height margin)
               (+ margin box-height box-width))
    (draw/line g style (+ margin box-height) 0 (+ margin box-height) (* margin 1/2))
    (draw/line g style (+ margin box-height box-length) 0 (+ margin box-height box-length) (* margin 1/2))
    (draw/line g style
               (+ margin box-height)
               (+ margin box-height box-width box-height (* margin 1/2))
               (+ margin box-height)
               (+ margin box-height box-width box-height margin))
    (draw/line g style
               (+ margin box-height box-length)
               (+ margin box-height box-width box-height (* margin 1/2))
               (+ margin box-height box-length)
               (+ margin box-height box-width box-height margin))))

(defn draw-box-bottom [^Graphics2D g flip?]
  (let [img (ImageIO/read (io/file "/home/peter/personal/allrgb/clj-renders/Render_178-edited.png"))
        w (.getWidth img)
        h (.getHeight img)
        scale 0.45
        sw (* w scale)
        sh (* h scale)
        x (int (- (* width 1/2) (* sw 1/2)))
        y (int (- (* height 1/2) (* sh 1/2)))]
    (if flip?
      (.drawImage g img x y (+ x sw) (+ y sh) 0 h w 0 nil)
      (.drawImage g img x y (+ x sw) (+ y sh) 0 0 w h nil)))
  (let [step-size 0.1
        xc (* width 1/2)
        yc (* height 1/2)]
    (loop [a 0
           b (rand step-size)]
      (let [x1 (* height (Math/sin a))
            y1 (* height (Math/cos a))
            x2 (* height (Math/sin b))
            y2 (* height (Math/cos b))]
        (draw/shape g (draw/fill-style (draw/rgb 255 255 255 (rand 100)))
                    (draw/poly
                      (map (partial draw/v+ [xc yc])
                           [[x1 y1] [x2 y2] [(- x2) (- y2)] [(- x1) (- y1)]]))))
      (when (< b util/TAU)
        (recur b (+ b (rand step-size))))))
  (draw-alignment-marks g))

(defn draw-box [^Graphics2D g]
  (let [top (draw/rectangle (+ margin box-height) (+ margin box-height) box-length box-width)
        short-side-1 (draw/rectangle margin (+ margin box-height) box-height box-width)
        short-side-2 (draw/rectangle (+ margin box-height box-length) (+ margin box-height) box-height box-width)
        long-side-1 (draw/rectangle (+ margin box-height) margin box-length box-height)
        long-side-2 (draw/rectangle (+ margin box-height) (+ margin box-height box-width) box-length box-height)
        net (draw/shape-add top
                            (enlarge short-side-1 margin)
                            (enlarge short-side-2 margin)
                            (enlarge long-side-1 margin)
                            (enlarge long-side-2 margin))]

    ;(draw/with-clip g net
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/CYAN)
    ;              net))
    ;
    ;(draw/with-clip g top
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/YELLOW)
    ;              top))
    ;
    ;(draw/with-clip g short-side-1
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/RED)
    ;              short-side-1))
    ;
    ;(draw/with-clip g short-side-2
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/RED)
    ;              short-side-2))
    ;
    ;(draw/with-clip g long-side-1
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/GREEN)
    ;              long-side-1))
    ;
    ;(draw/with-clip g long-side-2
    ;  (draw/shape g (draw/shape-style Color/BLACK 20 Color/GREEN)
    ;              long-side-2))

    (draw-box-bottom g false)

    #_(let [img (ImageIO/read (io/file "/home/peter/personal/allrgb/clj-renders/Render_178-edited.png"))
          w (.getWidth img)
          h (.getHeight img)
          scale 0.45
          sw (* w scale)
          sh (* h scale)
          x (int (- (* width 1/2) (* sw 1/2)))
          y (int (- (* height 1/2) (* sh 1/2)))]
      (.drawImage g img x y (+ x sw) (+ y sh) 0 0 w h nil))


    (draw/with-transform g (draw/transforms
                             (AffineTransform/getRotateInstance (* util/TAU -1/4))
                             (AffineTransform/getTranslateInstance (+ margin box-height (* box-length 1/2))
                                                                   (+ margin box-height (* box-width 1/2))))

      #_(let [step-size 0.1]
        (loop [a 0
               b (rand step-size)]

          (let [x1 (* height (Math/sin a))
                y1 (* height (Math/cos a))
                x2 (* height (Math/sin b))
                y2 (* height (Math/cos b))]
            (draw/shape g (draw/fill-style (draw/rgb 255 255 255 (rand 100)))
                        (draw/poly [[x1 y1] [x2 y2] [(- x2) (- y2)] [(- x1) (- y1)]])))

          (when (< b util/TAU)
            (recur b (+ b (rand step-size))))))

      (symbol/cake g 0 0 (* box-width 0.4) 10 10)

      (let [text-shape (draw/text->shape g (draw/text-style 150 Color/WHITE :bold!) "Cakewalk" -390 -720)]
        (doseq [n [10 7 5 3 1]]
          (draw/shape g (draw/shape-style Color/BLACK n (draw/rgb 255 255 255 100)) text-shape)))


      (let [stripe-top (* box-length 0.39)
            draw-text (fn [text line-no]
                        (draw/shape g (draw/fill-style Color/WHITE)
                                    (draw/center (draw/text->shape g (draw/text-style 20 Color/WHITE) text)
                                                 0 (+ stripe-top
                                                      (* line-no (* box-length 0.02))))))]
      (draw/shape g (draw/shape-style Color/BLACK 2 (draw/rgb 0 0 0 150))
                  (draw/rectangle -10000 stripe-top 20000 (* box-length 0.08)))
        ;(draw-text util/tagline 1)
        (draw-text "Version 0.1 (Beta)" 1)
        (draw-text util/copyright-text 3)
        )
      ;(draw/shape g (draw/fill-style Color/WHITE)
      ;            (draw/text->shape g (draw/text-style 20 Color/WHITE) "hello" 0 (* box-length 0.35)))
      ;(draw/shape g (draw/fill-style Color/WHITE)
      ;            (draw/center (draw/text->shape g (draw/text-style 20 Color/WHITE) "Version 0.1 (Beta)")
      ;                         0 (* box-length 0.35)))

      ;(draw/text g (draw/text-style 20 Color/WHITE)
      ;           "hello" 0 (* box-length 0.35))

      #_(draw/text g (draw/text-style 20 Color/WHITE)
                 "Version 0.1 (Beta)" 0 (* box-length 0.35))

      )

    ;(draw/shape g (draw/line-style) top)
    ;(draw/shape g (draw/line-style) short-side-1)
    ;(draw/shape g (draw/line-style) short-side-2)
    ;(draw/shape g (draw/line-style) long-side-1)
    ;(draw/shape g (draw/line-style) long-side-2)
    ;
    ;(draw/line g (draw/line-style 3 Color/RED) 0 (+ margin box-height) (* margin 1/2) (+ margin box-height))
    ;(draw/line g (draw/line-style 3 Color/RED) 0 (+ margin box-height box-width) (* margin 1/2) (+ margin box-height box-width))
    ;(draw/line g (draw/line-style 3 Color/RED)
    ;           (+ margin box-height box-length box-height (* margin 1/2))
    ;           (+ margin box-height)
    ;           (+ margin box-height box-length box-height margin)
    ;           (+ margin box-height))
    ;(draw/line g (draw/line-style 3 Color/RED)
    ;           (+ margin box-height box-length box-height (* margin 1/2))
    ;           (+ margin box-height box-width)
    ;           (+ margin box-height box-length box-height margin)
    ;           (+ margin box-height box-width))
    ;(draw/line g (draw/line-style 3 Color/RED) (+ margin box-height) 0 (+ margin box-height) (* margin 1/2))
    ;(draw/line g (draw/line-style 3 Color/RED) (+ margin box-height box-length) 0 (+ margin box-height box-length) (* margin 1/2))
    ;(draw/line g (draw/line-style 3 Color/RED)
    ;           (+ margin box-height)
    ;           (+ margin box-height box-width box-height (* margin 1/2))
    ;           (+ margin box-height)
    ;           (+ margin box-height box-width box-height margin))
    ;(draw/line g (draw/line-style 3 Color/RED)
    ;           (+ margin box-height box-length)
    ;           (+ margin box-height box-width box-height (* margin 1/2))
    ;           (+ margin box-height box-length)
    ;           (+ margin box-height box-width box-height margin))

    ))

(draw/new-image-file util/dir-to-print "box-top"
  width
  height
  draw-box)

(draw/new-image-file util/dir-to-print "box-bottom"
  width
  height
  #(draw-box-bottom % true))

(do ; test block

  (require '[see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  ;(def width (+ box-length box-height box-height margin margin))
  ;(def height (+ box-width box-height box-height margin margin))

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))

  (def g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  (draw-box g)

  (refresh-fn)

  )