(ns domination.piece-card-2
  (:require
    [domination.see.core :as see]
    [clojure.java.io :as io]
    [domination.support.symbol :as symbol]
    [domination.support.util :as util]
    [domination.support.draw :as draw]
    [domination.data :as data]
    [clojure.string :as string]
    [domination.support.tokens :as tokens]
    [domination.support.texture :as texture]
    [domination.support.stone :as stone])
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics2D Font BasicStroke Color RenderingHints]
    [javax.imageio ImageIO]
    [java.awt.font TextLayout]
    (java.awt.geom Ellipse2D Ellipse2D$Float Rectangle2D Rectangle2D$Double)))

;
;(def TAU (* 2 Math/PI))
;
;(def millis-per-inch 25.4)
;
;(def minion-size-mm millis-per-inch)
;(def shield-size-mm 20) ;TODO: unknown!!1
;
;(def scale 4) ; aka pixels per millimeter
;(defn mm [mm] (* mm scale))
;
;(def minion-size (mm minion-size-mm))
;(def shield-size (mm shield-size-mm))
;
;(def a4-width-mm 297)
;(def a4-height-mm 210)
;

(def inner-width-mm 295)
(def inner-height-mm 185)
;
(def width (+ (util/mm->px inner-width-mm) 100))
(def height (+ (util/mm->px inner-height-mm) 100))

(def total-width-mm (util/px->mm width))
(def total-height-mm (util/px->mm height))

(defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))


(def attributes [#_{:label         "Cost"
                  :character-key :price
                  :colour        (draw/rgb 200 200 200)}
                 {:label         "Spend"
                  :character-key :coin
                  :colour        (draw/rgb 240, 239, 137)}
                 {:label         "Move"
                  :character-key :move
                  :colour        (draw/rgb 127 255 127)}
                 {:label         "Damage"
                  :character-key :damage
                  :colour        (draw/rgb 255 50 50)}
                 {:label         "Range"
                  :character-key :range
                  :colour        (draw/rgb 250 175 0)}
                 {:label         "Shield"
                  :character-key :shield
                  :colour        (draw/rgb 137, 207, 240)}])

(defn draw-piece-card []
  (let [^Graphics2D g (.getGraphics image)
        outline-style (draw/line-style 2)
        outer-rectangle (draw/rectangle 0 0 width height)
        inner-rectangle (draw/rectangle 50 50 (- width 100) (- height 100))
        ]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    ;(.setColor g Color/WHITE)
    ;(.fillRect g 0 0 util/a4-width-mm util/a4-height-mm)

    ;(.setColor g (draw/rgb 120 180 40))
    ;(.fillRect g 0 0 width height)
    (draw/shape g
      (draw/fill-style (draw/rgb 120 180 40))
      outer-rectangle)
    (draw/shape g
      (draw/fill-style texture/grass)
      outer-rectangle)

    (draw/shape g
      (draw/fill-style texture/mud)
      inner-rectangle)

    ;(.setColor g (draw/rgb (rand 255)(rand 255)(rand 255)))
    ;(.fillRect g 0 0 width height)

    #_(draw/shape g
                (draw/fill-style texture/mud)
                (Rectangle2D$Double. 0 0 width height))

    (stone/do-instructions g
      (concat
        (stone/h-wall 50 (+ 50 10) (- width 100) 10 5 5 4)
        (stone/h-wall 50 (+ (- height 50) 10) (- width 100) 10 5 5 4)
        (stone/v-wall 50 (+ 50 10) (- height 100) 7 10 5 3)
        (stone/v-wall (- width 50) (+ 50 10) (- height 100) 7 10 5 3)

        (stone/rings 50 (+ 50 10) [0 5] 20 30 0.8 6)
        (stone/rings (- width 50) (+ 50 10) [0 5] 20 30 0.8 6)
        (stone/rings (- width 50) (+ (- height 50) 10) [0 5] 20 30 0.8 6)
        (stone/rings 50 (+ (- height 50) 10) [0 5] 20 30 0.8 6)))

    (symbol/flag g 50 30 20 0.8)
    (symbol/flag g (- width 50) 30 20 0.8)
    (symbol/flag g 50 (- height 50 20) 20 0.8)
    (symbol/flag g (- width 50) (- height 50 20) 20 0.8)

    (let [shield-spacing (* util/shield-size 11/10)
          minion-spacing (* util/minion-size 11/10)
          x-offset (util/mm->px -3)
          x-spread (util/mm->px 15)
          y-offset (+ (util/mm->px 80) 50)]
      (doseq [[shield cost] {0 1
                             1 2
                             2 3
                             3 4
                             4 5
                             5 6}]
        (let [x (+ x-offset (- (* width 1/2) (* shield shield-spacing) x-spread))]
          (symbol/price-label g cost
                              x (+ y-offset (util/mm->px -20))
                              x (+ y-offset (util/mm->px 85)))
          (doseq [player (range 0 4)]
            (let [y (+ y-offset (* player minion-spacing))]
              (draw/circle g (draw/styles
                               draw/style-shield
                               outline-style) x y (/ util/shield-size 2))
              (symbol/shield g x (- y (util/mm->px 0.5)) (/ util/shield-size 4))))))

      (doseq [[minion cost] {0 1
                             1 2
                             2 4
                             3 6
                             4 8}]
        (let [x (+ x-offset (+ (* width 1/2) (* minion minion-spacing) x-spread))]
          (symbol/price-label g cost
                              x (+ y-offset (util/mm->px -20))
                              x (+ y-offset (util/mm->px 85)))
          (doseq [player (range 0 4)]
            (let [y (+ y-offset (* player minion-spacing))]
              (draw/circle g (nth [(draw/styles draw/style-player-1 outline-style)
                                   (draw/styles draw/style-player-2 outline-style)
                                   (draw/styles draw/style-player-3 outline-style)
                                   (draw/styles draw/style-player-4 outline-style)] player)
                           x y (/ util/minion-size 2))
              (symbol/empty-mask g x (+ y (util/mm->px 0.7)) (/ util/minion-size 4)))))
        ))

    (let [y-offset 50
          x-offset 50
          width (- width x-offset x-offset)]
      (doseq [[character-idx character] (->> data/characters
                                             (sort-by :price)
                                             (map-indexed vector))
              :let [token-image (draw/with-new-image [g2 (BufferedImage. 200 200 BufferedImage/TYPE_INT_ARGB)]
                                  (tokens/draw-token character g2 0 0))
                    ;scale (* width 0.1)
                    inset (* width 0.03)
                    left (+ inset x-offset (* (- width (* 2 inset)) character-idx 1/9))
                    right (+ inset x-offset (* (- width (* 2 inset)) (inc character-idx) 1/9))
                    top (+ (util/mm->px 20) y-offset)
                    bottom (+ top (- right left))
                    ;this-width (- right left)
                    ;this-height (- bottom top)
                    ;mid-x (+ left (/ this-width 2))
                    ;mid-y (+ top (/ this-height 2))
                    r (util/mm->px 12.75)
                    token-shape (draw/ellipse (- (/ (+ left right) 2) r)
                                              (- (/ (+ top bottom) 2) r)
                                              (+ r r)
                                              (+ r r))]]
        (symbol/price-label g (:price character)
                            (/ (+ left right) 2) (+ (util/mm->px 15) y-offset)
                            (/ (+ left right) 2) (+ (util/mm->px 28) y-offset))
        (draw/with-clip g token-shape
          (.drawImage g token-image
                      left top right bottom ; (* scale (inc idx)) (* scale 1) (* scale (inc (inc idx)))
                      0 0 (.getWidth token-image) (.getHeight token-image)
                      nil))
        (draw/shape g outline-style token-shape)
        #_(let [line (last (string/split (:label character) #" "))]
            ;(draw/text g (draw/text-style (util/px 6) Color/BLACK) line mid-x mid-y)
            (draw/line g (draw/line-style 1 (apply draw/rgb (:colour character)))
                       left bottom
                       right bottom))
        ))

    #_(doseq [[attr-idx attribute] (map-indexed vector attributes)
            :let [
                  ;label (:label attribute)
                  scale (* width 0.1)
                  top (+ (util/mm->px 26)
                         (* (util/mm->px 7) (inc attr-idx)))
                  bottom (+ top (* width 0.02))
                  left 0
                  right scale
                  this-width (- right left)
                  this-height (- bottom top)
                  mid-x (+ left (/ this-width 2))
                  mid-y (+ top (/ scale 2))
                  inset (* scale 0.05)]]


      ;(.setColor g (:colour attribute))
      (draw/line g (draw/line-style 1 (:colour attribute))
                 0 (+ mid-y (* width 0.01))
                 width (+ mid-y (* width 0.01)))

      ;(text g plain-font Color/BLACK label mid-x mid-y)

      (doseq [[character-idx character] (->> data/characters
                                             (sort-by :price)
                                             (map-indexed vector))
              :let [
                    ;token-image (tokens/draw-token character)
                    scale (* width 0.1)
                    top (* height 0.75)
                    bottom (+ (* height 0.75) (/ scale 4))
                    left (* scale character-idx 10/9)
                    right (* scale (inc character-idx) 10/9)
                    this-width (- right left)
                    this-height (- bottom top)
                    mid-x (+ left (/ this-width 2))
                    ;mid-y (+ top (/ this-height 2))
                    ]]

        (when-let [value ((:character-key attribute) character)]
          (draw/text g (draw/text-style (util/mm->px 6) Color/BLACK)
                     (str value " " (:label attribute))
                     mid-x mid-y))
        )

      )

    (refresh-fn)
    (ImageIO/write image "png" (io/file "piece-card-2.png"))

    (println (str (Math/round ^Double total-width-mm) " mm X "
                  (Math/round ^Double total-height-mm) " mm"))))

(draw-piece-card)
