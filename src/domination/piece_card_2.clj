(ns domination.piece-card-2
  (:require
    [domination.see.core :as see]
    [clojure.java.io :as io]
    [domination.support.symbol :as symbol]
    [domination.support.util :as util]
    [domination.support.draw :as draw]
    [domination.data :as data]
    [clojure.string :as string]
    [domination.token-images :as tokens])
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics2D Font BasicStroke Color RenderingHints]
    [javax.imageio ImageIO]
    [java.awt.font TextLayout]))

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
(def width-mm (double util/a4-width-mm))
(def height-mm (double util/a4-height-mm))
;
(def width (util/mm->px width-mm))
(def height (util/mm->px height-mm))
;
;(defn clamp [lower x upper]
;  (-> x
;      (max lower)
;      (min upper)))
;
;(defn rgb
;  ([r g b]
;   (Color. (int (clamp 0 r 255))
;           (int (clamp 0 g 255))
;           (int (clamp 0 b 255))))
;  ([r g b a]
;   (Color. (int (clamp 0 r 255))
;           (int (clamp 0 g 255))
;           (int (clamp 0 b 255))
;           (int (clamp 0 a 255)))))
;
;(def shield-color (rgb 50 200 255))
;(def player-1-color Color/GREEN)
;(def player-2-color Color/BLUE)
;(def player-3-color Color/YELLOW)
;(def player-4-color Color/RED)
;
;(let [font-size (* scale 6)]
;  (def bold-font (Font. nil Font/BOLD font-size))
;  (def plain-font (Font. nil Font/PLAIN font-size)))

(defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))

;
;(defn text [^Graphics2D g ^Font font colour ^String string x y]
;  (let [ ;frc (FontRenderContext. (AffineTransform.) (boolean true) (boolean true))
;        frc (.getFontRenderContext g)
;        text-layout (TextLayout. string font frc)
;        bounds (.getPixelBounds text-layout frc 0 0)
;        gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
;        gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
;    (.setColor g colour)
;    (.draw text-layout g (float gx) (float gy))))
;
;(defn circle [^Graphics2D g x y r stroke-width stroke fill]
;  (let [gx (- x r)
;        gy (- y r)
;        size (* 2 r)]
;    (when fill
;      (.setColor g fill)
;      (.fillArc g gx gy size size 0 360))
;    (when stroke
;      (.setColor g stroke)
;      (.setStroke g (BasicStroke. stroke-width))
;      (.drawArc g gx gy size size 0 360))))


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
  (let [^Graphics2D g (.getGraphics image)]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 width height)

    (let [shield-spacing (* util/shield-size 11/10)
          minion-spacing (* util/minion-size 11/10)
          x-offset 0
          y-offset (util/mm->px 5)
          x-spread (util/mm->px 17)]
      (doseq [[shield cost] {0 1
                             1 2
                             2 3
                             3 4
                             4 5
                             5 6}]
        (let [x (+ x-offset (- (* width 1/2) (* shield shield-spacing) x-spread))]
          (symbol/price-label g cost
                              x (+ y-offset (- (* height 1/2) (util/mm->px 20)))
                              x (+ y-offset (+ (* height 1/2) (util/mm->px 85))))
          (doseq [player (range 0 4)]
            (let [y (+ y-offset (* height 1/2) (* player minion-spacing))]
              (draw/circle g draw/style-shield x y (/ util/shield-size 2))))))

      (doseq [[minion cost] {0 1
                             1 2
                             2 4
                             3 6
                             4 8}]
        (let [x (+ x-offset (+ (* width 1/2) (* minion minion-spacing) x-spread))]
          (symbol/price-label g cost
                              x (+ y-offset (- (* height 1/2) (util/mm->px 20)))
                              x (+ y-offset (+ (* height 1/2) (util/mm->px 85))))
          (doseq [player (range 0 4)]
            (let [y (+ y-offset (* height 1/2) (* player minion-spacing))]
              (draw/circle g (nth [draw/style-player-1
                                   draw/style-player-2
                                   draw/style-player-3
                                   draw/style-player-4] player)
                           x y (/ util/minion-size 2)))))

        ))

    (doseq [[character-idx character] (->> data/characters
                                           (sort-by :price)
                                           (map-indexed vector))
            :let [token-image (tokens/draw-token character)
                  ;scale (* width 0.1)
                  left (* width character-idx 1/9)
                  right (* width (inc character-idx) 1/9)
                  top (util/mm->px 12)
                  bottom (+ top (- right left))
                  ;this-width (- right left)
                  ;this-height (- bottom top)
                  ;mid-x (+ left (/ this-width 2))
                  ;mid-y (+ top (/ this-height 2))
                  ]]
      (symbol/price-label g (:price character)
                          (/ (+ left right) 2) (util/mm->px 7)
                          (/ (+ left right) 2) (util/mm->px 20))
      (.drawImage g token-image
                  left top right bottom ; (* scale (inc idx)) (* scale 1) (* scale (inc (inc idx)))
                  0 0 (.getWidth token-image) (.getHeight token-image)
                  nil)
      #_(let [line (last (string/split (:label character) #" "))]
        ;(draw/text g (draw/text-style (util/px 6) Color/BLACK) line mid-x mid-y)
        (draw/line g (draw/line-style  1 (apply draw/rgb (:colour character)))
                   left bottom
                   right bottom))
      )

    (doseq [[attr-idx attribute] (map-indexed vector attributes)
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

    (println (str (Math/round ^Double width-mm) " mm X " (Math/round ^Double height-mm) " mm"))))

(draw-piece-card)
