(ns domination.support.tokens
  (:require [domination.support.draw :as draw]
            [domination.data :as data]
            [domination.support.symbol :as symbol]
            [domination.support.util :as util]
            [clojure.java.io :as io])
  (:import
    [java.awt Graphics2D Color Font]
    (java.awt.geom Ellipse2D$Double AffineTransform)
    (java.awt.font FontRenderContext)
    (javax.imageio ImageIO)
    (java.io File)
    (java.awt.image RenderedImage)))


(defn indexed [c]
  (map-indexed vector c))

(defn- transforms [& tfs]
  (reduce (fn [a b]
            (.preConcatenate a b)
            a)
          (AffineTransform.)
          tfs))

(defn- arc-text [g style text cx cy size radius angle]
  (let [font ^Font (.deriveFont draw/font-bold (float size))
        frc ^FontRenderContext (.getFontRenderContext g)
        gv (.createGlyphVector font frc ^String text)
        text-bounds (.getPixelBounds gv frc 0 0)
        num-glyphs (.getNumGlyphs gv)]

    ;(draw/line g (draw/line-style 1) (- cx 2) cy (+ cx 2) cy)
    ;(draw/line g (draw/line-style 1) cx (- cy 2) cx (+ cy 2))

    (doseq [idx (reverse (range 0 num-glyphs))]
      (let [glyph-shape (.getGlyphOutline gv idx 0 0)
            glyph-bounds (.getBounds glyph-shape)
            prop (/ (+ (.getX glyph-bounds)
                       (/ (.getWidth glyph-bounds) 2))
                    (.getWidth text-bounds))
            theta (+ (* util/TAU -1 angle)
                     (* util/TAU 2 angle prop))
            tf (transforms
                 (AffineTransform/getTranslateInstance (+ (- (.getX glyph-bounds))
                                                          (* (.getWidth glyph-bounds) -1/2))
                                                       (* (.getHeight text-bounds) 1/2))
                 (AffineTransform/getRotateInstance theta)
                 (AffineTransform/getTranslateInstance
                   (* radius (Math/sin theta))
                   (* radius (Math/cos theta) -1))
                 (AffineTransform/getTranslateInstance cx cy))]
        (draw/with-transform g tf
          (draw/shape g style glyph-shape))))))

(defn draw-token [{:keys [label coin money? price move damage shield cake? colour]
                   attack-range :range
                   :as _character}
                  ^Graphics2D g x y
                  & [no-border?]]

  (let [token-size 200]

    ;(.setColor g Color/WHITE)
    ;(.fillRect g x y token-size token-size)

    (let [inset (* token-size 0.095)
          outer-width (* token-size 0.02)
          clip-size (- token-size inset inset)
          outer-ring (Ellipse2D$Double. (+ x inset) (+ y inset) clip-size clip-size)
          inner-ring (Ellipse2D$Double. (+ x inset outer-width)
                                        (+ y inset outer-width)
                                        (- clip-size outer-width outer-width)
                                        (- clip-size outer-width outer-width))
          color (case label
                   "Gold" Color/YELLOW
                   "Silver" Color/LIGHT_GRAY
                   "Bronze" (draw/rgb 205, 127, 0)
                   "Scout" (draw/rgb 255, 175, 0)
                   (draw/rgb colour))]

      ; TODO: curved text label over the top: https://stackoverflow.com/questions/5159845/curved-text-in-java
      ; OR does the label go on the back?

      (when-not no-border?
        ;(draw/with-clip g outer-ring)
        (draw/shade g x y token-size color draw/shade-lowlight)
        (draw/shape g (draw/line-style 1.5) outer-ring))

      (if no-border?
        (draw/shade g x y token-size color draw/shade-highlight)
        (do
          (draw/with-clip g inner-ring
            (draw/shade g x y token-size color draw/shade-highlight))
          (draw/shape g (draw/line-style 0.5) inner-ring)))

      (let [r (/ token-size 2)
            cx (+ x r)
            cy (+ y r)
            symbols (remove nil?
                            (flatten
                              [(when move
                                 [(fn [g x y]
                                    (symbol/move g x y move))])
                               (when coin
                                 #_(repeat coin
                                         (fn [g x y]
                                           (symbol/price-label g 1 x y)))
                                 [(fn [g x y]
                                    (symbol/price-label g coin
                                                        x
                                                        (if money?
                                                          (- y (* 10 (dec coin)))
                                                          y)))]
                                 )
                               (when attack-range
                                 [(fn [g x y]
                                    (symbol/attack-range g x y attack-range))])
                               (when damage
                                 [(fn [g x y]
                                    (symbol/damage g x y damage))])
                               (when shield
                                 #_(repeat shield (fn [g x y]
                                                  (symbol/shield g x (- y (util/mm->px 0.75)) 20)))
                                 (fn [g x y]
                                   (symbol/shield g x (- y (util/mm->px 0.75)) 20 shield)))
                               (when cake?
                                 [(fn [g x y]
                                    (symbol/cake g x y 20))])]))]

        (when-not money?
          (symbol/role-mask g cx (- cy (* r 1/16)) (/ r 5)))

        (let [steps (case (count symbols)
                      0 []
                      1 [0]
                      2 [(- (* util/TAU 1/8)) (* util/TAU 1/8)]
                      3 [(- (* util/TAU 1/6)) 0 (* util/TAU 1/6)])]
          (doseq [[theta sym] (map vector
                                   steps
                                   symbols)]
            (sym g
                 (+ cx (* (Math/sin (+ theta #_step-size)) r 1/2))
                 (+ (- cy (* r 1/32)) (* (Math/cos (+ theta #_step-size)) r 1/2)))))

        (case label
          "Gold" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx (+ cy (util/mm->px 21)) 50 120 1/12)
          "Silver" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx (+ cy (util/mm->px 25)) 48 130 1/11)
          "Bronze" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx (+ cy (util/mm->px 40)) 46 170 1/15)
          "Farmer" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 35 55 3/14)
          "Scout" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 40 55 3/14)
          "Archer" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 35 55 3/14)
          "Chocolate Cake" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 30 55 4/10)
          "Smithy" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 35 60 3/14)
          "Knight" (arc-text g (draw/shape-style Color/BLACK 0.5 color) label cx cy 35 60 3/14))
        #_(if (and money? (= "Gold" label))
          (arc-text g (draw/line-style 1) label cx (+ cy (util/mm->px 12)) 50 88 1/8)

          ))
      )))



(defn ffilter [pred coll]
  (first (filter pred coll)))

(defn farmer [g x y]
  (draw-token (ffilter #(-> % :label (= "Farmer")) data/characters) g x y))


(do ; test block

  (require '[domination.see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  (def size 960)
  (def width size)
  (def height size)

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))
  (defonce g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  (doseq [[idx c] (map-indexed vector data/characters)]
    (let [row (quot idx 3)
          col (rem idx 3)]
      (draw-token c g (* col 250) (* row 250))))

  (refresh-fn)

  (doseq [[idx c] (map-indexed vector data/characters)]
    (let [file (io/file "generated" (str "token_" idx ".png"))]
      (io/make-parents file)
      (ImageIO/write
        ^RenderedImage
        (draw/with-new-image [^Graphics2D g (BufferedImage. 220 220 BufferedImage/TYPE_INT_ARGB)]
          (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
          (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
          (.setColor g Color/YELLOW)
          (.fillRect g 0 0 250 250)
          (draw-token c g 10 10 :no-border!))
        ; TODO: can probably get away with a 200 x 200 image and the
        ; tokens drawn at (0, 0).
        ; They got shrunk because the printers were worried about bleed distances and
        ; cutting accuracy and didn't want to print it.
        "png"
        ^File file))
    )

  )