(ns board-game.images.tokens
  (:require [board-game.support.data :as data]
            [board-game.support.draw :as draw]
            [board-game.support.symbol :as symbol]
            [board-game.support.texture :as texture]
            [board-game.support.util :as util]
            [clojure.java.io :as io]
            [see.core :as see])
  (:import
    [java.awt Color Font Graphics2D]
    [java.awt RenderingHints]
    [java.awt.font FontRenderContext]
    [java.awt.geom AffineTransform Ellipse2D$Double]
    [java.awt.image BufferedImage RenderedImage]
    [java.awt.image BufferedImage]
    [java.io File]
    [javax.imageio ImageIO]))



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

(defn token-background [{:keys [label colour] :as _character} ^Graphics2D g x y & [no-border?]]
  (let [token-size 200
        inset (* token-size 0.095)
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
    (if no-border?
      (draw/shade g x y token-size color draw/shade-highlight)
      (do
        (draw/shade g x y token-size color draw/shade-lowlight)
        (draw/shape g (draw/line-style 1.5) outer-ring)
        (draw/with-clip g inner-ring
          (draw/shade g x y token-size color draw/shade-highlight))
        (draw/shape g (draw/line-style 0.5) inner-ring)))))

(defn draw-token-back [{:keys [label coin money? price move damage shield cake? colour]
                        attack-range :range
                        :as character}
                       ^Graphics2D g x y
                       & [no-border?]]
  (token-background character g x y no-border?)
  (case label
    "Chocolate Cake" (do
                       (doseq [x (range 0 200 31)
                               y (range 0 200 31)]
                         (symbol/vp g x (if (odd? x) (+ y 15) y) 13))
                       (symbol/cake g 100 100 50))
    nil))

(defn draw-token [{:keys [label coin money? price move damage shield cake? colour]
                   attack-range :range
                   :as character}
                  ^Graphics2D g x y
                  & [no-border?]]

  (token-background character g x y no-border?)
  (let [token-size 200
        color (draw/rgb colour)
        r (/ token-size 2)
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
      (symbol/role-mask g cx (- cy (* r 7/16)) (/ r 5)))

    (let [steps (case (count symbols)
                  0 []
                  1 [0]
                  2 [(- (* util/TAU 1/12)) (* util/TAU 1/12)]
                  3 [(- (* util/TAU 10/68)) 0 (* util/TAU 10/68)])]
      (doseq [[theta sym] (map vector
                               steps
                               symbols)]
        (sym g
             (+ cx (* (Math/sin (+ theta #_step-size)) r 1/2))
             (+ (- cy (* r 1/32)) (* (Math/cos (+ theta #_step-size)) r 1/2)))))

    (let [line-color (draw/rgb-lerp color Color/BLACK 0.4)
          color (draw/rgb-lerp color Color/BLACK 0.15)
          draw-text (fn [text-size y-offset]
                      (draw/shape g
                                  (draw/shape-style line-color 1 color)
                                  (draw/translate (draw/text->shape g (draw/text-style text-size Color/BLACK :bold!)
                                                                    (if (= "Chocolate Cake" label)
                                                                      "Cake"
                                                                      label))
                                                  cx (- cy (* r y-offset)))))]
      (doseq [line-thickness [1]]
        (case label
          "Gold" (arc-text g (draw/shape-style line-color line-thickness color) label cx (+ cy (util/mm->px 21)) 50 120 1/12)
          "Silver" (arc-text g (draw/shape-style line-color line-thickness color) label cx (+ cy (util/mm->px 25)) 48 130 1/11)
          "Bronze" (arc-text g (draw/shape-style line-color line-thickness color) label cx (+ cy (util/mm->px 40)) 46 170 1/15)
          "Plough" (draw-text 40 1/32)

          ;(arc-text g (draw/shape-style line-color line-thickness color) label cx cy 35 60 3/14)
          "Dagger" (draw-text 40 1/32)
          "Bow" (draw-text 36 3/32)
          "Chocolate Cake" (draw-text 40 0)
          "Anvil" (draw-text 40 1/32)
          "Sword" (draw-text 40 1/32)
          )))
    ))

(defn goddess-rays [g [cx cy] step-size r]
  (loop [a 0
         b (rand step-size)]
    (let [x1 (* r (Math/sin a))
          y1 (* r (Math/cos a))
          x2 (* r (Math/sin b))
          y2 (* r (Math/cos b))]
      (draw/shape g (draw/fill-style (draw/rgb 255 255 255 (rand 100)))
        (draw/poly
          (map (partial draw/v+ [cx cy])
               [[x1 y1] [x2 y2] [(- x2) (- y2)] [(- x1) (- y1)]]))))
    (when (< b util/TAU)
      (recur b (+ b (rand step-size))))))

(defn draw-bonus-token [^Graphics2D g n]
  (let [color (case n
                0 (draw/rgb 255 150 150)
                1 (draw/rgb 150 100 255)
                2 (draw/rgb 50 255 255)
                3 (draw/rgb 100 255 100)
                4 (draw/rgb 255 230 60))
        current-clip (.getClip g)]
    (draw/shape g (draw/fill-style Color/BLACK)
                (draw/rectangle 0 0 200 200))

    (draw/shape g (draw/shape-style nil 0 (texture/new-texture [g2 [200 200]]
                                            (draw/shade g2 0 0 200 color draw/shade-highlight)
                                            ))
                (draw/text->shape g (draw/text-style 40 color :bold!) "Bonus!" 100 80))

    (draw/with-clip g
      (draw/shape-intersect
        ;(draw/shape-add
          ;(draw/text->shape g (draw/text-style 40 color :bold!) "Bonus!" 100 80)
        (draw/rectangle 0 100 200 55)
        ;)
        (or current-clip (draw/rectangle 0 0 200 200)))
      (draw/shade g 0 0 200 color draw/shade-highlight))
    (draw/shape g (draw/line-style 10) (draw/ellipse 100 100 80))
    (draw/shape g (draw/line-style 15) (draw/ellipse 100 100 90))
    (draw/shape g (draw/line-style 55) (draw/ellipse 100 100 100))
    (draw/text g (draw/text-style 40 Color/BLACK :bold!) "+" 75 127))
  (case n
    0 (symbol/move g 115 127 1)
    1 (symbol/price-label g 1 115 127)
    2 (symbol/attack-range g 115 127 1)
    3 (symbol/damage g 115 127 1)
    4 (symbol/shield g 115 123 20 1)))


(defn draw-bonus-back [^Graphics2D g]
  (draw/shape g (draw/fill-style (Color/BLACK))
    (draw/rectangle 0 0 200 200))
  (doseq [x (range -100 300 40)
          y (range -100 300 40)]

    (draw/with-transform g (transforms
                             (AffineTransform/getTranslateInstance x (+ y (* x 0.5)))
                             (AffineTransform/getRotateInstance -0.6))
      (draw/text-shape g
                       (draw/text-shape-style 20 (draw/rgb 128 128 0) 0.5 (draw/rgb 255 255 0 200) :bold!)
                       "Bonus!" 0 0))
    )
  (draw/text-shape g
                   (draw/text-shape-style 140 Color/YELLOW 3 (draw/rgb 128 128 0) :bold!)
                   "?" 103 105)
  #_(draw/text g (draw/text-style 140 Color/YELLOW :bold!) "?" 103 105))

(defn ffilter [pred coll]
  (first (filter pred coll)))

(def width 1000)
(def height 800)

(defn render-token-images [] ; test block

  ;(def size 1000)

  ;(defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  ;(defonce refresh-fn (see/see image :only-draw-when-updated? true))
  ;(defonce g (.getGraphics image))
  ;(.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  ;(.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
  ;
  ;(.setColor g Color/WHITE)
  ;(.fillRect g 0 0 width height)

  ;(doseq [[idx c] (map-indexed vector data/characters)]
  ;  (let [row (quot idx 3)
  ;        col (rem idx 3)]
  ;    (draw-token c g (* col 250) (* row 250))))
  ;
  ;(refresh-fn)

  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        refresh-fn (see/see image :only-draw-when-updated? true)
        g ^Graphics2D (.getGraphics image)

        _ (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        _ (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
        _ (.setColor g Color/WHITE)
        _ (.fillRect g 0 0 width height)

        to-print (io/file "generated" "to-print")
        for-instructions (io/file "generated" "for-instructions")
        token-clip (draw/ellipse 100 100 81)
        token-images (for [[idx c] (map-indexed vector data/characters)]
                       (draw/new-image-file to-print (str "token_" idx)
                         (fn [g] (draw-token c g 0 0 :no-border!))))
        token-backs (for [[idx c] (map-indexed vector data/characters)
                          :when (= idx 6)]
                      (draw/new-image-file to-print (str "token_back" idx)
                        (fn [g] (draw-token-back c g 0 0 :no-border!))))
        shield-image  (draw/new-image-file to-print "shield"
                        (fn [g]
                          (draw/shape g draw/style-shield
                            (draw/rectangle 0 0 200 200))
                          (goddess-rays g [100 100] 0.3 100)
                          (symbol/shield g 100 95 41)
                          (symbol/shield g 100 95 40)))
        vp-image  (draw/new-image-file to-print "vp"
                    (fn [g]
                      (draw/shape g (draw/fill-style (draw/rgb 255 50 200))
                        (draw/rectangle 0 0 200 200))
                      (symbol/vp g 100 90 52.5 :with-crown!)))
        bonus-tokens (map (fn [idx]
                            (draw/new-image-file to-print (str "bonus_" idx)
                              (fn [g]
                                (draw-bonus-token g idx)))) (range 5))
        bonus-back (draw/new-image-file to-print "bonus_back" draw-bonus-back)

        images (concat [shield-image vp-image] token-images token-backs bonus-tokens [bonus-back])]

    ; TODO: cake crumbs!

    ; TODO: bonus render quality?

    (doseq [[idx c] (map-indexed vector data/characters)]
      (draw/new-image-file for-instructions (str "token_" idx)
        (fn [g]
          (draw/with-clip g token-clip
            (draw-token c g 0 0)))))

    (doseq [[idx c] (map-indexed vector data/characters)
            :when (= idx 6)]
      (draw/new-image-file for-instructions (str "token_back_" idx)
        (fn [g] (draw-token-back c g 0 0 :no-border!))))

    (draw/new-image-file for-instructions "shield"
      (fn [g]
        (draw/with-clip g token-clip
          (draw/shape g draw/style-shield
                      (draw/rectangle 0 0 200 200))
          (symbol/shield g 100 95 41)
          (symbol/shield g 100 95 40))
        (draw/shape g (draw/line-style 3) token-clip)))

    (draw/new-image-file for-instructions "vp"
      (fn [g]
        (draw/with-clip g token-clip
          (draw/shape g (draw/fill-style (draw/rgb 255 50 200))
                      (draw/rectangle 0 0 200 200))
          (symbol/vp g 100 90 52.5 :with-crown!))
        (draw/shape g (draw/line-style 3) token-clip)))

    (doseq [idx (range 5)]
      (draw/new-image-file for-instructions (str "bonus_" idx)
        (fn [g]
          (draw/with-clip g token-clip
            (draw-bonus-token g idx)))) (range 5))

    (let [xs [0 1 2 3 4]
          ys [0 1 2 3]]
      (doseq [x xs
              y ys]
        (let [image (get (vec images) (+ x (* y (count xs))))
              xp (* x 200)
              yp (* y 200)]
          (.drawImage g ^BufferedImage image nil (int xp) (int yp))
          (draw/shape g
            (draw/line-style 1 (draw/rgb 255 0 0 128))
            (draw/ellipse (+ xp 100) (+ yp 100) 80))
          )))
    (refresh-fn)

    (println (count images)))

  #_(doseq [[idx c] (map-indexed vector data/characters)]
    (let [file (io/file "generated" (str "token_" idx ".png"))]
      (io/make-parents file)
      (ImageIO/write
        ^RenderedImage
        (draw/with-new-image [^Graphics2D g (BufferedImage. 200 200 BufferedImage/TYPE_INT_ARGB)]
          ;(.setColor g Color/YELLOW)
          ;(.fillRect g 0 0 250 250)
          (draw-token c g 0 0 :no-border!))
        ; TODO: can probably get away with a 200 x 200 image and the
        ; tokens drawn at (0, 0).
        ; They got shrunk because the printers were worried about bleed distances and
        ; cutting accuracy and didn't want to print it.
        "png"
        ^File file))
    )

  )
