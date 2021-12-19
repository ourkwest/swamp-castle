(ns domination.piece-card
  (:require
    [clojure.java.io :as io]
    [domination.see.core :as see]
    [domination.data :as data]
    [domination.token-images :as tokens]
    [clojure.string :as string])
  (:import
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage]
    [java.awt Color Polygon Graphics2D RenderingHints Rectangle BasicStroke Font]
    [java.awt.geom Area Rectangle2D AffineTransform]
    [java.awt.font TextLayout FontRenderContext]
    [java.time Instant]))


(def TAU (* 2 Math/PI))

(def millis-per-inch 25.4)

(def minion-size-mm millis-per-inch)
(def shield-size-mm 20) ;TODO: unknown!!1

(def scale 4) ; aka pixels per millimeter

(def minion-size (* minion-size-mm scale))
(def shield-size (* shield-size-mm scale))

(def a4-width-mm 297)
(def a4-height-mm 210)

(def width-mm (double a4-width-mm))
(def height-mm (double a4-height-mm))

(def width (* scale width-mm))
(def height (* scale height-mm))


(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))

(defn rgb
  ([r g b]
   (Color. (clamp 0 r 255)
           (clamp 0 g 255)
           (clamp 0 b 255)))
  ([r g b a]
   (Color. (clamp 0 r 255)
           (clamp 0 g 255)
           (clamp 0 b 255)
           (clamp 0 a 255))))

(def thin-stroke (BasicStroke. (* scale 1) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
(def thick-stroke (BasicStroke. (* scale 6) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn polygon [x y r n angle & [ar]]
  (let [angles (->> (range 0 TAU (/ TAU n))
                    (map (partial + angle)))
        aspect-ratio (or ar 1)
        xs (map #(-> % Math/sin (* r) (+ x)) angles)
        ys (map #(-> % Math/cos (* r aspect-ratio) (+ y)) angles)]
    (Polygon. (int-array xs) (int-array ys) (count xs))))

(defn shape-subtract [shape-a shape-b]
  (doto (Area. shape-a)
    (.subtract (Area. shape-b))))

(defn shape-add [shape-a shape-b]
  (doto (Area. shape-a)
    (.add (Area. shape-b))))


(defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))

(let [font-size (* scale 6)]
  (def bold-font (Font. nil Font/BOLD font-size))
  (def plain-font (Font. nil Font/PLAIN font-size)))

(defn draw-line [g xa ya xb yb colour width]
  (.setColor g colour)
  (.setStroke g (BasicStroke. width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  (.drawLine g xa ya xb yb))

(defn text [^Graphics2D g ^Font font colour ^String string x y]
  (let [frc (FontRenderContext. (AffineTransform.) (boolean true) (boolean true))
        text-layout (TextLayout. string font frc)
        bounds (.getPixelBounds text-layout frc 0 0)
        gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
        gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
    (.setColor g colour)
    (.draw text-layout g (float gx) (float gy))))

(defn circle [^Graphics2D g x y r stroke-width stroke fill]
  (let [gx (- x r)
        gy (- y r)
        size (* 2 r)]
    (when fill
      (.setColor g fill)
      (.fillArc g gx gy size size 0 360))
    (when stroke
      (.setColor g stroke)
      (.setStroke g (BasicStroke. stroke-width))
      (.drawArc g gx gy size size 0 360))))

(defn price-line [^Graphics2D g xa ya xb yb]
  (draw-line g xa ya xb yb (rgb 255 230 100 100) 12)
  (draw-line g xa ya xb yb (rgb 255 230 100) 5)
  (draw-line g xa ya xb yb (rgb 0 0 0) 1)
  )

(defn price [^Graphics2D g x y cost]
  (.setColor g (rgb 220 200 70))
  (.fill g (polygon x y (* scale 6) 8 (/ TAU 16)))
  (.setColor g (rgb 255 230 100))
  (.fill g (polygon x y (* scale 5) 8 (/ TAU 16)))
  (.setColor g (rgb 0 0 0))
  (.setStroke g (BasicStroke. 1))
  (.draw g (polygon x y (* scale 6) 8 (/ TAU 16)))
  (.draw g (polygon x y (* scale 5) 8 (/ TAU 16)))
  (text g bold-font Color/BLACK (str cost) x y))

(defn shield-spot [^Graphics2D g x y n]
  (circle g x y (/ shield-size 2) 2 Color/BLACK (rgb 50 200 255))
  (price g x y n))

(defn minion-spot [^Graphics2D g x y colour]
  (circle g x y (/ minion-size 2) 2 Color/BLACK colour))

(def attributes [{:label         "Cost"
                  :character-key :price
                  :colour (Color. 200 200 200)}
                 {:label         "Spend"
                  :character-key :coin
                  :colour (Color. 	240, 239, 137)}
                 {:label         "Move"
                  :character-key :move
                  :colour (Color. 127 255 127)}
                 {:label         "Damage"
                  :character-key :damage
                  :colour (Color. 255  50  50)}
                 {:label         "Range"
                  :character-key :range
                  :colour        (Color. 250 175 0)}
                 {:label         "Shield"
                  :character-key :shield
                  :colour        (Color. 137, 207, 240)}])

(defn draw-piece-card []
  (let [^Graphics2D g (.getGraphics image)]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 width height)

    ; TODO: symbol for players instead of "4 pl"
    (draw-line g (* width 0.005) (* height 0.07) (* width 0.09) (* height 0.07) Color/GRAY 1)
    (draw-line g (* width 0.005) (* height 0.44) (* width 0.09) (* height 0.44) Color/GRAY 1)
    (draw-line g (* width 0.005) (* height 0.07) (* width 0.005) (* height 0.44) Color/GRAY 1)
    (text g bold-font (rgb 0 0 0) "4 pl" (* width 0.04) (* height 0.4))

    (draw-line g (* width 0.015) (* height 0.07) (* width 0.09) (* height 0.07) Color/GRAY 1)
    (draw-line g (* width 0.015) (* height 0.35) (* width 0.09) (* height 0.35) Color/GRAY 1)
    (draw-line g (* width 0.015) (* height 0.07) (* width 0.015) (* height 0.35) Color/GRAY 1)
    (text g bold-font (rgb 0 0 0) "3 pl" (* width 0.045) (* height 0.3))

    (draw-line g (* width 0.025) (* height 0.07) (* width 0.09) (* height 0.07) Color/GRAY 1)
    (draw-line g (* width 0.025) (* height 0.25) (* width 0.09) (* height 0.25) Color/GRAY 1)
    (draw-line g (* width 0.025) (* height 0.07) (* width 0.025) (* height 0.25) Color/GRAY 1)
    (text g bold-font (rgb 0 0 0) "2 pl" (* width 0.05) (* height 0.17))

    ;(text g font (rgb 0 0 0) "2 pl" (* width 0.03) (* height 0.2))
    ;(text g font (rgb 0 0 0) "3 pl" (* width 0.03) (* height 0.3))
    ;(text g font (rgb 0 0 0) "4 pl" (* width 0.03) (* height 0.4))

    ; max: 22
    ; 4 x gold + 5 x farmer =
    ; 4 * 3 + 5 * 2 =
    ; 12 + 10 =
    ; 22
    (doseq [n (range 1 23)
            :let [x (+ (* scale (/ 50 2.5)) (* n scale 12))
                  y (* height -0.05)]]

      (price-line g
                  x (+ y (* height 0.1))
                  x (+ y (* height 0.395)))

      (let [shield-price-y (+ y (* height 0.08))]
        (price g x shield-price-y n))

      (if (odd? n)
        (do (shield-spot g x (+ y (* height 0.17)) n)
            (shield-spot g x (+ y (* height 0.35)) n))
        (do (shield-spot g x (+ y (* height 0.26)) n)
            (shield-spot g x (+ y (* height 0.44)) n))))

    (doseq [n (range 0 5)
            :let [x (+ (* scale (/ 77 2.5)) (* (+ n 0.0) scale 12 4.9))
                  y (* height 0.6)
                  cost ({0 1
                         1 2
                         2 4
                         3 6
                         4 8} n)]]

      (price-line g
                  (+ x (* minion-size 0.5)) (+ y (* minion-size 0.5))
                  (- x (* minion-size 0.5)) (- y (* minion-size 0.5)))
      (price-line g
                  (+ x (* minion-size 0.5)) (- y (* minion-size 0.5))
                  (- x (* minion-size 0.5)) (+ y (* minion-size 0.5)))

      (doseq [m (range 0 4)
              :let [xm (+ x
                          (* 0.8 minion-size (Math/sin (+ (/ TAU 8) (* m (/ TAU 4)))))
                          ;(* width 0.04 m)
                          )
                    ym (+ y
                          (* 0.8 minion-size (Math/cos (+ (/ TAU 8) (* m (/ TAU 4)))))
                          ;(* height (+ 0.01 (* 0.12 m)))
                          )
                    colour (nth [Color/GREEN Color/YELLOW Color/BLUE Color/RED] m)]]
        (minion-spot g xm ym colour)
        (price g xm ym cost))

      (price g x y cost))


    (doseq [[character-idx character] (->> data/characters
                                           (sort-by :price)
                                           (map-indexed vector))
            :let [token-image (tokens/draw-token character)
                  scale (* width 0.1)
                  top (* height 0.75)
                  bottom (+ (* height 0.75) (/ scale 4))
                  left (* scale character-idx 10/9)
                  right (* scale (inc character-idx) 10/9)
                  this-width (- right left)
                  this-height (- bottom top)
                  mid-x (+ left (/ this-width 2))
                  mid-y (+ top (/ this-height 2))]]
      #_(.drawImage g token-image
                  left top right bottom ; (* scale (inc idx)) (* scale 1) (* scale (inc (inc idx)))
                  0 0 (.getWidth token-image) (.getHeight token-image)
                  nil)
      (let [line (last (string/split (:label character) #" "))]
        (text g plain-font Color/BLACK line mid-x mid-y)
        (draw-line g left bottom right bottom (apply rgb (:colour character)) 1))
      )

    (doseq [[attr-idx attribute] (map-indexed vector attributes)
            :let [label (:label attribute)
                  scale (* width 0.1)
                  top (+ (* width 0.5)
                         (* (* width 0.023) (inc attr-idx)))
                  bottom (+ top (* width 0.02))
                  left 0
                  right scale
                  this-width (- right left)
                  this-height (- bottom top)
                  mid-x (+ left (/ this-width 2))
                  mid-y (+ top (/ scale 2))
                  inset (* scale 0.05)]]

      ;(.setColor g (:colour attribute))
      (draw-line g 0 (+ mid-y (* width 0.01)) width (+ mid-y (* width 0.01)) (:colour attribute) 1)

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
          (text g plain-font Color/BLACK (str value " " (:label attribute)) mid-x mid-y))
        )

      )


    (refresh-fn)
    (ImageIO/write image "png" (io/file "piece-card.png"))

    (println (str (Math/round ^Double width-mm) " mm X " (Math/round ^Double height-mm) " mm"))))


(draw-piece-card)

(println (str (Instant/now)))
