(ns domination.support.draw
  (:require
    [domination.support.util :as util]
    [domination.support.protocols :as protocols]
    [clojure.java.io :as io])
  (:import
    [java.awt Polygon BasicStroke Font Graphics2D Color RenderingHints]
    (java.awt.font TextLayout)
    (java.awt.geom Area AffineTransform Rectangle2D Rectangle2D$Double Ellipse2D$Double)))


(defn rgb
  ([color-bits]
   (apply rgb color-bits))
  ([r g b]
   (Color. (int (util/clamp 0 r 255))
           (int (util/clamp 0 g 255))
           (int (util/clamp 0 b 255))))
  ([r g b a]
   (Color. (int (util/clamp 0 r 255))
           (int (util/clamp 0 g 255))
           (int (util/clamp 0 b 255))
           (int (util/clamp 0 a 255)))))

(defn rgb-lerp [c1 c2 p]
  (let [r1 (.getRed c1)
        g1 (.getGreen c1)
        b1 (.getBlue c1)
        a1 (.getAlpha c1)
        r2 (.getRed c2)
        g2 (.getGreen c2)
        b2 (.getBlue c2)
        a2 (.getAlpha c2)
        ip (- 1 p)]
    (rgb (+ (* ip r1) (* p r2))
         (+ (* ip g1) (* p g2))
         (+ (* ip b1) (* p b2))
         (+ (* ip a1) (* p a2)))))


(def color-shield (rgb 50 200 255))
(def color-player-1 Color/GREEN)
(def color-player-2 Color/BLUE)
(def color-player-3 Color/YELLOW)
(def color-player-4 Color/RED)

;(defn style
;  ([stroke-color] (style stroke-color nil))
;  ([stroke-color fill-color] (style stroke-color 1 fill-color))
;  ([stroke-color stroke-width fill-color]
;   {:stroke-color stroke-color
;    :stroke-width stroke-width
;    :fill-color   fill-color}))

(def font-regular (Font/createFont Font/TRUETYPE_FONT (io/input-stream (io/resource "fonts/ah/AtkinsonHyperlegible-Regular.ttf"))))
(def font-bold (Font/createFont Font/TRUETYPE_FONT (io/input-stream (io/resource "fonts/ah/AtkinsonHyperlegible-Bold.ttf"))))

#_(defmacro style [draw-form fill-form]
  `(reify Style
    (prepare-for-draw [_ ^Graphics2D g#]
      ~@draw-form)
    (prepare-for-fill [_ ^Graphics2D g#]
      ~@fill-form)))

(def default-style
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (.setColor g Color/BLACK)
      (.setStroke g (BasicStroke. 1))
      (.setFont g font-regular)
      true)
    (prepare-for-fill [_ _]
      false)))

(defn styles [& styles]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (some identity (mapv #(protocols/prepare-for-draw % g) (cons default-style styles))))
    (prepare-for-fill [_ g]
      (some identity (mapv #(protocols/prepare-for-fill % g) (cons default-style styles))))))

(defn line-style
  ([] (line-style 1))
  ([width] (line-style width Color/BLACK))
  ([width color] (line-style width color BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  ([width color cap join]
   (reify protocols/Style
     (prepare-for-draw [_ g]
       (.setColor g color)
       (.setStroke g (BasicStroke. width cap join))
       true)
     (prepare-for-fill [_ _]
       false))))

(defn fill-style [fill-paint]
  (reify protocols/Style
    (prepare-for-draw [_ _g]
      false)
    (prepare-for-fill [_ g]
      (.setPaint g fill-paint)
      true)))

(defn shape-style [stroke-color stroke-width fill-paint]
  (reify protocols/Style
    (prepare-for-draw [_ g]
      (.setColor g stroke-color)
      (.setStroke g (BasicStroke. stroke-width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      true)
    (prepare-for-fill [_ g]
      (.setPaint g fill-paint)
      true)))

(defn text-style
  ([font-size color] (text-style font-size color false))
  ([font-size color bold?]
   (reify protocols/Style
     (prepare-for-draw [_ g]
       (.setFont g (.deriveFont (if bold? font-bold font-regular) (float font-size)))
       (.setColor g color)
       true)
     (prepare-for-fill [_ _]
       false))))

(def style-shield (shape-style Color/BLACK 1 color-shield))
(def style-player-1 (shape-style Color/BLACK 1 Color/GREEN))
(def style-player-2 (shape-style Color/BLACK 1 Color/BLUE))
(def style-player-3 (shape-style Color/BLACK 1 Color/YELLOW))
(def style-player-4 (shape-style Color/BLACK 1 Color/RED))


(defn shape-intersect
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.intersect (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-intersect (shape-intersect shape-a shape-b) more-shapes)))

(defn shape-subtract
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.subtract (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-subtract (shape-subtract shape-a shape-b) more-shapes)))

(defn shape-add
  ([shape-a shape-b]
   (doto (Area. shape-a)
     (.add (Area. shape-b))))
  ([shape-a shape-b & more-shapes]
   (reduce shape-add (shape-add shape-a shape-b) more-shapes)))

(defn v+ [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn v- [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn v* [[x y] f]
  [(* x f) (* y f)])

(defn vlerp [a b p]
  (let [q (- 1 p)]
    (v+ (v* a q) (v* b p))))

(defn bezier [a b c d n]
  (for [p (range 0 1.00001 (/ 1 n))]
    (let [e (vlerp a b p)
          f (vlerp b c p)
          g (vlerp c d p)
          h (vlerp e f p)
          i (vlerp f g p)
          j (vlerp h i p)]
      j)))

(defn poly
  ([x y r n angle] (poly x y r n angle 1))
  ([x y r n angle aspect-ratio]
   (let [angles (->> (range 0 util/TAU (/ util/TAU n))
                     (map (partial + angle)))
         points (map (fn [angle]
                       [(-> angle Math/sin (* r) (+ x))
                        (-> angle Math/cos (* r aspect-ratio) (+ y))])
                     angles)]
     (poly points)))
  ([xys]
   (let [xs (map first xys)
         ys (map second xys)]
     (Polygon. (int-array xs) (int-array ys) (count xys)))))

(defn polygon [g style x y r n angle & [ar]]
  ; todo: deprecated use draw/poly for creation and draw/shape for actual drawing
  (let [angles (->> (range 0 util/TAU (/ util/TAU n))
                    (map (partial + angle)))
        aspect-ratio (or ar 1)
        xs (map #(-> % Math/sin (* r) (+ x)) angles)
        ys (map #(-> % Math/cos (* r aspect-ratio) (+ y)) angles)
        poly (Polygon. (int-array xs) (int-array ys) (count xs))]
    (when (protocols/prepare-for-fill style g)
      (.fill g poly))
    (when (protocols/prepare-for-draw style g)
      (.draw g poly))))

(defn line [g style xa ya xb yb]
  (when (protocols/prepare-for-draw style g)
    (.drawLine g xa ya xb yb)))

(defn text [^Graphics2D g style ^String string x y]
  (when (protocols/prepare-for-draw style g)
    (let [font (.getFont g)
          x-offset (case string
                     "1" (* (.getSize font) -1/16)
                     0)
          frc (.getFontRenderContext g)
          text-layout (TextLayout. string font frc)
          bounds (.getPixelBounds text-layout frc 0 0)
          gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
          gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
      (.draw text-layout g (float (+ gx x-offset)) (float gy)))))

(defn shape [^Graphics2D g style shape]
  (when (protocols/prepare-for-fill style g)
    (.fill g shape))
  (when (protocols/prepare-for-draw style g)
    (.draw g shape)))

(defn text-shape [^Graphics2D g style ^String string x y]
  (when (protocols/prepare-for-draw style g)
    (let [font (.getFont g)
          x-offset (case string
                     "1" (* (.getSize font) -1/16)
                     0)
          frc (.getFontRenderContext g)
          text-layout (TextLayout. string font frc)
          bounds (.getPixelBounds text-layout frc 0 0)
          gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
          gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
      (shape g style (.getOutline text-layout (AffineTransform/getTranslateInstance (float (+ gx x-offset)) (float gy))))
      #_(.draw g (.getOutline text-layout (AffineTransform/getTranslateInstance (float (+ gx x-offset)) (float gy)))))))

(defn circle [^Graphics2D g style x y r]
  (let [gx (- x r)
        gy (- y r)
        size (* 2 r)]
    (when (protocols/prepare-for-fill style g)
      (.fillArc g gx gy size size 0 360))
    (when (protocols/prepare-for-draw style g)
      (.drawArc g gx gy size size 0 360))))

(defn translate [shape x y]
  (doto (Area. shape)
    (.transform (AffineTransform/getTranslateInstance x y))))

(defn rotate [shape radians]
  (doto (Area. shape)
    (.transform (AffineTransform/getRotateInstance radians))))

(defmacro with-clip [g clip & body]
  `(let [pre-clip# (.getClip ~g)]
     (try
       (.setClip ~g ~clip)
       ~@body
       (finally
         (.setClip ~g pre-clip#)))))

(defmacro with-transform [g transform & body]
  `(let [pre-transform# (.getTransform ~g)]
     (try
       (.setTransform ~g ~transform)
       ~@body
       (finally
         (.setTransform ~g pre-transform#)))))

(defn shade-highlight [p]
  (+ (/ (+ (Math/sin (* (- p 1/4) util/TAU)) 1) 2.5)
     (/ (max (Math/tan (* (- p 1/6) util/TAU)) 0) 60)))

(defn shade-lowlight [p]
  (/ (Math/sin (* (+ p 1/4) util/TAU)) 2.5))

(defn shade [g x y size color shade-fn]
  (doseq [p (range 0 1 (/ 1 (* 2 size)))]
    (let [n (* p 2 size)
          cf (shade-fn p)
          c (if (pos? cf)
              (rgb-lerp color Color/WHITE cf)
              (rgb-lerp color Color/BLACK (- cf)))]
      (line g (line-style 1 c) (+ x n) y x (+ y n)))))

(defmacro with-new-image [[graphics-symbol image-form] & forms]
  `(let [image# ~image-form
         ~graphics-symbol (.getGraphics image#)]
     (.setRenderingHint ~graphics-symbol RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
     (.setRenderingHint ~graphics-symbol RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
     ~@forms
     image#))

(defn rectangle [x y w h]
  (Rectangle2D$Double. x y w h))

(defn ellipse [x y w h]
  (Ellipse2D$Double. x y w h))
