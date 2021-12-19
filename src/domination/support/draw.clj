(ns domination.support.draw
  (:require
    [domination.support.util :as util]
    [clojure.java.io :as io])
  (:import
    [java.awt Polygon BasicStroke Font Graphics2D Color]
    (java.awt.font TextLayout)
    (java.awt.geom Area AffineTransform)))


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

(defprotocol Style
  (prepare-for-draw [this ^Graphics2D g] "Returns a boolean")
  (prepare-for-fill [this ^Graphics2D g] "Returns a boolean"))

#_(defmacro style [draw-form fill-form]
  `(reify Style
    (prepare-for-draw [_ ^Graphics2D g#]
      ~@draw-form)
    (prepare-for-fill [_ ^Graphics2D g#]
      ~@fill-form)))

(def default-style
  (reify Style
    (prepare-for-draw [_ g]
      (.setColor g Color/BLACK)
      (.setStroke g (BasicStroke. 1))
      (.setFont g font-regular)
      true)
    (prepare-for-fill [_ _]
      false)))

(defn styles [& styles]
  (reify Style
    (prepare-for-draw [_ g]
      (some identity (mapv #(prepare-for-draw % g) (cons default-style styles))))
    (prepare-for-fill [_ g]
      (some identity (mapv #(prepare-for-fill % g) (cons default-style styles))))))

(defn line-style
  ([] (line-style 1))
  ([width] (line-style width Color/BLACK))
  ([width color] (line-style width color BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  ([width color cap join]
   (reify Style
     (prepare-for-draw [_ g]
       (.setColor g color)
       (.setStroke g (BasicStroke. width cap join))
       true)
     (prepare-for-fill [_ _]
       false))))

(defn shape-style [stroke-color stroke-width fill-paint]
  (reify Style
    (prepare-for-draw [_ g]
      (.setColor g stroke-color)
      (.setStroke g (BasicStroke. stroke-width))
      true)
    (prepare-for-fill [_ g]
      (.setPaint g fill-paint)
      true)))

(defn text-style
  ([font-size color] (text-style font-size color false))
  ([font-size color bold?]
   (reify Style
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
    (when (prepare-for-fill style g)
      (.fill g poly))
    (when (prepare-for-draw style g)
      (.draw g poly))))

(defn line [g style xa ya xb yb]
  (when (prepare-for-draw style g)
    (.drawLine g xa ya xb yb)))

(defn text [^Graphics2D g style ^String string x y]
  (when (prepare-for-draw style g)
    (let [font (.getFont g)
          frc (.getFontRenderContext g)
          text-layout (TextLayout. string font frc)
          bounds (.getPixelBounds text-layout frc 0 0)
          gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
          gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
      (.draw text-layout g (float gx) (float gy)))))

(defn circle [^Graphics2D g style x y r]
  (let [gx (- x r)
        gy (- y r)
        size (* 2 r)]
    (when (prepare-for-fill style g)
      (.fillArc g gx gy size size 0 360))
    (when (prepare-for-draw style g)
      (.drawArc g gx gy size size 0 360))))

(defn shape [^Graphics2D g style shape]
  (when (prepare-for-fill style g)
    (.fill g shape))
  (when (prepare-for-draw style g)
    (.draw g shape)))

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
