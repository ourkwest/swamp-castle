(ns domination.token-images
  (:require [clojure.java.io :as io]
            [domination.data :as data]
            [domination.see.core :as see]
            [domination.board-image :as board])
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color Polygon Graphics2D RenderingHints Rectangle BasicStroke Font]
           [java.awt.geom Area Rectangle2D AffineTransform]
           [java.awt.font FontRenderContext]))


(def TAU (* 2 Math/PI))

(def sf 2.0)
(def token-size (int (* 4.0 sf board/hex-radius)))

(def width (* 3 token-size))
(def height (* 3 token-size))

(def millis-per-inch 25.4)
(def a4-width 210)
(def a4-height 297)

(defn rand-0 [n]
  (- (rand n) (/ n 2)))

;(def width 2100)
;(def height 2970)
(def hex-radius 40)
(def x-step (* (/ (Math/sqrt 3) 2) 2 hex-radius))
(def y-step (* (/ 3 2) hex-radius))
(def n-x 10)
(def n-y 15)




(def board-color (Color. 60 90 20))
(def ground-color (Color. 120 180 40))
(def outline-color (Color. 200 200 200))

(defn brighter [[r g b]]
  [(int (min 255 (+ (* r 1.5) 50)))
   (int (min 255 (+ (* g 1.5) 50)))
   (int (min 255 (+ (* b 1.5) 50)))])

(defn darker [[r g b]]
  [(int (max 0 (- (/ r 1.5) 50)))
   (int (max 0 (- (/ g 1.5) 50)))
   (int (max 0 (- (/ b 1.5) 50)))])

(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))

(defn rgb [[r g b]]
  (Color. (clamp 0 r 255)
          (clamp 0 g 255)
          (clamp 0 b 255)))

(def thin-stroke (BasicStroke. (/ hex-radius 60) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))




(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))

(def xyn
  (for [x (range 3)
        y (range 3)]
    [x y (+ x (* y 3))]))

(defn smear-text [g x y smear string]
  (doseq [x-offset (range (- smear) smear)
          y-offset (range (- smear) smear)]
    (.drawString g string (int (+ x x-offset)) (int (+ y y-offset)))))

(defn write-text [g font string y-offset]
  (let [font (Font. nil Font/PLAIN font)
        bounds (.getStringBounds font string (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))]
    (.setFont g font)
    (.setColor g Color/BLACK)
    (smear-text g
                (int (- (/ token-size 2) (/ (.getWidth bounds) 2)))
                (int (+ y-offset (/ token-size 2) (* (.getHeight bounds) 0.3)))
                (* token-size 0.0065)
                string)
    (.setColor g Color/WHITE)
    (smear-text g
                (int (- (/ token-size 2) (/ (.getWidth bounds) 2)))
                (int (+ y-offset (/ token-size 2) (* (.getHeight bounds) 0.3)))
                (* token-size 0.0025)
                string)))

(defn write-shadow [g font string y-offset]
  (let [font (Font. nil Font/PLAIN font)
        bounds (.getStringBounds font string (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))
        old-clip (.getClip g)]
    (.setFont g font)
    (.setColor g (Color. 0 0 0 10))
    (.setClip g (board/polygon
                  (* token-size 0.5)
                  (* token-size 0.705)
                  (* sf board/hex-radius)
                  20 0 0.5))
    (.setTransform g (AffineTransform/getScaleInstance 1.0 0.8))
    (smear-text g
                (int (- (/ token-size 2) (/ (.getWidth bounds) 2)))
                (int (+ y-offset (/ token-size 1.2) (* (.getHeight bounds) 0.3)))
                (* token-size 0.01)
                string)
    (.setTransform g (AffineTransform.))
    (.setClip g old-clip)))

(defn write-text-with-shadow [g font string y-offset]
  (write-text g font string y-offset)
  (write-shadow g font string y-offset))

(defn draw-tokens []
  (let [^Graphics2D g (.getGraphics image)
        images (for [_ (range 9)]
                 (BufferedImage. token-size token-size BufferedImage/TYPE_INT_ARGB))]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (doseq [[x y n] xyn
            :let [token-image (nth images n)
                  {:keys [label colour money?]} (nth data/characters n)]]

      (let [^Graphics2D g (.getGraphics token-image)]

        (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

        (.setColor g (Color. (first colour) (second colour) (last colour)))
        (.fillRect g 0 0 token-size token-size)

        ;(.setStroke g (BasicStroke. (/ token-size 20)))
        (let [bit (* token-size 0.1)
              offset (* token-size 0.01)]
          (.setColor g (Color. 255 255 255 127))
          (.fillArc g (+ bit offset) (+ bit offset) (- token-size bit bit) (- token-size bit bit) 0 360)
          (.setColor g (Color. 0 0 0 127))
          (.fillArc g (- bit offset) (- bit offset) (- token-size bit bit) (- token-size bit bit) 0 360)
          (.setColor g (rgb colour))
          (.fillArc g bit bit (- token-size bit bit) (- token-size bit bit) 0 360))

        (let [bit (* token-size 0.12)
              offset (* token-size 0.01)]
          (.setColor g (Color. 0 0 0 127))
          (.fillArc g (+ bit offset) (+ bit offset) (- token-size bit bit) (- token-size bit bit) 0 360)
          (.setColor g (Color. 255 255 255 127))
          (.fillArc g (- bit offset) (- bit offset) (- token-size bit bit) (- token-size bit bit) 0 360)
          (.setColor g (rgb colour))
          (.fillArc g bit bit (- token-size bit bit) (- token-size bit bit) 0 360))


        #_(.setStroke g (BasicStroke. (/ token-size 100)))
        #_(doseq [angle (range 0 TAU 0.01)
                :let [xa (* token-size (+ 0.5 (/ (Math/sin angle) 2.5)))
                      ya (* token-size (+ 0.5 (/ (Math/cos angle) 2.5)))
                      xb (* token-size (+ 0.5 (/ (Math/sin angle) 2.4)))
                      yb (* token-size (+ 0.5 (/ (Math/cos angle) 2.4)))
                      la (* 0.5 (- 1 (Math/sin (+ angle (/ TAU 8)))))
                      lb (* 0.5 (+ 1 (Math/sin (+ angle (/ TAU 8)))))
                      [red grn blu] colour]]
          (.setColor g (rgb [(int (+ (* la 127) (* la red 0.5)))
                             (int (+ (* la 127) (* la grn 0.5)))
                             (int (+ (* la 127) (* la blu 0.5)))]))
          (.drawLine g xa ya xa ya)
          (.setColor g (rgb [(int (+ (* lb 127) (* lb red 0.5)))
                             (int (+ (* lb 127) (* lb grn 0.5)))
                             (int (+ (* lb 127) (* lb blu 0.5)))]))
          (.drawLine g xb yb xb yb)
          )

        (case label
          ("Bronze" "Silver" "Gold") nil
          "Chocolate Cake"
          (board/render-token
            g
            (* token-size 0.5)
            (- (* token-size 0.35) (* 2.5 (/ board/hex-radius 9)))
            (rgb (darker colour))
            (rgb (brighter colour))
            (* 2.0 sf)
            )
          (board/render-token
            g
            (* token-size 0.5)
            (- (* token-size 0.65) (* 2.5 (/ board/hex-radius 9)))
            (rgb (darker colour))
            (rgb (brighter colour))
            (* 2.0 sf)
            ))

        (case label
          "Bronze" (write-text g (* token-size 0.2) label 0)
          "Silver" (write-text g (* token-size 0.25) label 0)
          "Gold" (write-text g (* token-size 0.3) label 0)
          "Farmer" (write-text-with-shadow g (* token-size 0.2) label 0)
          "Horse Rider" (do
                          (write-text-with-shadow g (* token-size 0.2) "Horse" (- (* token-size 0.1)))
                          (write-text-with-shadow g (* token-size 0.2) "Rider" (* token-size 0.1)))
          "Archer" (write-text-with-shadow g (* token-size 0.2) label 0)
          "Chocolate Cake" (do
                             (write-text g (* token-size 0.15) "Chocolate" (* token-size 0.05))
                             (write-text g (* token-size 0.15) "Cake" (* token-size 0.25))
                             (let [h-scale (* sf sf 0.7)
                                   v-scale (* sf sf 0.5)
                                   x-offset (* token-size 0.03)
                                   y-offset (* token-size 0.025)
                                   cake (ImageIO/read (io/file "./resources/public/images/cake.png"))
                                   cake-w (.getWidth cake)
                                   cake-h (.getHeight cake)
                                   dx1 (- (+ (/ token-size 2) x-offset) (/ (* h-scale cake-w) 2)) #_(- x (* scale (/ hex-radius 2)))
                                   dy1 (- (+ (/ token-size 2) y-offset) (/ (* v-scale cake-h) 2) (/ token-size 4.5))
                                   dx2 (+ dx1 (* h-scale cake-w))
                                   dy2 (+ dy1 (* v-scale cake-h))]
                               (.drawImage g cake
                                           dx1 dy1 dx2 dy2
                                           0 0 cake-w cake-h
                                           nil))
                             )
          "Blacksmith" (write-text-with-shadow g (* token-size 0.13) label 0)
          "Knight" (write-text-with-shadow g (* token-size 0.2) label 0)
          nil)
        )

      (.drawImage g token-image (* x token-size) (* y token-size) nil)
      (ImageIO/write token-image "png" (io/file (str "token_" n ".png"))))

    (refresh-fn)))

(draw-tokens)