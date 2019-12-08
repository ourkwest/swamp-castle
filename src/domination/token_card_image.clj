(ns domination.token-card-image
  (:require
    [domination.see.core :as see]
    [domination.token-images :as tokens]
    [domination.data :as data]
    [clojure.java.io :as io])
  (:import
    [java.awt.image BufferedImage]
    [java.awt RenderingHints Graphics2D Color BasicStroke Font]
    [sun.font Font2D]
    [java.awt.font FontRenderContext TextLayout]
    [java.awt.geom AffineTransform]
    [javax.imageio ImageIO]))


(def scale 200)

(def width (* scale 7))
(def height (* scale 10))

; TODO: flip x/y !

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))

(def attributes [{:label         "Cost to buy"
                  :character-key :price
                  :colour (Color. 200 200 200)}
                 {:label         "Spend"
                  :character-key :coin
                  :colour (Color. 	240, 239, 137)}
                 {:label         "Movement"
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

(defn text [^Graphics2D g ^Font font ^String string x y]
  (let [frc (FontRenderContext. (AffineTransform.) (boolean true) (boolean true))
        text-layout (TextLayout. string font frc)
        bounds (.getPixelBounds text-layout frc 0 0)
        gx (- x (.getX bounds) (/ (.getWidth bounds) 2))
        gy (- y (.getY bounds) (/ (.getHeight bounds) 2))]
    ;(println x gx (.getWidth bounds) (.getX bounds))
    (.draw text-layout g (float gx) (float gy))))

(defn draw-token-card []
  (let [^Graphics2D g (.getGraphics image)
        clip (.getClip g)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 width height)

    (.setColor g Color/BLACK)
    (.setStroke g (BasicStroke. (* scale 0.01)))
    (let [font-size (* scale 0.125)
          font (Font. nil Font/BOLD font-size)]
      (doseq [[attr-idx attribute] (map-indexed vector attributes)
              :let [label (:label attribute)
                    left (* scale (inc attr-idx))
                    right (+ scale left)
                    this-width (- right left)
                    top 0
                    bottom scale
                    this-height (- bottom top)
                    mid-x (+ left (/ this-width 2))
                    mid-y (+ 0 (/ scale 2))
                    inset (* scale 0.05)]]

        (.setColor g (:colour attribute))
        (.fillArc g
                  (+ left inset)
                  (+ top inset)
                  (- this-width inset inset)
                  (- this-height inset inset) 0 360)
        (.setColor g Color/BLACK)
        (.setStroke g (BasicStroke. (* scale 0.05)))
        (.drawArc g
                  (+ left inset)
                  (+ top inset)
                  (- this-width inset inset)
                  (- this-height inset inset) 0 360)

        (text g font (str label) (int mid-x) (int mid-y))
        (.setStroke g (BasicStroke. (* scale 0.01)))
        (.drawLine g left 0 left height)
        ))

    (let [font-size (* scale 0.6)
          font (Font. nil Font/PLAIN font-size)]
      (doseq [[character-idx character] (map-indexed vector data/characters)
              :let [token-image (tokens/draw-token character)
                    left 0
                    right scale
                    this-width (- right left)
                    top (* scale (inc character-idx))
                    bottom (* scale (inc (inc character-idx)))
                    this-height (- bottom top)
                    mid-x (+ left (/ this-width 2))
                    mid-y (+ top (/ this-height 2))]]

        (.setClip g (domination.board-image/polygon mid-x mid-y (/ this-width 2) 100 0))
        (.drawImage g token-image
                    left top right bottom ; (* scale (inc idx)) (* scale 1) (* scale (inc (inc idx)))
                    0 0 (.getWidth token-image) (.getHeight token-image)
                    nil)

        (.setColor g Color/WHITE)
        (.setStroke g (BasicStroke. (* scale 0.15)))
        (.drawArc g left top this-width this-height 0 360)
        (.setClip g clip)

        (.setColor g Color/BLACK)
        (.setStroke g (BasicStroke. (* scale 0.05)))
        (let [i (* scale 0.05)]
          (.drawArc g (+ left i) (+ top i) (- this-width i i) (- this-height i i) 0 360))
        #_(.draw g (domination.board-image/polygon mid-x mid-y (* this-width 0.45) 200 0))

        (.setStroke g (BasicStroke. (* scale 0.01)))
        (.drawLine g 0 top width top)

        (doseq [[attr-idx attribute] (map-indexed vector attributes)
                :let [left (* scale (inc attr-idx))
                      right (+ scale left)
                      width (- right left)
                      mid-x (+ left (/ width 2))
                      value ((:character-key attribute) character)
                      inset (* scale 0.05)]
                :when value]

          (.setColor g (:colour attribute))
          (.fillArc g
                    (+ left inset)
                    (+ top inset)
                    (- width inset inset)
                    (- this-height inset inset) 0 360)
          (.setColor g Color/BLACK)
          (.setStroke g (BasicStroke. (* scale 0.05)))
          (.drawArc g
                    (+ left inset)
                    (+ top inset)
                    (- width inset inset)
                    (- this-height inset inset) 0 360)
          (.setFont g font)
          (text g font (str value) (int mid-x) (int mid-y))
          ;(.drawString g (str value) (int mid-x) (int mid-y))
          )

        ))

    (.setClip g clip)

    )

  (ImageIO/write image "png" (io/file "token-card.png"))
  (refresh-fn))

(draw-token-card)
