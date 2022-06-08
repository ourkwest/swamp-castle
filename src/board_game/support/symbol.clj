(ns board-game.support.symbol
  (:require
    [board-game.support.util :as util]
    [board-game.support.draw :as draw]
    [clojure.java.io :as io]
    [board-game.support.texture :as texture])
  (:import
    [java.awt Graphics2D Color BasicStroke Rectangle TexturePaint]
    (java.awt.geom Ellipse2D Rectangle2D Rectangle2D$Float Ellipse2D$Float Area AffineTransform RectangularShape)
    (javax.imageio ImageIO)))


;TODO
;  money symbol - bordered octagon with number inside
;  role symbol - theatrical face mask
;  player count symbol (2, 3, 4) - top half of an oval with a circle on top, 'stacked' sideways for more players
;  cake symbol / image - hand drawn picture of cake
;  shield symbol - ?
;  VP symbol - ?
;  midden symbol - pile of tokens?

; do we need a minion symbol?

; TODO:
; 'draw' symbol for bonus tokens? or see-through mask symbol?
; 'players' symbols for piece-card? 1,2,3,4 players or shields?


(def color-price-outer (draw/rgb 220 200 70))
(def color-price-inner (draw/rgb 255 230 100))
(def style-price-outer (draw/shape-style Color/BLACK 1.5 color-price-outer))
(def style-price-inner (draw/shape-style Color/BLACK 1.5 color-price-inner))
(def text-style-price (draw/text-style (util/mm->px 8) (draw/rgb 0 0 0 200) true))

(defn- price-line [^Graphics2D g x1 y1 x2 y2]
  (draw/line g (draw/line-style (util/mm->px 3) (draw/rgb 255 255 0 128)) x1 y1 x2 y2)
  (draw/line g (draw/line-style (util/mm->px 2) color-price-inner) x1 y1 x2 y2)
  (draw/line g (draw/line-style (util/mm->px 1) (draw/rgb 255 240 150)) x1 y1 x2 y2))

(defn price-label
  ([^Graphics2D g cost x y] (price-label g cost x y nil nil))
  ([^Graphics2D g cost x1 y1 x2 y2]
   (when x2
     (price-line g x1 y1 x2 y2))

   (let [outer-poly (draw/poly x1 y1 (util/mm->px 6) 8 (/ util/TAU 16))
         inner-poly (draw/poly x1 y1 (util/mm->px 5) 8 (/ util/TAU 16))]

     (draw/shape g (draw/shape-style nil 0 color-price-outer) outer-poly)
     (let [extent 5]
       (draw/with-clip g outer-poly
         (doseq [p (range 0 1 (/ 1 (util/mm->px extent)))]
           (draw/line g (draw/line-style 1 (draw/rgb-lerp Color/WHITE (draw/rgb 255 255 255 0) p))
                      (- x1 (* p (util/mm->px extent))) (+ y1 (util/mm->px 5))
                      (+ x1 (util/mm->px 5)) (- y1 (* p (util/mm->px extent)))))))
     (draw/shape g (draw/line-style 1.5 Color/BLACK) outer-poly)

     (draw/shape g (draw/shape-style nil 0 color-price-inner) inner-poly)
     (draw/text g text-style-price (str cost) x1 y1)
     (let [extent 4]
       (doseq [p (range 0 1 (/ 1 (util/mm->px extent)))]
         (draw/line g (draw/line-style 1 (draw/rgb-lerp Color/WHITE (draw/rgb 255 255 255 0) p))
                    (+ x1 (* p (util/mm->px extent))) (- y1 (util/mm->px 4))
                    (- x1 (util/mm->px 4)) (+ y1 (* p (util/mm->px extent))))))
     (draw/shape g (draw/line-style 1.5 Color/BLACK) inner-poly)
     (draw/text g (draw/text-style (util/mm->px 8) (draw/rgb 0 0 0 100) false) (str cost) x1 y1)

     ;(draw/shape g style-price-inner inner-poly)
     )

   ;(draw/polygon g style-price-outer x1 y1 (util/mm->px 6) 8 (/ util/TAU 16))
   ;(draw/polygon g style-price-inner x1 y1 (util/mm->px 5) 8 (/ util/TAU 16))
   #_(draw/text g text-style-price (str cost) x1 y1)))


(defn mask-shape [x y size]
  (draw/shape-subtract
    (draw/shape-add
      (Rectangle2D$Float. (- x size) (- y size) (* size 2) (* size 1))
      ; top curve
      (Ellipse2D$Float. (- x size) (- y (* size 1.2)) (* size 2) (* size 0.4))
      ; jaw line
      (Ellipse2D$Float. (- x size) (- y (* size 1.2)) (* size 2) (* size 2.4)))
    ; mouth
    (draw/shape-subtract
      (Ellipse2D$Float. (- x (* size 0.6)) (+ y (* size 0.2)) (* size 1.2) (* size 0.4))
      (Rectangle2D$Float. (- x (* size 0.6)) (+ y (* size 0.1)) (* size 1.2) (* size 0.2)))
    ; left eye
    (Ellipse2D$Float. (- x (* size 0.7)) (- y (* size 0.6)) (* size 0.6) (* size 0.3))
    ; right eye
    (Ellipse2D$Float. (+ x (* size 0.1)) (- y (* size 0.6)) (* size 0.6) (* size 0.3))) )

(defn role-mask [^Graphics2D g x y size]
  (let [mask-style (draw/shape-style
                     Color/BLACK
                     1.0
                     (TexturePaint. (ImageIO/read (io/resource "images/woodgrain2.jpeg"))
                                    (Rectangle2D$Float. 0 0 (util/mm->px (* size 2)) (util/mm->px (* size 2)))))
        mask-style-2 (draw/shape-style
                       ;(draw/rgb 100 50 50)
                       Color/BLACK
                       1.5
                       (draw/rgb 255 255 255 75))
        mask-shape (mask-shape x y size)]
    (draw/shape g mask-style mask-shape)
    (draw/shape g mask-style-2 mask-shape)))

(defn empty-mask [^Graphics2D g x y size]
  (let [mask-shape (mask-shape x y size)]
    (draw/shape g (draw/shape-style (draw/rgb 0 0 0 100) 1.5 (draw/rgb 0 0 0 25)) mask-shape)))

(defn move [^Graphics2D g x y distance]
  (let [move-style (draw/shape-style Color/BLACK 1.0 Color/GREEN)
        radius (util/mm->px 6)
        move-shape (draw/poly [#_[(- x (* radius 1)) (- y (* radius 1))]
                               [(+ x (* radius 0)) (- y (* radius 1))]
                               [(+ x (* radius 1)) (+ y (* radius 0))]
                               [(+ x (* radius 0)) (+ y (* radius 1))]
                               #_[(- x (* radius 1)) (+ y (* radius 1))]])
        static-shape (draw/shape-subtract
                       (draw/poly x y radius 20 0)
                       (Rectangle2D$Float. x (- y radius) radius (* radius 2)))
        s 0.75
        move-shape (draw/shape-add
                     (draw/shape-subtract
                       (draw/poly x y (* radius s) 20 0)
                       (Rectangle2D$Float. x (- y radius) radius (* radius 2)))
                     ;(Rectangle2D$Float. (- x (util/mm->px 1)) (- y (* radius s)) (util/mm->px 2) (* radius 2 s))
                     (draw/poly [[(+ x (util/mm->px 0)) (- y radius)]
                                 [(+ x (util/mm->px 0) radius) y]
                                 [(+ x (util/mm->px 0)) (+ y radius)]]))
        ]
    #_(doseq [[x-offset color] {(util/mm->px 1)  (draw/rgb 0 255 0)
                              0                (draw/rgb 75 255 75)
                              (util/mm->px -1) (draw/rgb 150 255 150)}]
      (draw/shape g
                  (draw/shape-style (draw/rgb 0 0 0 0) 1.0 color)
                  (doto (Area. move-shape)
                    (.transform (AffineTransform/getTranslateInstance x-offset 0)))))

    (draw/with-clip g move-shape
      (doseq [p (range 0 1 (/ 1 (+ radius radius)))]
        (let [n (+ x (- radius) (* p (+ radius radius)))
              p-2 (* 2 (- (util/clamp 0.5 p 1) 0.5))
              c (draw/rgb-lerp (draw/rgb 230 210 125) (draw/rgb 0 225 0) p-2)]
          (draw/line g (draw/line-style 1 c) n y (- n radius) (- y radius))
          (draw/line g (draw/line-style 1 c) n y (- n radius) (+ y radius))))
      (draw/text g text-style-price (str distance) x y)
      (draw/shape g (draw/line-style 1) (draw/translate move-shape (util/mm->px -1) 0))
      (draw/shape g (draw/line-style 0.5) (draw/translate move-shape (util/mm->px -2) 0)))

    (draw/shape g (draw/line-style 1.5) move-shape)))

(defn star
  ([size point-count] (star size point-count 1))
  ([size point-count pointiness]
   (let [point-shape (draw/poly [[0 (- (* size pointiness))]
                                 [(* size 1/3) 0]
                                 [(* size -1/3) 0]])]
     (apply draw/shape-add (for [theta (range 0 util/TAU (/ util/TAU point-count))]
                             (draw/rotate point-shape theta))))))

(defn shield [g x y size & [shield-count bw?]]
  (let [cutout (util/mm->px (/ size 10))
        shape (draw/shape-subtract
                (draw/shape-add
                  (Rectangle2D$Float. (- x size) (- y size) (* 2 size) (* 1 size))
                  (draw/shape-subtract
                    (Ellipse2D$Float. (- x size) (- y (* size 1.5)) (* 3 size) (* 3 size))
                    (Rectangle2D$Float. (- x size) (- y size size) (* 3 size) (* 2 size))
                    (Rectangle2D$Float. x (- y size) (* 2 size) (* 3 size)))
                  (draw/shape-subtract
                    (Ellipse2D$Float. (- x size size) (- y (* size 1.5)) (* 3 size) (* 3 size))
                    (Rectangle2D$Float. (- x size size) (- y size size) (* 3 size) (* 2 size))
                    (Rectangle2D$Float. (- x size size) (- y size) (* 2 size) (* 3 size))))
                (Ellipse2D$Float. (- x size cutout) (- y size cutout)
                                  (* 2 cutout) (* 2 cutout))
                (Ellipse2D$Float. (- x (- size) cutout) (- y size cutout)
                                  (* 2 cutout) (* 2 cutout)))
        inset (util/mm->px 1)
        shape-inner (draw/shape-subtract
                      (draw/shape-add
                        (Rectangle2D$Float. (- x (- size inset)) (- y (- size inset)) (* 2 (- size inset)) (* 1 (- size inset)))
                        (draw/shape-subtract
                          (Ellipse2D$Float. (- x size (- inset)) (- y (* size 1.5)) (* 3 size) (* 3 size))
                          (Rectangle2D$Float. (- x size) (- y size size) (* 3 size) (* 2 size))
                          (Rectangle2D$Float. x (- y size) (* 3 size) (* 3 size)))
                        (draw/shape-subtract
                          (Ellipse2D$Float. (- x size size inset) (- y (* size 1.5)) (* 3 size) (* 3 size))
                          (Rectangle2D$Float. (- x size size inset) (- y size size) (* 3 size) (* 2 size))
                          (Rectangle2D$Float. (- x size size inset) (- y size) (+ (* 2 size) inset) (* 3 size))))
                      (Ellipse2D$Float. (- x size cutout inset) (- y size cutout inset)
                                        (+ (* 2 cutout) inset inset) (+ (* 2 cutout) inset inset))
                      (Ellipse2D$Float. (- x (- size) cutout inset) (- y size cutout inset)
                                        (+ (* 2 cutout) inset inset) (+ (* 2 cutout) inset inset)))
        castle-shape (draw/shape-subtract
                       (Rectangle2D$Float. (- x size) (- y size) (* 2 size) (* 3 size)) ; block
                       (Rectangle2D$Float. (- x size) (- y size) (* 2 size) (* 0.7 size)) ; top
                       (Rectangle2D$Float. (- x size) (+ y) (* 2 size) (* 0.1 size)) ; line
                       (Rectangle2D$Float. (- x size) (+ y (* 0.3 size)) (* 2 size) (* 0.1 size)) ; line
                       (Rectangle2D$Float. (- x size) (+ y (* 0.6 size)) (* 2 size) (* 0.1 size)) ; line
                       (Rectangle2D$Float. (- x size) (+ y (* 0.9 size)) (* 2 size) (* 0.1 size)) ; line
                       (Rectangle2D$Float. (- x size) (+ y (* 1.2 size)) (* 2 size) (* 0.1 size)) ; line
                       (Rectangle2D$Float. (- x (* 0.3 size)) (- y (* 0.3 size)) (* 0.2 size) (* 0.1 size)) ; castelations
                       (Rectangle2D$Float. (+ x (* 0.1 size)) (- y (* 0.3 size)) (* 0.2 size) (* 0.1 size)) ; castelations
                       ; sides
                       (Ellipse2D$Float. (- x (* size 3)) (- y (* size 1.5)) (* 2.6 size) (* 3 size))
                       (Ellipse2D$Float. (+ x (* size 3) (- (* 2.6 size))) (- y (* size 1.5)) (* 2.6 size) (* 3 size)))
        restyle (if bw? draw/grayscale identity)
        draw-star (fn [g x y]
                    (draw/shape g (restyle (draw/shape-style Color/WHITE
                                                             (* 0.6 (/ size 20))
                                                             Color/YELLOW))
                      (draw/translate (star (* size 0.3) 4) x y)))]

    (draw/with-clip g shape
      (doseq [n (range (* 2 size))]
        (let [color (draw/rgb (- 150 (* 50 (/ n (* 2 size)))) (- 200 (* 50 (/ n (* 2 size)))) 255)]
          (draw/line g (restyle (draw/line-style 1 color))
                     (+ x (- size) n) (- y size) (+ x (- size) n) (+ y size size)))))

    (draw/with-clip g shape-inner
      (doseq [n (range (* 2 size))]
        (let [color (draw/rgb (+ 100 (* 50 (/ n (* 2 size)))) (+ 150 (* 50 (/ n (* 2 size)))) 255)]
          (draw/line g (restyle (draw/line-style 1 color))
                     (+ x (- size) n) (- y size) (+ x (- size) n) (+ y size size))))

      (if shield-count
        #_(draw/text-shape
            g
            (draw/styles text-style-price
                         (draw/shape-style (draw/rgb 255 255 255 175) 3
                                           Color/BLACK))
            (str shield-count) x (+ y (util/mm->px 0.8)))
        (do

          #_(draw/line g (draw/line-style (* 0.7 size) (draw/rgb ))
                     (- x (* 0.8 size)) (+ (+ y (* 0.1 size)) (* 1.0 size))
                     (+ x (* 0.8 size)) (- (+ y (* 0.1 size)) (* 1.0 size)))
          (draw/line g (restyle (draw/line-style (* 0.7 size) (draw/rgb 75 75 250)))
                     (- x (* 0.8 size)) (+ (+ y (* 0.1 size)) (* 1.0 size))
                     (+ x (* 0.8 size)) (- (+ y (* 0.1 size)) (* 1.0 size)))

          (doseq [[x-off y-off] [[-1 -1] [1 1] [1 -1] [-1 1]]]
            (draw/text g (restyle (draw/text-style (util/mm->px 10) (draw/rgb 255 255 150 200) true))
                       (str shield-count) (+ x x-off) (+ y y-off (util/mm->px 0.8))))
          (draw/text g (restyle (draw/text-style (util/mm->px 10) (draw/rgb 0 0 0 255) true))
                     (str shield-count) x (+ y (util/mm->px 0.8))))
        (do
          (draw/line g (restyle (draw/line-style (* 0.7 size) Color/BLUE))
                     (- x (* 0.8 size)) (+ (+ y (* 0.1 size)) (* 1.0 size))
                     (+ x (* 0.8 size)) (- (+ y (* 0.1 size)) (* 1.0 size)))

          (draw/line g (restyle (draw/line-style (* 0.5 size) Color/BLACK))
                     (- x (* 0.8 size)) (+ (+ y (* 0.1 size)) (* 1.0 size))
                     (+ x (* 0.8 size)) (- (+ y (* 0.1 size)) (* 1.0 size)))

          (draw-star g (- x (* 0.4 size)) (+ (+ y (* 0.1 size)) (* 0.5 size)))
          (draw-star g (- x (* 0.0 size)) (+ (+ y (* 0.1 size)) (* 0.0 size)))
          (draw-star g (+ x (* 0.4 size)) (- (+ y (* 0.1 size)) (* 0.5 size))))))

    (draw/shape g (restyle (draw/line-style 1.5)) shape)
    (draw/shape g (restyle (draw/line-style 1 Color/YELLOW)) shape-inner)

    ))

(defn damage [g x y damage]
  (let [size 35
        points 9
        pointiness 0.7
        star-1 (star size points pointiness)
        star-2 (draw/rotate star-1 (/ util/TAU (* points 2)))]

    (draw/shape g (draw/line-style 3) (draw/translate star-1 x y))
    (draw/shape g (draw/line-style 3) (draw/translate star-2 x y))

    (draw/shape g
                (draw/shape-style Color/RED 1.5 Color/ORANGE)
                (draw/translate star-1 x y))
    (draw/shape g
                (draw/shape-style Color/RED 1.5 Color/ORANGE)
                (draw/translate star-2 x y))

    (draw/with-clip g (draw/translate star-2 x y)
      (doseq [p (range 0 1 (/ 1 size))]
        (draw/circle g (draw/line-style 1 (draw/rgb-lerp Color/YELLOW Color/RED p))
                     x y (* p size))))

    (draw/text g text-style-price (str damage) x y)))

(defn attack-range [g x y attack-range]

  (let [size 20
        arrow-head-size 9
        impact-point-x (+ x size -5)
        impact-point-y (+ y size)
        arc (draw/shape-subtract
              (Ellipse2D$Float. (- x size) (- y size) (* 2 size) (* 4 size))
              (Rectangle2D$Float. (- x size) (+ y size) (* 2 size) (* 2 size)))
        arrow (draw/shape-add
                (draw/shape-subtract
                  (Ellipse2D$Float. (- x size) (- y size) (* 2 size) (* 4 size))
                  (Ellipse2D$Float. (+ (- x size) 2) (+ (- y size) 5) (- (* 2 size) 10) (- (* 4 size) 10))
                  (Rectangle2D$Float. (- x size) (+ y size) (* 2 size) (* 2 size))
                  (Rectangle2D$Float. x (+ y size -6) (* 2 size) (* 2 size)))
                (draw/poly [[(+ impact-point-x) (+ y size)]
                            [(+ impact-point-x arrow-head-size) (+ impact-point-y (- arrow-head-size))]
                            [(+ impact-point-x (- arrow-head-size)) (+ impact-point-y (- arrow-head-size))]]))]

    (draw/shape g (draw/shape-style Color/BLACK 1 (draw/rgb 255 200 200)) arc)
    (draw/shape g (draw/shape-style Color/RED 1 Color/ORANGE)
                (Ellipse2D$Float. (- impact-point-x 10) (- impact-point-y 4)
                                  20 8))
    (draw/shape g (draw/shape-style Color/BLACK 1 Color/MAGENTA) arrow)

    )


  (draw/text g text-style-price (str attack-range) (- x 3) (+ y 3))
  )

(defn make-crumbs [^RectangularShape rectangle size n]
  (for [_ (range n)]
    (let [

          size (rand (* size 0.04))
          cx (+ (.getX rectangle) (rand (.getWidth rectangle)))
          cy (+ (.getY rectangle) (rand (.getHeight rectangle)) (- size))

          ;angle (rand util/TAU)
          ;dist (rand (* size 4 0.45))
          ;cx (+ x (* size 10) (* dist (Math/sin angle)))
          ;cy (+ y (* size 50) (* dist 0.5 (Math/cos angle)) )
          sides (rand-nth [5 6 7 9])
          crumb-shape-highlight (draw/poly (- cx (* size 0.1)) (- cy (* size 0.1)) size sides (rand util/TAU))
          crumb-shape-lowlight (draw/poly cx cy size sides (rand util/TAU))
          outline (draw/shape-add crumb-shape-highlight crumb-shape-lowlight)]
      {:y cy
       :outline outline
       :high crumb-shape-highlight
       :low crumb-shape-lowlight}
      ;(draw/shape g (draw/line-style 3) outline)
      ;(draw/shape g (draw/fill-style (draw/rgb 200 100 0)) crumb-shape-highlight)
      ;(draw/shape g (draw/fill-style (draw/rgb 150 75 0)) crumb-shape-lowlight)
      )))

(defn cake [g x y size & [outline-width inline-width crumbs?]]
  (let [y (- y (* size 1/2))
        layer (fn [y height]
                (draw/shape-add
                  (Ellipse2D$Float. (- x size) (- y (* size 1/2)) (* size 2) size)
                  (Ellipse2D$Float. (- x size) (+ (- y (* size 1/2)) height) (* size 2) size)
                  (Rectangle2D$Float. (- x size) y (* size 2) height)))
        cake-color (draw/rgb 200 100 0)
        highlight-color (draw/rgb 255 200 0)
        icing-color (draw/rgb 150 75 0)
        detail-color (draw/rgb 75 50 0)
        cutout (draw/shape-subtract
                 (Rectangle2D$Float. x y (* size 2) (* size 2))
                 (draw/poly [[x y]
                             [(+ x (* size 4/9)) (+ y (* size 4/9))]
                             [(+ x (* size 4/9)) (+ y (* size 2))]
                             [x (+ y (* size 2))]]))
        ;crumbs (make-crumbs x y size 20)

        crumb-zone-1 (draw/rectangle (+ x (* size 0.8)) y (* size 1/2) (* size 1))
        crumb-zone-2 (draw/rectangle (+ x (* size 0.4)) (+ y (* size 1)) size (* size 0.45))
        crumb-zone-3 (draw/rectangle (- x (* size 1/2)) (+ y (* size 1.5)) (* size 2) (* size 1/2))
        crumbs-1 (make-crumbs crumb-zone-1 size 4)
        crumbs-2 (make-crumbs crumb-zone-2 size 10)
        crumbs-3 (make-crumbs crumb-zone-3 size 10)

        cake (draw/shape-subtract
               (layer y size)
               cutout)
        outline-width (or outline-width 1.5)
        inline-width (or inline-width 3)

        cake-tex texture/cake-2
        ice-tex  texture/cake-1
        crumb-tex  texture/cake-3
        render-crumbs (fn [crumbs]
                        (doseq [{:keys [outline high low]} (sort-by :y crumbs)]
                          (draw/shape g (draw/line-style outline-width) outline)
                          (draw/shape g (draw/fill-style highlight-color) high)
                          (draw/shape g (draw/fill-style crumb-tex) low)

                          ))
        ]

    ; crumbs-1
    (render-crumbs crumbs-1)

    ; inside slice
    (draw/with-clip g cutout
      (draw/shape g (draw/fill-style cake-tex)
                  (Rectangle2D$Float. x y size size))

      (doseq [yf [(* size 0/3) (* size 1/3) (* size 2/3)]]
        (draw/line g (draw/line-style inline-width ice-tex BasicStroke/CAP_BUTT BasicStroke/JOIN_BEVEL) x (+ y yf) (+ x size) (+ y yf))
        (draw/line g (draw/line-style 1 detail-color) x (+ y yf (* inline-width 1/2)) (+ x size) (+ y yf (* inline-width 1/2))))
      ;(draw/line g (draw/line-style inline-width icing-color BasicStroke/CAP_BUTT BasicStroke/JOIN_BEVEL) x (+ y (* size 1/3)) (+ x size) (+ y (* size 1/3)))
      ;(draw/line g (draw/line-style 1 Color/RED) x (+ y inline-width) (+ x size) (+ y inline-width))
      ;(draw/line g (draw/line-style inline-width icing-color BasicStroke/CAP_BUTT BasicStroke/JOIN_BEVEL) x (+ y (* size 2/3)) (+ x size) (+ y (* size 2/3)))
      ;(draw/line g (draw/line-style 1 Color/RED) x (+ y inline-width) (+ x size) (+ y inline-width))

      (when (> size 50)
        (draw/line g (draw/line-style (* inline-width 1/4) (draw/with-alpha highlight-color 100))
                   x (+ (- y (* inline-width 1/6)) (* size 1/3))
                   (+ x size) (+ (- y (* inline-width 1/6)) (* size 1/3)))
        (draw/line g (draw/line-style (* inline-width 1/4) (draw/with-alpha highlight-color 100))
                   x (+ (- y (* inline-width 1/6)) (* size 2/3))
                   (+ x size) (+ (- y (* inline-width 1/6)) (* size 2/3))))

      (draw/shape g (draw/line-style inline-width)
                  (Rectangle2D$Float. (- x (* size 0.1)) (- y (* size 0.1))
                                      (+ size (* size 0.1))
                                      (+ size (* size 0.1))))
      )

    (render-crumbs crumbs-2)

    ; layers
    (draw/with-clip g cake
      (doseq [yf [(* size 2/3) (* size 1/3) (* size 0/3)]]
        (draw/shape g (draw/line-style (inc (inc inline-width)) detail-color) (layer (+ y yf) (* size 1/3)))
        (draw/shape g (draw/shape-style ice-tex inline-width cake-tex) (layer (+ y yf) (* size 1/3)))
        )
      ;(draw/shape g (draw/shape-style ice-tex inline-width cake-tex) (layer (+ y (* size 1/3)) (* size 1/3)))
      ;(draw/shape g (draw/shape-style ice-tex inline-width cake-tex) (layer (+ y (* size 0/3)) (* size 1/3)))
      (draw/shape g (draw/line-style (inc (inc inline-width)) detail-color) (layer y 0))
      (draw/shape g (draw/shape-style nil inline-width ice-tex) (layer y 0)))

    ; highlight
    (doseq [p (range 1 0 -1/16)
            yf (map #(- % 1) [(* size 2/3)
                              (* size 1/3)
                              (* size 0/3)
                              ])]
      (let [xf (* size p)]
        (draw/with-clip g (Rectangle2D$Float. (- x (* size 1/2) (/ xf 2))
                                              (+ (- y 0 #_(* size 1/5)) yf)
                                              xf
                                              size)
          (draw/shape g (draw/line-style (* inline-width 2/3 (- 1 p)) (draw/rgb-lerp highlight-color icing-color p))
                      (Ellipse2D$Float. (- x size) (+ (- y (* size 1/2)) yf) (* size 2) size)))))


    (do
      (draw/shape g (draw/line-style inline-width detail-color) cake)

      #_(draw/shape g (draw/line-style outline-width)
                  (draw/shape-add cake (Rectangle2D$Float. x y size size)))

      (draw/with-clip g (draw/shape-subtract (Rectangle2D$Float. (- x (* size 2)) (- y (* size 2)) (* size 4) (* size 4))
                                             (Rectangle2D$Float. (- x inline-width) (- y (* inline-width 1/2)) (+ size (* inline-width 1/2)) size))
        (draw/shape g (draw/line-style outline-width)
                    cake))

      (draw/line g (draw/line-style (* inline-width 1/4) (draw/with-alpha highlight-color 100) BasicStroke/CAP_BUTT BasicStroke/JOIN_BEVEL)
                 (- x (* outline-width 1/2)) (- y (* inline-width 1/2))
                 (+ (- x (* outline-width 1/2)) size) (- y (* inline-width 1/2))))

    (render-crumbs crumbs-3)
    ;(draw/shape g (draw/line-style 1 Color/RED) crumb-zone-1)
    ;(draw/shape g (draw/line-style 1 Color/GREEN) crumb-zone-2)
    ;(draw/shape g (draw/line-style 1 Color/BLUE) crumb-zone-3)

    ))

(defn arc-points [x y r arc-start arc-end]
  (for [theta (range arc-start arc-end 0.05)]
    [(+ x (* r (Math/sin theta)))
     (+ y (* r (Math/cos theta)))]))

(defn vp [g x y size & [with-crown?]]
  (let [x-spread 0.45
        r (* size 1/2)
        r-bit (* r (Math/sqrt 0.5))
        x (int x)
        y (int y)
        x1 (- x (* size x-spread))
        x2 (+ x (* size x-spread))

        points-left (arc-points (- x1 0.5) (- y 0.1) r (* Math/PI 0.64) (* Math/PI 1.75))
        points-right (arc-points (- x2 0.5) (- y 0.1) r (* Math/PI 0.25) (* Math/PI 1.36))
        heart-shape (draw/poly (concat points-right
                                       points-left
                                       [[x (+ y r-bit r-bit (* size x-spread))]]))

        shape (draw/shape-add
                (draw/ellipse (- x1 0.5) (- y (* 0.1)) r)
                (draw/ellipse (- x2 0.5) (- y (* 0.1)) r)
                (draw/poly [[(- x1 r-bit) (+ y r-bit)]
                            [x y]
                            [(+ x2 r-bit) (+ y r-bit)]
                            [x (+ y r-bit r-bit (* size x-spread))]])
                #_(draw/shape-subtract
                    (draw/poly x y size 4 0)
                    (draw/rectangle (- x size) (- y size) (* size 2) size)))
        highlight (draw/shape-subtract
                    (draw/ellipse 0 0 (* r-bit 3/3))
                    (draw/ellipse r-bit r-bit (* r-bit 6/3)))
        crown-angle 0.3
        crown-transform (fn [shape]
                          (-> shape
                              (draw/rotate crown-angle)
                              (draw/scale (/ size 20))
                              (draw/translate (+ x (* size 0.68)) (- y (* size 0.75)))))
        crown (draw/shape-subtract
                (draw/rectangle -10 0 20 12)
                (draw/ellipse -10.5 -8 11 15)
                (draw/ellipse -0.5 -8 11 15))
        jewel (draw/ellipse -2 6.5 4 4)
        j2 (-> jewel (draw/translate -9 0) #_(draw/ellipse -11 6.5 4 4))
        j3 (-> jewel (draw/translate 9 0) #_(draw/ellipse 7 6.5 4 4))
        h1 (-> (draw/ellipse -1.5 7 1.5 1.5))
        h2 (-> h1 (draw/translate -9 0))
        h3 (-> h1 (draw/translate 9 0))
        jc (draw/shape-style Color/BLACK 1 (draw/rgb 0 150 0))
        hc (draw/fill-style (draw/rgb 255 255 255 255))]
    (draw/shape g (draw/shape-style Color/BLACK 2 Color/RED) heart-shape)
    (draw/shape g (draw/fill-style (draw/rgb 255 255 255 128))
      (draw/ellipse (- x1 (* r-bit 1/3)) (- y (* r-bit 1/3)) (* r-bit 1/2)))
    (draw/shape g (draw/fill-style (draw/rgb 255 255 255 128))
      (draw/ellipse (- x2 (* r-bit 1/3)) (- y (* r-bit 1/3)) (* r-bit 1/2)))
    (draw/shape g (draw/fill-style (draw/rgb 255 255 255 150))
      (draw/translate highlight x2 y))
    (draw/shape g (draw/fill-style (draw/rgb 255 255 255 150))
      (draw/translate highlight x1 y))
    (when with-crown?
      (draw/shape g (draw/fill-style (draw/rgb 0 0 0 70))
        (crown-transform (draw/ellipse -10 8 20 7)))
      (draw/with-clip g (crown-transform crown)
        (draw/vshade g
                     [x (- y (* size 1/5))]
                     (draw/v* [(Math/cos crown-angle) (Math/sin crown-angle)] size)
                     1
                     (draw/rgb 230 220 0)
                     draw/shade-highlight
                     1.5))
      (draw/shape g (draw/line-style 2) (crown-transform crown))
      (draw/shape g jc (crown-transform jewel))
      (draw/shape g jc (crown-transform j2))
      (draw/shape g jc (crown-transform j3))
      (draw/shape g hc (crown-transform h1))
      (draw/shape g hc (crown-transform h2))
      (draw/shape g hc (crown-transform h3))

      ;(draw/shape g (draw/line-style 1 Color/GREEN) heart-shape)
      ;(draw/shape g (draw/fill-style Color/CYAN) (draw/poly points-right))

      )))

(defn flag [g x y size pole-length]
  (let [length (* size 2)
        height (* size 1)
        flag-shape (draw/translate
                     (draw/shape-subtract
                       (draw/poly
                         (concat
                           (draw/bezier [x (- y height)]
                                        [(+ x (* length 1/2)) (- y height)]
                                        [(+ x (* length 2/3)) (- y (* height 1/2))]
                                        [(+ x (* length 3/3)) (- y (* height 1/2))]
                                        10)
                           (draw/bezier [(+ x (* length 3/3)) (- y (* height 1/2))]
                                        [(+ x (* length 2/3)) (- y (* height 1/2))]
                                        [(+ x (* length 1/2)) y]
                                        [x y]
                                        10)))
                       (draw/ellipse (- x (* size 1/6)) (- y height) (* size 1/3) (* size)))
                     (util/mm->px 1)
                     0)
        star-shape (draw/translate (star (* size 1/3) 5) (+ x (* length 1/3)) (- y (* height 1/2)))]

    (.setColor g (draw/rgb 0 0 0 20))
    ;(.setColor g (draw/rgb 255 0 0 100))
    (doseq [n (range 0 1 1/10)]
      (.fillArc g
                (- x (* 0.3 size n))
                (+ (- y (* 0.25 size n)) (* height pole-length))
                (* 1.2 size n)
                (* 0.6 size n)
                0 360))

    (draw/line g (draw/line-style 7 Color/BLACK) x (- y height) x (+ y (* height pole-length)))
    (draw/line g (draw/line-style 3 Color/WHITE) x (- y height) x (+ y (* height pole-length)))
    (draw/line g (draw/line-style 1.5 Color/GRAY) (inc x) (- y height) (inc x) (+ y (* height pole-length)))

    (draw/with-clip g flag-shape
      (doseq [lx (range 0 length)]
        (draw/line g (draw/line-style 1 (draw/rgb-lerp
                                          (draw/rgb 170 100 220)
                                          (draw/rgb 255 100 200)
                                          ;Color/ORANGE
                                          ;Color/PINK
                                          (Math/abs (Math/sin (* (/ lx length) 4)))))
                   (+ x lx) (- y height)
                   (+ x lx) y)))
    #_(draw/with-clip g star-shape
      (doseq [x (range x (+ x length))]
        (draw/line g (draw/line-style 1 (draw/rgb-lerp Color/WHITE
                                                       Color/YELLOW
                                                       (Math/abs (Math/sin (/ x (/ size 3))))))
                   x (- y height)
                   x y)))

    #_(draw/shape g (draw/line-style 1) star-shape)

    ;(cake g (+ x (* length 0.3) (util/mm->px 1)) (- y (* height 1/2)) (* (/ 13 50) size) 1.25)
    (vp g (+ x (* length 0.3) (util/mm->px 1)) (- y (* height 0.57)) (* size 1/3) :with-crown!)

    (draw/shape g (draw/line-style 2)
      flag-shape)
    )

  )

(do ; test block

  (require '[see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  (def width 800)
  (def height 800)

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))

  (def g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  ;(cake g 50 40 20)
  ;(cake g 50 120 40)
  ;(cake g 400 400 315 10 10)

  (shield g 40 220 20)
  (shield g 120 220 30)
  (shield g 185 220 15)
  (shield g 220 220 10)
  (shield g 320 220 (* 120 1/5) 1)
  (shield g 420 220 (* 120 1/5) "∞")
  (shield g 490 220 (* 120 1/5) nil true)

  (flag g 300 300 50 1/2)
  (flag g 200 300 40 1/2)
  (flag g 100 300 30 1/2)
  (flag g 100 500 30 1.2)

  (.setColor g Color/BLACK)
  (.fillRect g 0 0 300 300)
  (price-label g 8 140 140 140 240)

  (doseq [[x y size] [[50 50 50]
                      [150 50 40]
                      [250 50 30]
                      [350 50 20]]]
    (vp g x y size :with-crown!))

  (comment
    (role-mask g 40 100 20)
    (role-mask g 80 100 15)
    (role-mask g 120 100 10)
    (empty-mask g 200 100 20)
    (move g 40 160 1)
    (move g 100 160 2)
    (move g 160 160 3)
    (move g 220 160 4)
    (shield g 40 220 20)
    (shield g 120 220 30)
    (shield g 185 220 15)
    (shield g 220 220 10)
    (damage g 40 300 1)
    (damage g 100 300 2)
    (damage g 160 300 3)
    (attack-range g 40 360 1)
    (attack-range g 100 360 2)
    (attack-range g 160 360 3)

    (cake g 300 40 20)
    (cake g 300 120 40)

    (draw/new-image-file (io/file "generated" "for-instructions") "cake"
      (fn [g]
        (cake g 100 100 90 4)))

    (flag g 300 350 50 1/2)
    (flag g 300 300 40 1/2)
    (flag g 300 250 30 1/2)

    (vp g 250 350 20 :with-crown!)
    ;(vp g 250 280 40 :with-crown!)
    (vp g 250 280 50 :with-crown!))

  (refresh-fn)

  )