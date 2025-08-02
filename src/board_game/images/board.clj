(ns board-game.images.board
  (:require
    [clojure.java.io :as io]
    [board-game.support.data :as data]
    [board-game.support.symbol :as symbol]
    [board-game.support.draw :as draw]
    [board-game.support.util :as util]
    [board-game.support.texture :as texture]
    [board-game.support.stone :as stone])
  (:import
    [javax.imageio ImageIO]
    [java.awt.image BufferedImage RenderedImage]
    [java.awt Color Polygon Graphics2D RenderingHints Rectangle BasicStroke TexturePaint]
    [java.awt.geom Area AffineTransform Rectangle2D Rectangle2D$Float Ellipse2D Ellipse2D$Float]
    [java.io File]))


(def ^:dynamic *quick?* false)

(def TAU (* 2 Math/PI))

(def appcat (partial apply concat))

(def millis-per-inch 25.4)
;(def a4-width 210)
;(def a4-height 297)

(defn rand-0 [n]
  (- (rand n) (/ n 2)))

#_(defn rand-bit [n variance]
    (+ n (rand-0 (* n variance))))

;(def width 2100)
;(def height 2970)
(def hex-radius 120)
(def x-step (* (/ (Math/sqrt 3) 2) 2 hex-radius))
(def y-step (* (/ 3 2) hex-radius))
(def n-x 10)
(def n-y 15)

(def width (* x-step (+ 0.5 n-x)))
(def height (* y-step (+ 0.9 n-y)))

(def units-per-inch x-step)
(def scale (/ millis-per-inch units-per-inch))
(def width-mm (double (* scale width)))
(def height-mm (double (* scale height)))

(def y-spare (- height (* (dec n-y) y-step)))
(def y-start (/ y-spare 2))
(def x-spare-even (- width (* (dec n-x) x-step)))
(def x-start-even (/ x-spare-even 2))
(def x-spare-odd (- width (* (dec (dec n-x)) x-step)))
(def x-start-odd (/ x-spare-odd 2))

(defn x-pos [x y]
  (+ (if (even? y) x-start-even x-start-odd)
     (* x x-step)))
(defn y-pos [y]
  (+ y-start
     (* y y-step)))
(defn xy-pos [x y]
  [(x-pos x y) (y-pos y)])

(defn y-unpos [y]
  (-> y (- y-start) (/ y-step) double Math/round))
(defn x-unpos [x y]
  (let [yu (y-unpos y)]
    (-> x (- (if (even? yu) x-start-even x-start-odd)) (/ x-step) double Math/round)))

(def all-xys
  (for [iy (range n-y)
        ix (range n-x)
        :when (or (even? iy)
                  (< ix (dec n-x)))]
    [ix iy]))

(def acceptable-tree-seeds
  [369 196 977 594 823 219 345 136
   261 884 211 150 855 472 228 149
   781 82 584 970 89 10 771 326 735 124 365
   861 49 350 735 575 120 245 55 765 612
   624 596 766 797 313 365 957 626 344 117 996 189 496 220])

(let [tree-seeds (volatile! (cycle (shuffle acceptable-tree-seeds)))]
  (defn set-next-tree-seed []
    (let [seed (first (vswap! tree-seeds rest))]
      (println "Tree seed:" seed)
      (util/set-random-seed seed))))

(def board-color (Color. 60 90 20))
;(def earth-color (Color. 150 120 50))
(def earth-color (Color. 100 75 15))
(def ground-color (Color. 100 160 30))
(def outline-color (Color. 200 200 200))
;(def wall-color (Color. 100 100 100)) 215 190 140
(def wall-color (Color. 215 190 140))
;(def wall-dark-color (Color. 75 75 75))
(def wall-dark-color (draw/rgb-lerp wall-color Color/BLACK 0.25))
(def wall-very-dark-color (draw/rgb-lerp wall-color Color/BLACK 0.75))
(def wall-outline-color Color/BLACK)
(def wall-outline-color-2 wall-very-dark-color)
(def wall-style (draw/shape-style wall-outline-color-2 3 wall-color))
(def wall-detail-style (draw/line-style 1.5 wall-outline-color-2))
(def wall-dark-style (draw/shape-style wall-outline-color-2 1.5 wall-dark-color))

(def thin-stroke (BasicStroke. (/ hex-radius 60) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
(def thick-stroke (BasicStroke. (/ hex-radius 10) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

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

(defn render-token
  ([^Graphics2D g x y colour-a colour-b]
   (render-token g x y colour-a colour-b 1.4))
  ([^Graphics2D g x y colour-a colour-b scale]
   (let [v-range (int (* scale (/ hex-radius 15)))]
     (dotimes [v v-range]
       (if (= v (- v-range 2))
         (.setColor g colour-b)
         (.setColor g colour-a))
       (.fillArc g
                 (- x (* scale (/ hex-radius 2)))
                 (- y v)
                 (* scale hex-radius)
                 (* scale (/ hex-radius 2))
                 0 360)))))

(defn- hyp [x1 y1 x2 y2]
  (let [x (- x1 x2)
        y (- y1 y2)]
    (Math/sqrt (+ (* x x) (* y y)))))

(def arrow-color (Color. 100 255 170 30))

(defn render-arrow-2
  ([^Graphics2D g x1 y1 x2 y2] (render-arrow-2 g x1 y1 x2 y2 0.56))
  ([^Graphics2D g x1 y1 x2 y2 shortening]
   (let [x-diff (- x2 x1)
         y-diff (- y2 y1)
         theta (Math/atan2 x-diff y-diff)
         length (* (Math/sqrt (+ (* x-diff x-diff) (* y-diff y-diff))) shortening)
         xc (Math/sin theta)
         yc (Math/cos theta)
         start [x1 y1]
         end (stone/v+ start (stone/v* [xc yc] length))
         v [xc yc]
         f #(stone/v* % (util/mm->px 20))]
     ;(println x-diff y-diff xc yc)
     ;(.setColor g Color/RED)
     ;(.drawLine g x1 y1 x2 y2)
     (doseq [p (range 0 1 1/33)]
       (let [across (stone/v* [yc (- xc)] p)
             along (stone/v* v p)
             head (draw/poly [end
                              (-> end (stone/v- (f along)) (stone/v+ (stone/v* (f across) 0.57)))
                              (-> end (stone/v- (f along)) (stone/v- (stone/v* (f across) 0.57)))])
             shaft (draw/poly [(-> end (stone/v- (f along)) (stone/v+ (stone/v* (f across) 2/6)))
                               (-> end (stone/v- (f along)) (stone/v- (stone/v* (f across) 2/6)))
                               (-> end (stone/v- (f along)) (stone/v- (stone/v* along length)) (stone/v- (stone/v* across 2/6)))
                               (-> end (stone/v- (f along)) (stone/v- (stone/v* along length)) (stone/v+ (stone/v* across 2/6)))])
             arrow (draw/shape-add head shaft)]
         #_(println [[x2 y2]
                     (-> [x2 y2] (stone/v- along) (stone/v+ (stone/v* across 0.57)))
                     (-> [x2 y2] (stone/v- along) (stone/v- (stone/v* across 0.57)))])
         ;(draw/shape g (draw/fill-style Color/RED) head)
         ;(draw/shape g (draw/fill-style Color/RED) shaft)
         (draw/shape g (draw/fill-style arrow-color) arrow))))))

(defn render-arrow
  ([^Graphics2D g x y] (render-arrow g x y 0))
  ([^Graphics2D g x y theta] (render-arrow g x y theta 1))
  ([^Graphics2D g x y theta length]
   (let [xc (* (util/mm->px -20) (Math/sin theta))
         yc (* (util/mm->px -20) (Math/cos theta))
         v [xc yc]]
     (doseq [p (range 0 1 1/33)]
       (let [n (stone/v* [yc (- xc)] p)
             v (stone/v* v p)
             head (draw/poly [[x y]
                              (-> [x y] (stone/v- v) (stone/v+ (stone/v* n 0.57)))
                              (-> [x y] (stone/v- v) (stone/v- (stone/v* n 0.57)))])
             shaft (draw/poly [(-> [x y] (stone/v- v) (stone/v+ (stone/v* n 2/6)))
                               (-> [x y] (stone/v- v) (stone/v- (stone/v* n 2/6)))
                               (-> [x y] (stone/v- v) (stone/v- (stone/v* v length)) (stone/v- (stone/v* n 2/6)))
                               (-> [x y] (stone/v- v) (stone/v- (stone/v* v length)) (stone/v+ (stone/v* n 2/6)))])
             arrow (draw/shape-add head shaft)]
         (draw/shape g (draw/fill-style arrow-color) arrow)))))
  #_(.setColor g (Color. 50 255 120 30))
  #_(let [y-offset (- (/ hex-radius 1.1))]
      (doseq [i (range 0 (/ hex-radius 3) (/ hex-radius 100))]
        (.fill g (polygon x (- y y-offset (/ hex-radius 2) i)
                          (- (/ hex-radius 3) i)
                          3 (/ TAU 6)))
        (.fill g (Rectangle. (+ (- x (/ hex-radius 6)) i)
                             (- y y-offset (/ hex-radius 3) (* i 1.5))
                             (* 2 (- (/ hex-radius 6) i))
                             hex-radius)))))

(defn render-tower-outline [^Graphics2D g x y sides angle & [height radius]]
  (let [height (or height 0.8)
        radius (or radius 1)
        top (- y (* hex-radius 0.4))
        bottom (+ top (* hex-radius height))
        ;clip (.getClip g)
        ]

    ;(.setClip g (polygon x y (* 1 hex-radius) 6 0))

    (.setColor g wall-outline-color)
    (doseq [i (range bottom top -0.5)]
      (draw/shape g
                  (draw/line-style 3 wall-outline-color)
                  (polygon x i (* 0.6 hex-radius radius) sides angle 0.66))
      #_(.draw g (polygon x i (* 0.6 hex-radius radius) sides angle 0.66)))

    ;(.setClip g clip)
    ))

(defn render-tower [^Graphics2D g x y sides angle & [height radius]]
  (let [height (or height 0.8)
        radius (or radius 1)
        top (- y (* hex-radius 0.4))
        middle (- y (* hex-radius 0.2))
        bottom (+ top (* hex-radius height))]

    ;(.setClip g (polygon x y (* 1 hex-radius) 6 0))
    (draw/with-clip g (polygon x y (* 1 hex-radius) 6 0)
                    ;(.setColor g (Color. 100 100 100))
                    ;(doseq [i (range bottom top -1)]
                    ;  (.fill g (polygon x i (* 0.55 hex-radius) sides angle 0.66)))

                    (let [top-poly (polygon x top (* 0.55 hex-radius radius) sides angle 0.66)
                          bottom-poly (polygon x bottom (* 0.55 hex-radius radius) sides angle 0.66)
                          extrusion (reduce draw/shape-add
                                            (for [i (range bottom top -1)]
                                              (polygon x i (* 0.55 hex-radius radius) sides angle 0.66)))]

                      (draw/shape g wall-style extrusion)

                      #_(doseq [i (range bottom top -1)]
                          (draw/shape g
                                      (draw/fill-style (draw/rgb 100 100 100))
                                      (polygon x i (* 0.55 hex-radius) sides angle 0.66)))

                      (doseq [[x1 y1 x2 y2] (map vector
                                                 (.-xpoints top-poly)
                                                 (.-ypoints top-poly)
                                                 (.-xpoints bottom-poly)
                                                 (.-ypoints bottom-poly))]
                        (when (< top y1)
                          (draw/line g wall-detail-style x1 y1 x2 y2))))
                    ; todo more wall-detail-style
                    ;(.setColor g outline-color)
                    ;(.setStroke g thin-stroke)
                    ;(.draw g (polygon x top (* 0.55 hex-radius) sides angle 0.66))
                    (draw/shape g wall-detail-style (polygon x top (* 0.55 hex-radius radius) sides angle 0.66))

                    ;(.draw g (polygon x top (* 0.45 hex-radius) sides angle 0.66))
                    (draw/shape g wall-detail-style (polygon x top (* 0.45 hex-radius radius) sides angle 0.66)))

    (draw/with-clip g (polygon x top (* 0.45 hex-radius radius) sides angle 0.66)
                    ;(.setClip g (polygon x top (* 0.45 hex-radius) sides angle 0.66))
                    (draw/shape g wall-dark-style
                                (polygon x middle (* 0.45 hex-radius radius) sides angle 0.66)   )
                    ;(.setColor g (Color. 75 75 75))
                    ;(.fill g (polygon x middle (* 0.45 hex-radius) sides angle 0.66))
                    ;(.setColor g outline-color)
                    #_(.draw g (polygon x middle (* 0.45 hex-radius) sides angle 0.66)))
    #_(.setClip g clip)

    (draw/line g
               (draw/line-style 6 (draw/rgb 0 0 0))
               x (- y (util/mm->px 7)) x (- y (util/mm->px 25)))
    (draw/line g
               (draw/line-style 3 (draw/rgb 100 50 0))
               x (- y (util/mm->px 7)) x (- y (util/mm->px 25)))

    (draw/shape g
                (draw/shape-style Color/BLACK 1 (draw/rgb 255 100 0))
                (draw/poly [[(+ x (util/mm->px 1)) (- y (util/mm->px 17))]
                            [(+ x (util/mm->px 1)) (- y (util/mm->px 25))]
                            [(+ x (util/mm->px 12)) (- y (util/mm->px 21))]]))

    ))

(defn render-wall-outline [^Graphics2D g x y left? right?]
  (let [clip (.getClip g)
        hex-area (polygon x y (* 1 hex-radius) 6 0)
        left-side (Rectangle. (- x hex-radius) (- y hex-radius) hex-radius (* 2 hex-radius))
        right-side (Rectangle. x (- y hex-radius) hex-radius (* 2 hex-radius))
        temp-clip (cond-> hex-area
                    (not left?) (shape-subtract left-side)
                    (not right?) (shape-subtract right-side))]
    ;(.setClip g temp-clip)
    (draw/with-clip g temp-clip
                    (draw/shape g
                                (draw/fill-style Color/BLACK)
                                (Rectangle. (- x hex-radius)
                                            (- y (* hex-radius 0.3))
                                            (* 2 hex-radius)
                                            (* hex-radius 0.7)))
                    #_(.setColor g wall-outline-color)
                    #_(.fill g (Rectangle. (- x hex-radius)
                                           (- y (* hex-radius 0.3))
                                           (* 2 hex-radius)
                                           (* hex-radius 0.7))))
    #_(.setClip g clip)))

(defn render-wall [^Graphics2D g x y left? right?]
  (let [clip (.getClip g)
        hex-area (polygon x y (* 1 hex-radius) 6 0)
        left-side (Rectangle. (- x hex-radius) (- y hex-radius) hex-radius (* 2 hex-radius))
        right-side (Rectangle. x (- y hex-radius) hex-radius (* 2 hex-radius))
        temp-clip (cond-> hex-area
                    (not left?) (shape-subtract left-side)
                    (not right?) (shape-subtract right-side)
                    )]

    (draw/with-clip g temp-clip
                    ;(.setClip g temp-clip)
                    ;(.setStroke g thin-stroke)
                    ;(.setColor g (Color. 100 100 100))
                    ;(.fill g (Rectangle. (- x hex-radius)
                    ;                     (- y (* hex-radius 0.25))
                    ;                     (* 2 hex-radius)
                    ;                     (* hex-radius 0.6)))
                    (draw/shape g wall-style
                                (Rectangle. (- x hex-radius)
                                            (- y (* hex-radius 0.25))
                                            (* 2 hex-radius)
                                            (* hex-radius 0.6)))
                    (draw/shape g wall-dark-style
                                (Rectangle. (- x hex-radius)
                                            (- y (* hex-radius 0.1))
                                            (* 2 hex-radius)
                                            (* hex-radius 0.15)))
                    ;(.setColor g (Color. 75 75 75))
                    #_(.fill g (Rectangle. (- x hex-radius)
                                           (- y (* hex-radius 0.1))
                                           (* 2 hex-radius)
                                           (* hex-radius 0.15)))
                    ;(.setColor g outline-color)
                    (draw/line g wall-detail-style (- x hex-radius) (- y (* hex-radius 0.2)) (+ x hex-radius) (- y (* hex-radius 0.2)))
                    (draw/line g wall-detail-style (- x hex-radius) (- y (* hex-radius 0.1)) (+ x hex-radius) (- y (* hex-radius 0.1)))
                    (draw/line g wall-detail-style (- x hex-radius) (+ y (* hex-radius 0.05)) (+ x hex-radius) (+ y (* hex-radius 0.05)))
                    (draw/line g wall-detail-style (- x hex-radius) (+ y (* hex-radius 0.1)) (+ x hex-radius) (+ y (* hex-radius 0.1)))
                    #_(.setClip g clip))))

(def terrain-map data/terrain-map)

(defmulti render (fn [_g terrain _x _y] terrain))
(defmethod render :default [_ _ _ _] nil)

(defn spot-hex [x y]
  (let [hex1 (polygon x y (* 0.95 hex-radius) 6 0)
        hex2 (polygon x y (* 1.05 hex-radius) 6 (* util/TAU 1/12))
        hex3 (polygon x y (* 1.04 hex-radius) 6 (+ (* util/TAU 1/12) 0.1))
        hex4 (polygon x y (* 1.04 hex-radius) 6 (- (* util/TAU 1/12) 0.1))
        hex5 (polygon x y (* 1.03 hex-radius) 6 (+ (* util/TAU 1/12) 0.2))
        hex6 (polygon x y (* 1.03 hex-radius) 6 (- (* util/TAU 1/12) 0.2))]
    (draw/shape-intersect hex1 hex2 hex3 hex4 hex5 hex6)))

(defn spot-hex-up [x y]
  (let [hex1' (polygon x (- y (* 0.04 hex-radius)) (* 0.95 hex-radius) 6 0)
        hex2' (polygon x (- y (* 0.04 hex-radius)) (* 1.05 hex-radius) 6 (* util/TAU 1/12))
        hex3' (polygon x (- y (* 0.04 hex-radius)) (* 1.04 hex-radius) 6 (+ (* util/TAU 1/12) 0.1))
        hex4' (polygon x (- y (* 0.04 hex-radius)) (* 1.04 hex-radius) 6 (- (* util/TAU 1/12) 0.1))]
    (draw/shape-intersect hex1' hex2' hex3' hex4')))

(defmethod render :spot [^Graphics2D g _terrain x y]
  (let [hex (spot-hex x y)
        hex-up (spot-hex-up x y)
        outline (draw/shape-add hex hex-up)
        clip (if-let [current-clip (.getClip g)]
               (draw/shape-intersect current-clip outline)
               outline)]
    (.setColor g ground-color)
    (.fill g hex)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex1)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex2)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex3)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex4)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex5)
    ;(draw/shape g (draw/line-style 1 Color/RED) hex6)
    ;(println "Spot:" (int x) (int y))
    (when
      true
      ;false
      ;(and (< 571 x 572) (= y 531.0))

      (draw/shape g (draw/fill-style texture/grass)
                  clip)

      #_(draw/with-clip g clip

                        (draw/shape g (draw/fill-style texture/grass)
                                    clip)
                        #_(doseq [xo (range (* -1 hex-radius) hex-radius 2)
                                  yo (range (* -1 hex-radius) hex-radius 2)]
                            (let [color (draw/rgb 0 (+ 128 (rand-int 128)) 0 50)
                                  xr (rand-nth [-1.5 -1 0 1 1.5])
                                  yr (rand-nth [-1.5 -1 0 1 1.5])]
                              (draw/line g (draw/line-style 1 color)
                                         (+ x xo xr) (+ y yo yr)
                                         (+ x xo xr) (+ y yo yr (* hex-radius 0.1)))
                              ))))))

(defn render-rubbish [g x y]
  (let [token-images (map #(ImageIO/read (io/file util/dir-to-print (str "token_" % ".png"))) (range 9))]
    (.setColor g (draw/rgb 0 0 0 15))
    (doseq [n (range 0.7 1.1 1/30)]
      (.fillArc g
                (- x (* 0.7 hex-radius n))
                (- y (* -0.3 hex-radius) (* 0.35 hex-radius n))
                (* 1.4 hex-radius n)
                (* 0.7 hex-radius n)
                0 360))
    (doseq [light (range 100 256 0.3)]
      (let [angle (rand-0 (/ TAU 4))
            dist (+ (rand (* 0.5 hex-radius)) (* 0.3 hex-radius))
            x-size (+ (* hex-radius 0.05) (rand (* hex-radius 0.20)))
            y-size (* hex-radius 0.25)
            x-midd (+ x (* dist (Math/sin angle)))
            y-midd (+ y (* dist (Math/cos angle)) (- (* 0.32 hex-radius)))
            x-start (- x-midd (/ x-size 2))
            y-start (- y-midd (/ y-size 2))
            ;color (draw/rgb-lerp (draw/rgb (:colour (rand-nth data/characters)))
            ;                     (draw/rgb light light light)
            ;                     (rand))
            token-img (rand-nth token-images)]

        (draw/with-transform g (draw/transforms (AffineTransform/getTranslateInstance (- x-midd) (- y-midd))
                                                (AffineTransform/getRotateInstance (+ (* TAU 1/4) (rand-0 (* TAU 1/3))))
                                                (AffineTransform/getTranslateInstance x-midd y-midd))

          (let [x-offset (/ (- y-size x-size) 5)
                overall-outline (draw/shape-add (draw/ellipse x-start y-start x-size y-size)
                                                (draw/ellipse (+ x-start x-offset) y-start x-size y-size)
                                                (draw/poly [[(+ x-start (* x-size 1/2)) y-start]
                                                            [(+ x-start (* x-size 1/2)) (+ y-start y-size)]
                                                            [(+ x-start (* x-size 1/2) x-offset) (+ y-start y-size)]
                                                            [(+ x-start (* x-size 1/2) x-offset) y-start]]))
                sf 0.9
                face (draw/ellipse (+ x-start (* x-size 1/2) (* x-size -1/2 sf))
                                   (+ y-start (* y-size 1/2) (* y-size -1/2 sf))
                                   (* x-size sf)
                                   (* y-size sf))]
            (draw/shape g (draw/shape-style Color/BLACK 1 (Color. 128 128 128)) overall-outline)
            (draw/with-clip g face
              (.drawImage g token-img x-start y-start x-size y-size nil))

            (draw/shape g (draw/fill-style (draw/rgb 0 0 0 (- 255 light)))
                        overall-outline)
            (draw/shape g (draw/fill-style (draw/rgb 0 0 0 15))
                        face)
            )

          #_(draw/shape g (draw/shape-style Color/WHITE 10 Color/BLACK)
                      (draw/ellipse x-start y-start x-size y-size))
          #_(let [x-offset (/ (- y-size x-size) 5)]
            (draw/shape g (draw/shape-style Color/BLACK 1 Color/WHITE)
                        (draw/shape-add (draw/ellipse x-start y-start x-size y-size)
                                        (draw/ellipse (+ x-start x-offset) y-start x-size y-size)
                                        (draw/poly [[(+ x-start (* x-size 1/2)) y-start]
                                                    [(+ x-start (* x-size 1/2)) (+ y-start y-size)]
                                                    [(+ x-start (* x-size 1/2) x-offset) (+ y-start y-size)]
                                                    [(+ x-start (* x-size 1/2) x-offset) y-start]]))))
          #_(draw/with-clip g (let [sf 0.9]
                              (draw/ellipse (+ x-start (* x-size 1/2) (* x-size -1/2 sf))
                                            (+ y-start (* y-size 1/2) (* y-size -1/2 sf))
                                            (* x-size sf)
                                            (* y-size sf)))

            ;(.setColor g color)
            #_(draw/shape g (draw/shape-style Color/WHITE 10 color)
                        (draw/ellipse x-start y-start x-size y-size))
            (.drawImage g token-img x-start y-start x-size y-size nil)
            )

          #_(draw/shape g (draw/fill-style (draw/rgb 0 0 0 (- 255 light)))
                      (draw/ellipse x-start y-start x-size y-size))

          ;(.fillArc g x-start y-start x-size y-size 0 360)
          )

        ;(.setColor graphics color)
        #_(.setTransform g (doto (AffineTransform.)
                                  (.translate x-midd y-midd)
                                  (.rotate (rand TAU))
                                  (.translate (- x-midd) (- y-midd))
                                  ))
        #_(.fillArc g x-start y-start x-size y-size 0 360)))
    ))

(defn rotate-2d [[x y] theta]
  [(+ (* x (Math/cos theta))
      (* y (Math/sin theta)))
   (- (* y (Math/cos theta))
      (* x (Math/sin theta)))])

(defn rotate-3d [[x y z] yaw pitch]
  (let [[x' z'] (rotate-2d [x z] pitch)
        [x'' y'] (rotate-2d [x' y] yaw)]
    [x'' y' z']))

(defn add-3d [a b]
  (map + a b))

(defn scale-3d [a scale]
  (map #(* scale %) a))

(defn create-branches [scale transform]
  (if (< 0.4 scale)
    (let [origin [0 0 0]
          new-point [0 0 hex-radius]
          angle (rand TAU)
          next-scale (* scale (+ 0.7 (rand 0.1)))
          branch [:branch origin new-point next-scale]
          transform-branch (fn [[render-type start end scale :as _arg]]
                             [render-type (transform start) (transform end) scale])]
      (map transform-branch
           (concat [branch]
                   (apply concat
                          (cons (create-branches next-scale #(add-3d new-point (scale-3d (rotate-3d % angle (/ TAU 60)) scale)))
                                (for [spread (range 0 TAU (/ TAU 7))
                                      :let [r (rand)]]
                                  (create-branches next-scale #(add-3d new-point (scale-3d (rotate-3d % (+ angle spread r) (/ TAU 15)) next-scale)))))))))
    [[:leaf [0 0 hex-radius] [0 0 hex-radius]]]))

(defn render-branches [g x y branches]
  (.setColor g (draw/rgb 0 0 0 20))
  (doseq [n (range 0 1 1/10)]
    (.fillArc g
              (- x (* 0.6 hex-radius n))
              (- y (* 0.25 hex-radius n))
              (* 1.2 hex-radius n)
              (* 0.6 hex-radius n)
              0 360))
  (.setColor g Color/BLACK)
  (doseq [n (range 0 (* hex-radius 0.09) 0.1)]
    (.draw g (polygon x
                      (+ y (* n 7) (- (* hex-radius 0.1 6)))
                      (inc (inc (/ (* n n) (* hex-radius 0.1))))
                      10 0 0.5)))
  (doseq [[render-type :as branch] (sort-by (fn [[_ _ [_ y z]]] (- z y)) branches)
          :when (= render-type :branch)]
    (let [[_ [x1 y1 z1] [x2 y2 z2] scale] branch
          gx1 (+ x x1)
          gy1 (- y (* y1 0.5) (* z1 0.4))
          gx2 (+ x x2)
          gy2 (- y (* y2 0.5) (* z2 0.4))
          offset (* 4 scale)]
      (.setStroke g (BasicStroke. (inc (inc (inc (* 2 offset)))) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      (.drawLine g (- gx1 offset) (- gy1 offset) (- gx2 offset) (- gy2 offset))
      (.drawLine g gx1 gy1 gx2 gy2)))

  (.setColor g (Color. 100 20 0))
  (doseq [n (range 0 (* hex-radius 0.09) 0.1)]
    (.fill g (polygon x
                      (+ y (* n 7) (- (* hex-radius 0.1 6)))
                      (/ (* n n) (* hex-radius 0.1))
                      10 0 0.5)))
  (doseq [[render-type :as branch] (sort-by (fn [[_ _ [_ y z]]] (- z y)) branches)
          :when (= render-type :branch)]
    (let [[_ [x1 y1 z1] [x2 y2 z2] scale] branch
          gx1 (+ x x1)
          gy1 (- y (* y1 0.5) (* z1 0.4))
          gx2 (+ x x2)
          gy2 (- y (* y2 0.5) (* z2 0.4))
          offset (* 4 scale)]
      (.setStroke g (BasicStroke. (* 2 offset) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      (.setColor g (Color. 140 40 20))
      (.drawLine g (- gx1 offset) (- gy1 offset) (- gx2 offset) (- gy2 offset))
      (.setColor g (Color. 100 20 0))
      (.drawLine g gx1 gy1 gx2 gy2))))

(defn render-leaves [g x y branches]
  (doseq [[render-type :as branch] (sort-by (fn [[_ _ [_ y z]]] (- z y)) branches)
          :when (= render-type :leaf)]
    (let [[_ [x1 y1 z1]] branch
          gx1 (+ x x1)
          gy1 (- y (* y1 0.5) (* z1 0.4))]
      (.setColor g (Color. 0 0 0 10))
      (.draw g (polygon gx1 gy1 (* hex-radius 0.05) 10 0))
      (.setColor g (Color. 0 (int (+ 100 (rand 155))) 0 30))
      (.fill g (polygon gx1 gy1 (* hex-radius 0.05) 10 0)))))

(defn render-shield [^Graphics2D g x y quantity]
  (symbol/shield g
                 (+ x (* hex-radius 0.5))
                 (+ y (* hex-radius 0.25))
                 (inc (* hex-radius 1/5))
                 quantity)
  (symbol/shield g
                 (+ x (* hex-radius 0.5))
                 (+ y (* hex-radius 0.25))
                 (* hex-radius 1/5)
                 quantity))

(defn render-shield-glow [^Graphics2D g x y]
  (let [hex (spot-hex x y)
        scaled-hex #(-> hex
                        (draw/scale %)
                        (draw/center x y))]
    (doseq [n (range 0 1 0.1)]
      (draw/shape g (draw/fill-style (draw/rgb 50 200 255 (* n 255)))
                  (scaled-hex (- 1.1 (* n 0.1)))))))

(defmethod render :trmd [^Graphics2D g _terrain x y]
  (set-next-tree-seed)
  (let [branches (create-branches 1.0 identity)]
    ;(render-shield-glow g x y)
    (render g :spot x y)
    (render-branches g x (+ y (* hex-radius 0.4)) branches)
    ;(render-token g x y board-color ground-color)
    (render-rubbish g x y)
    (render-leaves g x (+ y (* hex-radius 0.4)) branches)
    (render-shield g (int x) y nil)))

(defmethod render :tree [^Graphics2D g _terrain x y]
  (set-next-tree-seed)
  ;(println [(x-unpos x y) (y-unpos y)])
  (when (= [5 11] [(x-unpos x y) (y-unpos y)])
    (util/set-random-seed 49))
  (let [branches (create-branches 1.0 identity)]
    ;(render-shield-glow g x y)
    (render g :spot x y)
    (when-not *quick?*
      (render-branches g x (+ y (* hex-radius 0.4)) branches)
      (render-leaves g x (+ y (* hex-radius 0.4)) branches))
    (render-shield g (int x) y nil)))

(defmethod render :cake [^Graphics2D g _terrain x y]
  (render g :spot x y)
  (let [x-spread (util/mm->px 23)
        x-raw (x-unpos x y)
        y-raw (y-unpos y)]
    (println x y x-raw y-raw)
    (case (y-unpos y)
      0 (if (< 5 (x-unpos x y))

          (let [[x2 y2] (xy-pos (- x-raw 2) (dec y-raw))] (render-arrow-2 g x y x2 y2))
          (let [[x2 y2] (xy-pos (inc x-raw) (dec y-raw))] (render-arrow-2 g x y x2 y2))
          ;(render-arrow g (- x x-spread x-spread) y (* util/TAU 2/12))
          ;(render-arrow g (+ x x-spread x-spread) y (* util/TAU 10/12))
          )
      1 (if (< 5 (x-unpos x y))
          (let [[x2 y2] (xy-pos (- x-raw 1) (dec y-raw))] (render-arrow-2 g x y x2 y2))
          (let [[x2 y2] (xy-pos (+ x-raw 2) (dec y-raw))] (render-arrow-2 g x y x2 y2))
          #_(render-arrow g (- x x-spread) (- y (* 1.25 hex-radius)) (* util/TAU 1/12))
          #_(render-arrow g (+ x x-spread) (- y (* 1.25 hex-radius)) (* util/TAU -1/12)))
      2 #_(if (even? (x-unpos x y))
          (render-arrow g (- x x-spread) (- y (* 1.25 hex-radius)) (* util/TAU 1/12))
          (render-arrow g (+ x x-spread) (- y (* 1.25 hex-radius)) (* util/TAU -1/12)))
      (let [[x2 y2] (xy-pos x-raw (- y-raw 2))] (render-arrow-2 g x y x2 y2))
      #_(render-arrow g x (- y (* 2 hex-radius)) (* util/TAU 0/12) 2)))
  #_(render-arrow g x (- y (* 1.25 hex-radius))
                  (if (even? (x-unpos x y))
                    [0 (util/mm->px -20)]
                    [(util/mm->px -6) (util/mm->px -17)]))
  #_(doseq [_ (range 10)]
    (let [angle (rand TAU)
          dist (rand (* hex-radius 0.45))C ;
          size (rand (* hex-radius 0.04))
          cx (+ x (* hex-radius 0.2) (* dist (Math/sin angle)))
          cy (+ y (* hex-radius -0.1) (* dist 0.5 (Math/cos angle)) (* 0.3 hex-radius))
          crumb-shape-highlight (polygon (- cx (* hex-radius 0.01)) (- cy (* hex-radius 0.01)) size (rand-nth [3 5 6 7]) (rand TAU))
          crumb-shape-lowlight (polygon cx cy size (rand-nth [3 5 6 7]) (rand TAU))
          outline (draw/shape-add crumb-shape-highlight crumb-shape-lowlight)]
      (draw/shape g (draw/line-style 3) outline)
      (draw/shape g (draw/fill-style (draw/rgb 200 100 0)) crumb-shape-highlight)
      (draw/shape g (draw/fill-style (draw/rgb 150 75 0)) crumb-shape-lowlight)))
  (symbol/cake g x y (* hex-radius 1/3) 3))

(defmethod render :strt [^Graphics2D g _terrain x y]
  ;(render-shield-glow g x y)
  (render g :spot x y)
  ;(.setColor g board-color)
  #_(render-arrow g
                x
                ;y
                (+ y (util/mm->px 14)))
  (let [[x1 y1] (xy-pos (x-unpos x y) (inc (inc (y-unpos y))))]
    (render-arrow-2 g x1 y1 x y 0.85))
  (let [r (util/mm->px 20)]
    (draw/shape g
                (draw/shape-style (draw/rgb 0 0 0 100) 1.5 (draw/rgb 0 0 0 25))
                (draw/shape-subtract
                  (draw/ellipse (- x r) (- y r) (* 2 r) (* 2 r))
                  (symbol/mask-shape x (+ y (util/mm->px 0.7)) 40))))
  (render-shield g (int x) y nil)
  #_(symbol/empty-mask g x (+ y (util/mm->px 0.7)) 40)

  )

(defmethod render :midd [^Graphics2D g _terrain x y]
  (render g :spot x y)
  (when-not *quick?* (render-rubbish g x y))
  )

(defn adj-circle [x y dx dy r]
  (polygon (+ x (* dx x-step))
           (+ y (* dy y-step))
           (* r hex-radius)
           60 0))

(defmethod render :brga [^Graphics2D g _terrain x y]
  (let [clip (.getClip g)]

    (doseq [i (range hex-radius 0 (- (/ hex-radius 50)))]
      (.setColor g (Color. 0 0 0 20))
      (.fill g (polygon x y i 6 0)))

    (.setClip g (-> (polygon x y hex-radius 6 0)
                    (shape-subtract (adj-circle x y -0.5 -1 1.1))
                    (shape-subtract (adj-circle x y 0.5 1 1.1))))
    (render g :spot x y)
    (.setClip g (-> (polygon x y hex-radius 6 0)
                    (shape-subtract (adj-circle x y -0.5 -1 1.05))
                    (shape-subtract (adj-circle x y 0.5 1 1.05))))

    ;(.setColor g outline-color)
    ;(.setStroke g (BasicStroke. (/ hex-radius 5)))
    ;(.draw g (adj-circle x y -0.5 -1 1.15))
    ;(.draw g (adj-circle x y 0.5 1 1.15))
    ;
    ;(.setColor g (Color. 100 100 100))
    ;(.setStroke g (BasicStroke. (/ hex-radius 8)))
    ;(.draw g (adj-circle x y -0.5 -1 1.15))
    ;(.draw g (adj-circle x y 0.5 1 1.15))

    (.setClip g clip)

    (stone/do-instructions g
                           (concat (stone/arc (- x (* x-step 1/2))
                                              (- y y-step 10)
                                              [0 10] 80
                                              (* hex-radius 1.2) 0.9 (* util/TAU 1/12))
                                   (stone/arc (+ x (* x-step 1/2))
                                              (+ y y-step -10)
                                              [0 10] 80
                                              (* hex-radius 1.2) 0.9 (* util/TAU 7/12))))

    (.setClip g clip)))

(defmethod render :brgb [^Graphics2D g _terrain x y]
  (let [clip (.getClip g)]
    (doseq [i (range hex-radius 0 (- (/ hex-radius 50)))]
      (.setColor g (Color. 0 0 0 20))
      (.fill g (polygon x y i 6 0)))

    (.setClip g (-> (polygon x y hex-radius 6 0)
                    (shape-subtract (adj-circle x y -1 0 1.05))
                    (shape-subtract (adj-circle x y 1 0 1.05))))
    (render g :spot x y)

    ;(.setColor g outline-color)
    ;(.setStroke g (BasicStroke. (/ hex-radius 5)))
    ;(.draw g (adj-circle x y -1 0 1.15))
    ;(.draw g (adj-circle x y 1 0 1.15))
    ;
    ;(.setColor g (Color. 100 100 100))
    ;(.setStroke g (BasicStroke. (/ hex-radius 8)))
    ;(.draw g (adj-circle x y -1 0 1.15))
    ;(.draw g (adj-circle x y 1 0 1.15))

    (.setClip g clip)

    (stone/do-instructions g
                           (concat (stone/arc (- x x-step)
                                              (- y 10)
                                              [0 10] 80
                                              (* hex-radius 1.15) 0.9 (* util/TAU 3/12))
                                   (stone/arc (+ x x-step)
                                              (- y 10)
                                              [0 10] 80
                                              (* hex-radius 1.15) 0.9 (* util/TAU 9/12))))
    ))

(defmethod render :rivr [^Graphics2D _g _terrain _x _y]
  ; River rendered separately.
  )

(def wall-y-offset (util/mm->px 6))

(defmethod render :wall [^Graphics2D g _terrain x y]
  ;(render-wall-outline g x (- y wall-y-offset) true true)
  ;(render-wall g x (- y wall-y-offset) true true)
  )

(defmethod render :twrr [^Graphics2D g _terrain x y]
  #_(let [sides 7
          angle (- (/ TAU 2) (rand (/ TAU 14)))]
      ;(render-tower-outline g x (- y wall-y-offset) sides angle)
      (render-wall-outline g x (- y wall-y-offset) true false)
      (render-wall g x (- y wall-y-offset) true false)
      ;(render-tower g x (- y wall-y-offset) sides angle)

      (stone/rings g x (+ y (util/mm->px 4)) [0 9] 30 80 0.8 11)
      (stone/h-wall g (- x 500) y 500 20 12 9 3)

      ))

(defmethod render :twrl [^Graphics2D g _terrain x y]
  #_(let [sides 7
          angle (+ (/ TAU 2) (rand (/ TAU 14)))]
      (render-tower-outline g x (- y wall-y-offset) sides angle)
      (render-wall-outline g x (- y wall-y-offset) false true)
      (render-wall g x (- y wall-y-offset) false true)
      (render-tower g x (- y wall-y-offset) sides angle)))

(defn board-outline [margin]
  (->> (for [[ix iy] all-xys]
         (polygon (x-pos ix iy) (y-pos iy) (* margin hex-radius) 6 0))
       (reduce shape-add)))

(def river-xys
  (concat [[-2 7] [-1 7]]
          (for [[ix iy] all-xys
                :let [terrain (get-in terrain-map [iy ix])]
                :when (#{:brga :brgb :rivr} terrain)]
            [ix iy])
          [[9 13] [10 14] [9 15] [9 16]]))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
;(defonce refresh-fn (see/see image :only-draw-when-updated? true))

(defn scale [a b prop]
  (+ (* prop b) (* (- 1 prop) a)))

(defn draw-line [g xa ya xb yb colour width]
  (.setPaint g colour)
  (.setStroke g (BasicStroke. width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  (.drawLine g xa ya xb yb))

(defn make-reeds [[xc yc] size density]
  (apply
    concat
    (for [_ (range density)
          :let [light (+ 25 (rand-int 230))
                blue (int (* 0.5 light))
                green (int light)
                red (int (* 0.4 green))
                angle (rand TAU)
                r (rand size)
                x (+ xc (* r (Math/sin angle)))
                y (+ yc (* r (Math/cos angle)))
                flower? (zero? (rand-int 5))
                flower-height 8
                height (+ 10 (rand 10) (if flower? 5 0))
                ]]
      [{:z-order (- y 600)
        :f       (fn [g] ; underwater part
                   (doseq [n [1 2 3 4 5 6 7]]
                     (.setColor g (Color. 20 100 0 50))
                     (.setStroke g (BasicStroke. 3))
                     (.drawLine g x y x (+ y n))))}
       {:z-order (- y 500)
        :f       (fn [g] ; shadow on water surface
                   (.setColor g (draw/rgb 0 0 0 30))
                   (doseq [n (range 0 1 1/5)]
                     (.fillArc g
                               (- x (* 6 n))
                               (- y (* 2 n))
                               (* 12 n)
                               (* 6 n)
                               0 360)))}
       {:z-order y
        :f       (fn [g]
                   (when flower?
                     (.setColor g Color/BLACK)
                     (.setStroke g (BasicStroke. 3))
                     (.drawLine g x y x (- y height))

                     (.setColor g Color/BLACK)
                     (.setStroke g (BasicStroke. 6 BasicStroke/CAP_ROUND BasicStroke/JOIN_BEVEL))
                     (.drawLine g x (- y height) x (- y height flower-height))

                     ; Stalk?
                     (.setColor g (Color. 180 200 130))
                     (.setStroke g (BasicStroke. 1))
                     (.drawLine g x y x (- y height))

                     ; flower shadow
                     (.setColor g (Color. 100 50 50) #_(Color. 100 50 50))
                     (.setStroke g (BasicStroke. 2 BasicStroke/CAP_ROUND BasicStroke/JOIN_BEVEL))
                     (.drawLine g (+ x 1) (- y height -1) (+ x 1) (- y height (dec flower-height)))

                     ; flower
                     (.setColor g (Color. 125 75 75) #_(Color. 150 100 100))
                     (.setStroke g (BasicStroke. 4 BasicStroke/CAP_ROUND BasicStroke/JOIN_BEVEL))
                     (.drawLine g x (- y height) x (- y height flower-height))

                     ; flower hightlight
                     (.setColor g (Color. 175 125 125))
                     (.setStroke g (BasicStroke. 2 BasicStroke/CAP_ROUND BasicStroke/JOIN_BEVEL))
                     (.drawLine g (- x 1) (- y height 1) (- x 1) (- y height flower-height)))

                   (dotimes [_ (inc (rand-int 3))]
                     (let [yd (- (rand height))
                           xd (rand-0 height)
                           points (draw/bezier [x y]
                                               [x (+ y (* yd 1/2))]
                                               [(+ x (* xd 1/2)) (+ y yd)]
                                               [(+ x xd) (+ y yd)] 5)]
                       ;  (println x y xd yd)

                       (.setColor g Color/BLACK)
                       (draw/lines g points 3 1.5)

                       (.setColor g (Color. 50 175 0))
                       (draw/lines g points 2 0.5)

                       (.setColor g (Color. 40 150 0))
                       (draw/lines g (map #(draw/v+ % [1 1]) points) 1 1)
                       ))

                   )}])))

(defn reed-flow [a _b p]
  (make-reeds a
              (+ 20 (* (draw/p->sin p) 5))
              (* (draw/p->sin p) 10)))

(defn make-rocks [a radius density]
  (appcat
    (for [_ (range density)]
      (let [[cx cy] (draw/v+ a [(rand-0 radius) (rand-0 radius)])
            size (+ 3 (rand-int 5))
            rock-shape (draw/poly 0 0 size (+ 5 (rand-int 4)) (rand))
            rock-color (-> (draw/rgb 215 190 140)
                           (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                           (draw/rgb-lerp (draw/rgb 0 0 0) (+ 0.3 (rand 0.25))))
            under (if (zero? (rand-int 2)) -10000 0)]
        [{:z-order (+ cy (* 10 size) under)
          :shape   (-> rock-shape (draw/center cx cy))
          :style   (draw/fill-style rock-color)}
         {:z-order (+ cy 1 (* 10 size) under)
          :shape   (-> rock-shape (draw/scale 0.75) (draw/center (dec cx) (dec cy)))
          :style   (draw/fill-style (draw/rgb-lerp rock-color Color/WHITE 0.2))}
         {:z-order (+ cy 2 (* 10 size) under)
          :shape   (-> rock-shape (draw/center cx cy))
          :style   (draw/line-style 1 Color/BLACK)}]))))

(defn rock-flow []
  (fn [a b p]
    (apply
      concat
      (for [_ (range (* (draw/p->sin p) 5))]
        (let [[cx cy] (draw/v+ a [(rand-0 30) (rand-0 30)])
              size (+ 3 (rand-int 5))
              rock-shape (draw/poly 0 0 size (+ 5 (rand-int 4)) (rand))
              rock-color (-> (draw/rgb 215 190 140)
                             (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                             (draw/rgb-lerp (draw/rgb 0 0 0) (+ 0.3 (rand 0.25))))
              under (if (zero? (rand-int 2)) -10000 0)]
          [{:z-order (+ cy (* 10 size) under)
            :shape   (-> rock-shape (draw/center cx cy))
            :style   (draw/fill-style rock-color)}
           {:z-order (+ cy 1 (* 10 size) under)
            :shape   (-> rock-shape (draw/scale 0.75) (draw/center (dec cx) (dec cy)))
            :style   (draw/fill-style (draw/rgb-lerp rock-color Color/WHITE 0.2))}
           {:z-order (+ cy 2 (* 10 size) under)
            :shape   (-> rock-shape (draw/center cx cy))
            :style   (draw/line-style 1 Color/BLACK)}])))))

(defn make-mud [a radius density]
  (appcat
    (for [_ (range density)]
      (let [[cx cy] (draw/v+ a [(rand-0 radius) (rand-0 radius)])
            size (+ 3 (rand-int 5))
            mud-shape (draw/poly 0 0 size (+ 5 (rand-int 4)) (rand))
            mud-color (-> (Color. 50 40 10 50)
                          (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.5)))]
        [{:z-order -110000
          :shape   (-> mud-shape (draw/center cx cy))
          :style   (draw/fill-style texture/mud)}
         {:z-order -100000
          :shape   (-> mud-shape (draw/center cx cy))
          :style   (draw/fill-style mud-color)}]))))

(defn mud-flow [a b p]
  (apply
    concat
    (for [_ (range (* (draw/p->sin p) 5))]
      (let [[cx cy] (draw/v+ a [(rand 30) (rand 30)])
            size (+ 3 (rand-int 5))
            mud-shape (draw/poly 0 0 size (+ 5 (rand-int 4)) (rand))
            mud-color (-> (Color. 50 40 10 50)
                           (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.5)))]
        [{:z-order -110000
          :shape   (-> mud-shape (draw/center cx cy))
          :style   (draw/fill-style texture/mud)}
         {:z-order -100000
          :shape   (-> mud-shape (draw/center cx cy))
          :style   (draw/fill-style mud-color)}]))))

(defn smooth-random-seq [variance]
  (let [!amount (volatile! (/ variance 2))]
    (map #(vswap! !amount draw/lerp % 0.4) (repeatedly #(rand variance)))))

(defn smooth-swinging-seq [n]
  (let [!state (volatile! {:x (/ n 2)
                           :dx 0
                           :ddx 0})
        step (fn [{:keys [x dx ddx]}]
               (println x dx ddx)
               (if (<= 0 x n)
                 {:x   (+ x dx)
                  :dx  (+ dx ddx)
                  :ddx (+ (rand-0 (/ n 10))
                          (* (- (/ n 2) x) 1/10))}
                 {:x   (+ x dx)
                  :dx  (+ dx ddx)
                  :ddx (* (- (/ n 2) x) 1/4)}))]
    (repeatedly (fn []
                  (:x (vswap! !state step))))))

(defn pulse [amplitude duration]
  (map #(* (draw/p->sin %) amplitude) (range 0 1 (/ 1 duration))))

(def reedses-top
  (map max
       (repeat 10000 0)
       (concat (repeat 133 0) (pulse 5 20) (repeat 100 0))
       (concat (repeat 145 0) (pulse 5 20) (repeat 100 0))
       (concat (repeat 155 0) (pulse 5 20) (repeat 100 0))
       (concat (repeat 80 0) (pulse 10 20) (repeat 10000 0))))

(def reedses-middle
  (map max
       (repeat 10000 0)
       (concat (repeat 145 0) (pulse 5 30) (repeat 100 0))))

(def reedses-bottom
  (map max
       (repeat 10000 0)
       (concat (repeat 110 0) (pulse 5 10) (repeat 100 0))
       (concat (repeat 140 0) (pulse 5 40) (repeat 100 0))))

(def rockses-top
  (concat
    (drop 5 (pulse 10 20))
    (repeat 25 0)
    (pulse 8 20)
    ;(repeat 10 10)
    (repeat 10000 0)))

(def rockses-middle
  (concat
    (repeat 50 0)
    (pulse 5 15)
    ;(repeat 10 10)
    (repeat 10000 0)))

(def rockses-bottom
  (concat
    (repeat 30 0)
    (pulse 5 15)
    (repeat 10 0)
    (pulse 10 15)
    (repeat 10000 0)))

(def mud-top
  (smooth-random-seq 20)
  #_(smooth-swinging-seq 20))

(def mud-bottom
  (smooth-random-seq 20))

(defn draw-river [g]
  (let [variance 0.2
        points (->> (for [[[x1 y1] [x2 y2] [x3 y3]] (partition 3 1 river-xys)]
                      (let [[xp1 yp1] (xy-pos x1 y1)
                            [xp2 yp2] (xy-pos x2 y2)
                            [xp3 yp3] (xy-pos x3 y3)
                            xh1 (scale xp2 xp1 0.5)
                            xh2 xp2
                            xh3 (scale xp2 xp3 0.5)
                            yh1 (scale yp2 yp1 0.5)
                            yh2 yp2
                            yh3 (scale yp2 yp3 0.5)]
                        (for [prop (range 0 1 0.1)]
                          (let [xa (scale xh1 xh2 prop)
                                ya (scale yh1 yh2 prop)
                                xb (scale xh2 xh3 prop)
                                yb (scale yh2 yh3 prop)]
                            [(scale xa xb prop)
                             (scale ya yb prop)]))))
                    (apply concat)
                    (map vector
                         (smooth-random-seq variance)
                         reedses-top reedses-middle reedses-bottom
                         rockses-top rockses-middle rockses-bottom
                         mud-top mud-bottom)
                    (map (fn [[v rt rm rb kt km kb mt mb [x y]]]
                           [x y v rt rm rb kt km kb mt mb]))
                    (partition 2 1))
        clip (.getClip g)]

    (println "Points: " (count points))
    ;(.setClip g (board-outline 1.0))

    ; Soft Brown edges
    (doseq [[[xa ya _wa] [xb yb _wb]] points
            f (range 0.75 1.1 0.05)]
      (draw-line g xa ya xb yb (Color. 50 40 10 30) (/ hex-radius (+ f (rand variance)))))

    ; Brown line
    (doseq [[[xa ya wa] [xb yb wb]] points]
      (draw-line g xa ya xb yb texture/mud #_(Color. 50 40 10 250) (* hex-radius (+ 0.95 wa))))

    ; Blue lines
    (doseq [[[xa ya wa] [xb yb wb]] points]
      (draw-line g xa ya xb yb
                 #_(Color. 100 100 250 127)
                 (Color. 50 50 150 127)
                 (/ hex-radius (+ 1.1 wa))))
    ;(doseq [[[xa ya] [xb yb]] points]
    ;  (draw-line g xa ya xb yb (Color. 80 80 200 127) (/ hex-radius 1.1)))
    ;(doseq [[[xa ya] [xb yb]] points]
    ;  (draw-line g xa ya xb yb (Color. 60 60 150) (/ hex-radius 1.4)))

    ; todo--------------------

    (let [flow-lines (for [[[xa ya wa r] [xb yb wb]] points
                           _ (range 1500)
                           :let [light (+ 25 (rand-int 230))
                                 blue (int light)
                                 green (int (* 0.4 light))
                                 red (int (* 0.4 green))
                                 scale (rand)
                                 x-offset (* scale (rand-0 hex-radius))
                                 y-offset (* scale (rand-0 hex-radius))
                                 x-offset-a (* x-offset (+ 0.75 (* wa 0.01) #_0.2))
                                 y-offset-a (* y-offset (+ 0.75 (* wa 0.01) #_0.2))
                                 x-offset-b (* x-offset (+ 0.75 (* wb 0.01) #_0.2))
                                 y-offset-b (* y-offset (+ 0.75 (* wb 0.01) #_0.2))]]
                       {:z-order 0
                        :f       (fn [g]
                                   (.setColor g (Color. red green blue (int (+ 50 (* 50 (- 1 scale))))))
                                   (.setStroke g (BasicStroke. 1))
                                   (.drawLine g
                                              (+ x-offset-a xa)
                                              (+ y-offset-a ya)
                                              (+ x-offset-b xb)
                                              (+ y-offset-b yb)))})
          reeds (appcat (for [[[xa ya _wa rt rm rb] [xb yb]] points]
                          (let [v (draw/v* [(- yb ya) (- xa xb)] 1.2)]
                            (concat
                              (make-reeds (draw/v+ [xa ya] v) (+ 20 rt) rt)
                              (make-reeds [xa ya] (+ 20 rm) rm)
                              (make-reeds (draw/v- [xa ya] v) (+ 20 rb) rb)))))
          #_#_reeds (appcat (draw/for-points (draw/bezier [1300 1940] [1450 1950] [1460 2020] [1490 2050] 20)
                          reed-flow))
          rocks (appcat (for [[[xa ya _wa rt rm rb kt km kb] [xb yb]] points]
                          (let [v (draw/v* [(- yb ya) (- xa xb)] 1.2)]
                            (concat
                              (make-rocks (draw/v+ [xa ya] v) (+ 20 kt) kt)
                              (make-rocks [xa ya] (+ 20 km) km)
                              (make-rocks (draw/v- [xa ya] v) (+ 20 kb) kb)))))
          #_#_rocks (appcat
                  (draw/for-points (draw/bezier [1200 1990] [1350 2000] [1430 2070] [1440 2100] 20)
                    (rock-flow)))
          mud (appcat (for [[[xa ya _wa rt rm rb kt km kb mt mb] [xb yb]] points]
                        (let [v [(- yb ya) (- xa xb)]]
                          (concat
                            (make-mud (draw/v+ [xa ya] (draw/v* v 2.5)) 20 mt)
                            (make-mud (draw/v+ [xa ya] (draw/v* v 2.0)) 30 mt)
                            (make-mud (draw/v- [xa ya] (draw/v* v 2.0)) 30 mb)
                            (make-mud (draw/v- [xa ya] (draw/v* v 2.5)) 20 mb)))))

          #_#_mud (appcat (draw/for-points (draw/bezier [1200 1990] [1350 2000] [1490 2120] [1490 2200] 40)
                        mud-flow))
          ]

      (draw/do-instructions g (concat flow-lines reeds rocks mud))
      )

    ; todo: define where these are more naturally - f.e. a bezier?
    ;(draw-reeds g [1380 1950] 20 30)
    ;(draw-reeds g [1420 1980] 30 50) ;(draw-reeds g [1460 1990] 60 50) ;(draw-reeds g [1460 1990] 40 1000)
    ;(draw-reeds g [1460 2020] 30 30)
    ;(draw-reeds g [1490 2050] 15 10)

    #_(draw/do-instructions
      g
      (apply
        concat
        (draw/for-points (draw/bezier [1300 1940] [1450 1950] [1460 2020] [1490 2050] 20)
          (fn [a b p]
            (make-reeds g a
                        (+ 20 (* (draw/p->sin p) 5))
                        (* (draw/p->sin p) 10))))))

    #_(draw/do-instructions
      g
      (apply
        concat
        (draw/for-points (draw/bezier [1200 1990] [1400 2000] [1460 2070] [1490 2100] 20)
          (fn [a b p]
            (apply
              concat
              (for [_ (range (* (draw/p->sin p) 5))]
                (let [[cx cy] (draw/v+ a [(rand 30) (rand 30)])
                      size (+ 3 (rand-int 5))
                      rock-shape (draw/poly 0 0 size (+ 5 (rand-int 4)) (rand))
                      rock-color (-> (draw/rgb 215 190 140)
                                     (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                                     (draw/rgb-lerp (draw/rgb 0 0 0) (+ 0.3 (rand 0.25))))]
                  [{:z-order (+ cy (* 10 size))
                    :shape   (-> rock-shape (draw/center cx cy))
                    :style   (draw/fill-style rock-color)}
                   {:z-order (+ cy 1 (* 10 size))
                    :shape   (-> rock-shape (draw/scale 0.75) (draw/center (dec cx) (dec cy)))
                    :style   (draw/fill-style (draw/rgb-lerp rock-color Color/WHITE 0.2))}
                   {:z-order (+ cy 2 (* 10 size))
                    :shape   (-> rock-shape (draw/center cx cy))
                    :style   (draw/line-style 1 Color/BLACK)}])))))))

    #_(doseq [[[[xa ya] [xb yb]] reeds] (map vector points (cycle [0 0 0 100 1000 100 0 0 0]))
            _ (range reeds)
            :let [light (+ 25 (rand-int 230))
                  blue (int (* 0.5 light))
                  green (int light)
                  red (int (* 0.4 green))
                  scale (rand)
                  sign (if (zero? (rand-int 2)) 1 -1)

                  xc (+ xa (* (- ya yb) sign hex-radius 0.03 (+ 0.25 (rand 0.5))))
                  yc (+ ya (* (- xb xa) sign hex-radius 0.03 (+ 0.25 (rand 0.5))))

                  xd (+ xc (* (- xb xa) sign hex-radius 0.03 (+ 0.25 (rand 0.5))))
                  yd (+ yc (* (- yb ya) sign hex-radius 0.03 (+ 0.25 (rand 0.5))))

                  ;x-offset (* sign hex-radius (+ 0.5 (rand 0.25)))
                  ;y-offset (* sign hex-radius (+ 0.5 (rand 0.25)))
                  ]]

      (.setColor g (Color. red green blue (int (+ 50 (* 50 (- 1 scale))))))
      (.setStroke g (BasicStroke. 1))
      (.drawLine g
                 (+ xd)
                 (+ yd)
                 (+ xd)
                 (+ yd (- (rand 10)))))

    (.setClip g clip)))

(defn push-back [distance instructions]
  (map #(update % :z-order - distance) instructions))

(defn draw-board []
  (let [^Graphics2D g (.getGraphics image)]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g board-color)
    (.fillRect g 0 0 width height)

    ;(.setStroke g thick-stroke)
    ;(.setColor g outline-color)
    ;(.setColor g Color/BLACK)
    ;(.draw g (board-outline 1.01))
    ;(.setColor g (Color. 200 200 200 127))
    ;(.draw g (board-outline 1.2))

    (draw/shape g
      (draw/fill-style texture/mud)
      (Rectangle2D$Float. 0 0 width height)
      ;(board-outline 1.01)
      )

    (draw/shape g
      (draw/fill-style texture/grass-2)
      (Rectangle2D$Float. 0 0 width height)
      ;(board-outline 1.01)
      )

    ;(.setStroke g thin-stroke)
    ;(.setColor g outline-color)
    ;(.setColor g Color/BLACK)
    ;(.draw g (board-outline 1.02))

    (doseq [[ix iy] all-xys]
      (let [[x y] (xy-pos ix iy)
            terrain (get-in terrain-map [iy ix])]
        (when (#{:strt :tree :trmd} terrain)
          (render-shield-glow g x y))))

    (when-not *quick?*
      (draw-river g))

    #_(let [box (Rectangle2D$Float. 0 -10 width (util/mm->px 19.5))
            arch-size 0.3
            arch-width 1.2
            arch-top 0.95
            arches (for [ix (range 0 10)]
                     (let [[x y] (xy-pos ix 0)]
                       (draw/shape-add
                         (Ellipse2D$Float. (- x (* hex-radius arch-size arch-width))
                                           (- y (* hex-radius (+ arch-size arch-top)))
                                           (* hex-radius arch-size 2 arch-width)
                                           (* hex-radius arch-size 1.3))
                         (Rectangle2D$Float. (- x (* hex-radius arch-size arch-width))
                                             (+ (- y (* hex-radius (+ arch-size arch-top)))
                                                (* hex-radius arch-size 1.3 1/2))
                                             (* hex-radius arch-size 2 arch-width)
                                             (* hex-radius arch-size 1.3)))))
            wall-with-arches (reduce draw/shape-subtract box arches)]

        (draw/shape g
          (draw/line-style 14 Color/BLACK)
          wall-with-arches)

        (doseq [ix (range -1 10)]
          (let [[x y] (xy-pos ix -1)
                sides 10
                angle (- (/ TAU 2) (rand (/ TAU 14)))]
            (render-tower-outline g x (+ y (util/mm->px 2)) sides angle 1.0 2/3)))

        (draw/shape g
          (draw/shape-style outline-color 3 wall-color)
          wall-with-arches)

        (doseq [ix (range -1 10)]
          (let [[x y] (xy-pos ix -1)
                sides 10
                angle (- (/ TAU 2) (rand (/ TAU 14)))]
            (render-tower g x (+ y (util/mm->px 2)) sides angle 1.0 2/3))))

    ;(let [[x y] (xy-pos -1 1)]
    ;  (render g :wall x y))
    ;(let [[x y] (xy-pos 9 1)]
    ;  (render g :wall x y))


    ;TODO: re-enable this!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (doseq [[ix iy] all-xys]
      (let [[x y] (xy-pos ix iy)
            terrain (get-in terrain-map [iy ix])]
        (when (#{:spot :brga :brgb} terrain))
        (render g terrain x y)))

    #_(let [x-spread (util/mm->px 30)]
        (doseq [[x y] (map #(xy-pos % -1) (range -1 10 2))]
          ;(println x y)
          #_(stone/do-instructions g
                                   (stone/rings x (- y (util/mm->px 10)) [0 9] 30 160 0.8 11))
          (stone/do-instructions g
                                 (concat
                                   (stone/h-wall (- x x-spread) (+ y (util/mm->px 10)) (* 2 x-spread) 20 12 9 8)
                                   (stone/rings (+ x x-spread) (+ y (util/mm->px 10)) [0 9] 12 20 0.8 11)
                                   (stone/rings (- x x-spread) (+ y (util/mm->px 10)) [0 9] 12 20 0.8 11)))))

    (let [instructions
          (concat

            [
             {:shape   (draw/poly [(xy-pos 1 -1)
                                   (draw/v+ (xy-pos 2 1) [0 (util/mm->px -5)])
                                   (draw/v+ (xy-pos 6 1) [0 (util/mm->px -5)])
                                   (xy-pos 7 -1)])
              :style   (draw/fill-style texture/light-stone-slabs)
              :z-order -50000}
             {:shape   (draw/poly [(draw/v+ (xy-pos 2 1) [0 (util/mm->px -18)])
                                   (draw/v+ (xy-pos 2 1) [0 (util/mm->px -8)])
                                   (draw/v+ (xy-pos 6 1) [0 (util/mm->px -8)])
                                   (draw/v+ (xy-pos 6 1) [0 (util/mm->px -18)])])
              :style   (draw/fill-style texture/stone-slabs)
              :z-order 8500 #_1500}
             {:shape   (draw/poly [(draw/v+ (xy-pos 1 -1) [(util/mm->px 7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 1 -1) [(util/mm->px -7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 2 1) [(util/mm->px -7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 2 1) [(util/mm->px 7) (util/mm->px -10)])])
              :style   (draw/fill-style texture/stone-slabs)
              :z-order 6500}
             {:shape   (draw/poly [(draw/v+ (xy-pos 7 -1) [(util/mm->px 7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 7 -1) [(util/mm->px -7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 6 1) [(util/mm->px -7) (util/mm->px -10)])
                                   (draw/v+ (xy-pos 6 1) [(util/mm->px 7) (util/mm->px -10)])])
              :style   (draw/fill-style texture/stone-slabs)
              :z-order 6500}]

            #_(apply
                concat
                (for [[x y] [(xy-pos 2.7 1) (xy-pos 3.3 1) (xy-pos 4.7 1) (xy-pos 5.3 1)]]
                  (push-back 10000 (stone/rings x (+ y (util/mm->px 5)) [0 9] 12 20 0.8 7))))

            (push-back -7000
                       (apply
                         concat
                         (for [[x y] [(xy-pos 2 1) (xy-pos 4 1)]
                               y-offset [-6 -14]] ; top wall
                           (stone/h-wall x (+ y (util/mm->px y-offset)) (util/mm->px 115) 20 8 9 4))))

            #_(apply
                concat
                (for [[x y] [(xy-pos 2 1) (xy-pos 4 1)]] ; bottom wall
                  (push-back
                    10000
                    (concat
                      (stone/h-wall x (+ y (util/mm->px 6)) (util/mm->px 40) 20 12 9 7)
                      (stone/h-wall (+ x (util/mm->px 70)) (+ y (util/mm->px 6)) (util/mm->px 40) 20 12 9 7)))))

            #_(apply
                concat
                (for [[x y] [(xy-pos 2 -1) (xy-pos 6 -1)]
                      x-offset [(util/mm->px -7) (util/mm->px 7)]]
                  (stone/v-wall (+ x x-offset) y (util/mm->px 100) 20 12 9 7)
                  ))

            (push-back -6000
                       (apply
                         concat
                         (for [[[x1 y1] [x2 y2]] #_[[(xy-pos 1 -1) (xy-pos 2 0)]
                                                    [(xy-pos 2 0) (xy-pos 2 1)]
                                                    [(xy-pos 7 -1) (xy-pos 7 0)]
                                                    [(xy-pos 7 0) (xy-pos 6 1)]]
                               [[(xy-pos 1 -1) (xy-pos 2 1)]
                                [(xy-pos 7 -1) (xy-pos 6 1)]]
                               x-offset [(util/mm->px -7) (util/mm->px 7)]
                               y-offset [(util/mm->px -10)]]
                           (stone/d-wall (+ x1 x-offset) (+ y1 y-offset) (+ x2 x-offset) (+ y2 y-offset) 20 12 9 2/3 4))))

            #_(let [[x1 y] (xy-pos -2 -1)
                    [x2 _] (xy-pos 10 -1)]
                (stone/h-wall x1 (+ y (util/mm->px 12)) (- x2 x1) 20 12 9 8))
            #_(let [[x y] (xy-pos 1 2)]
                (stone/h-wall (- x 500) (+ y (util/mm->px 4)) 500 20 12 9 8))
            #_(let [[x y] (xy-pos 8 2)]
                (stone/h-wall x (+ y (util/mm->px 4)) 500 20 12 9 8))
            (let [[x1 y] (xy-pos 2 3)
                  [x2 _] (xy-pos 6 3)]
              (stone/h-wall x1 (+ y (util/mm->px 4)) (- x2 x1) 20 12 9 8))

            (let [[x y] (xy-pos 0 3)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 80 0.8 10))
            (let [[x1 y1] (xy-pos 0 3) [x2 y2] (xy-pos -1 0)]
              (stone/d-wall x1 y1 x2 y2 20 12 9 2/3 6))
            (let [[x y] (xy-pos 8 3)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 80 0.8 10))
            (let [[x1 y1] (xy-pos 8 3) [x2 y2] (xy-pos 10 0)]
              (stone/d-wall x1 y1 x2 y2 20 12 9 2/3 6))

            (let [[x y] (xy-pos 2 3)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 80 0.8 10))
            (let [[x y] (xy-pos 6 3)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 80 0.8 10))

            (let [[x y] (xy-pos 1 -1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 2 0)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 7 0)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 7 -1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))

            (let [[x y] (xy-pos 2 1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 3 1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 4 1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 5 1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))
            (let [[x y] (xy-pos 6 1)] (stone/rings x (+ y (util/mm->px 4)) [0 9] 30 60 0.8 13))

            )]
      (stone/do-instructions g instructions))

    (let [[x y] (xy-pos 0 3)] (symbol/flag g x (- y (util/mm->px 25) -9) 40 1.2))
    (let [[x y] (xy-pos 8 3)] (symbol/flag g x (- y (util/mm->px 25) -9) 40 1.2))
    (let [[x y] (xy-pos 2 3)] (symbol/flag g x (- y (util/mm->px 25) -9) 40 1.2))
    (let [[x y] (xy-pos 6 3)] (symbol/flag g x (- y (util/mm->px 25) -9) 40 1.2))

    (let [[x y] (xy-pos 2 1)] (symbol/flag g x (- y (util/mm->px 25) 18) 60 0.8))
    (let [[x y] (xy-pos 4 1)] (symbol/flag g x (- y (util/mm->px 25) 18) 60 0.8))
    (let [[x y] (xy-pos 6 1)] (symbol/flag g x (- y (util/mm->px 25) 18) 60 0.8))

    ;(stone/h-wall g (- x 500) y 500 20 12 9 3)

    #_(doseq [[ix iy] all-xys
              :let [[x y] (xy-pos ix iy)
                    terrain (get-in terrain-map [iy ix])]
              :when (#{:brga :brgb} terrain)]
        (render g terrain x y))


    ;(refresh-fn)
    (ImageIO/write ^RenderedImage image "png" ^File (io/file "./generated/to-print/board.png"))

    (println (str (Math/round (double width-mm)) " mm X " (Math/round (double height-mm)) " mm"))))

(defn draw-board-quickly []
  (binding [*quick?* true]
    (draw-board)))

(defn draw-board-properly []
  (binding [*quick?* false]
    (draw-board)))

(comment
  ; Standard Matte (Smooth Finish) Custom Size Game Board Gameboard
  ; Length: 350mm, Width: 267mm, (Bi-Fold)
  (draw-board-properly)

  )

#_(do ; test block

  (require '[see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  (def width 1000)
  (def height 1000)

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))

  (def g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  (println (vec (for [[x y seed] (for [x [100 300 500 700]
                                       y [100 300 500 700]]
                                   [(* x 10/8) (* y 10/8) (rand-int 1000)])]
                  (do
                    (util/set-random-seed seed)
                    (render g :midd x (+ y 20))
                    #_(if (> 0.5 (rand))
                        (render g :midd x (+ y 20))
                        (render g :trmd x (+ y 20)))
                    (draw/text g (draw/text-style 20 Color/BLACK :bold!) (str seed) (- x 75) (+ y 110))
                    seed))))

  (def acceptable-tree-seeds
    [369 196 977 594 823 219 345 136
     261 884 211 150 855 472 228 149
     781 82 584 970 89 10 771 326 735 124 365
     861 49 350 735 575 120 245 55 765 612
     624 596 766 797 313 365 957 626 344 117 996 189 496 220])

  (refresh-fn)

  )
