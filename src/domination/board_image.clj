(ns domination.board-image
  (:require [clojure.java.io :as io]
            [domination.see.core :as see])
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color Polygon Graphics2D RenderingHints Rectangle BasicStroke]
           [java.awt.geom Area Rectangle2D AffineTransform]))


(def TAU (* 2 Math/PI))

(def millis-per-inch 25.4)
(def a4-width 210)
(def a4-height 297)

(defn rand-0 [n]
  (- (rand n) (/ n 2)))

(defn rand-bit [n variance]
  (+ n (rand-0 (* n variance))))

;(def width 2100)
;(def height 2970)
(def hex-radius 60)
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

(def all-xys
  (for [iy (range n-y)
        ix (range n-x)
        :when (or (even? iy)
                  (< ix (dec n-x)))]
    [ix iy]))


(def board-color (Color. 60 90 20))
(def ground-color (Color. 120 180 40))
(def outline-color (Color. 200 200 200))

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

(defn render-arrow [^Graphics2D g x y]
  (.setColor g (Color. 50 255 120 30))
  (let [y-offset (- (/ hex-radius 1.1))]
    (doseq [i (range 0 (/ hex-radius 3) (/ hex-radius 100))]
      (.fill g (polygon x (- y y-offset (/ hex-radius 2) i) (- (/ hex-radius 3) i) 3 (/ TAU 6)))
      (.fill g (Rectangle. (+ (- x (/ hex-radius 6)) i)
                           (- y y-offset (/ hex-radius 3) (* i 1.5))
                           (* 2 (- (/ hex-radius 6) i))
                           hex-radius)))))

(defn render-tower-outline [^Graphics2D g x y sides angle]
  (let [top (- y (* hex-radius 0.4))
        bottom (+ y (* hex-radius 0.4))
        clip (.getClip g)]

    (.setClip g (polygon x y (* 1 hex-radius) 6 0))

    (.setColor g outline-color)
    (doseq [i (range bottom top -0.5)]
      (.draw g (polygon x i (* 0.6 hex-radius) sides angle 0.66)))

    (.setClip g clip)))

(defn render-tower [^Graphics2D g x y sides angle]
  (let [top (- y (* hex-radius 0.4))
        middle (- y (* hex-radius 0.2))
        bottom (+ y (* hex-radius 0.4))
        clip (.getClip g)]

    (.setClip g (polygon x y (* 1 hex-radius) 6 0))

    (.setColor g (Color. 100 100 100))
    (doseq [i (range bottom top -1)]
      (.fill g (polygon x i (* 0.55 hex-radius) sides angle 0.66)))

    (.setColor g outline-color)
    (.setStroke g thin-stroke)
    (.draw g (polygon x top (* 0.55 hex-radius) sides angle 0.66))

    (.draw g (polygon x top (* 0.45 hex-radius) sides angle 0.66))
    (.setClip g (polygon x top (* 0.45 hex-radius) sides angle 0.66))
    (.setColor g (Color. 75 75 75))
    (.fill g (polygon x middle (* 0.45 hex-radius) sides angle 0.66))
    (.setColor g outline-color)
    (.draw g (polygon x middle (* 0.45 hex-radius) sides angle 0.66))
    (.setClip g clip)))

(defn render-wall-outline [^Graphics2D g x y left? right?]
  (let [clip (.getClip g)
        hex-area (polygon x y (* 1 hex-radius) 6 0)
        left-side (Rectangle. (- x hex-radius) (- y hex-radius) hex-radius (* 2 hex-radius))
        right-side (Rectangle. x (- y hex-radius) hex-radius (* 2 hex-radius))
        temp-clip (cond-> hex-area
                          (not left?) (shape-subtract left-side)
                          (not right?) (shape-subtract right-side))]
    (.setClip g temp-clip)
    (.setColor g outline-color)
    (.fill g (Rectangle. (- x hex-radius)
                         (- y (* hex-radius 0.3))
                         (* 2 hex-radius)
                         (* hex-radius 0.7)))
    (.setClip g clip)))

(defn render-wall [^Graphics2D g x y left? right?]
  (let [clip (.getClip g)
        hex-area (polygon x y (* 1 hex-radius) 6 0)
        left-side (Rectangle. (- x hex-radius) (- y hex-radius) hex-radius (* 2 hex-radius))
        right-side (Rectangle. x (- y hex-radius) hex-radius (* 2 hex-radius))
        temp-clip (cond-> hex-area
                          (not left?) (shape-subtract left-side)
                          (not right?) (shape-subtract right-side)
                          )]
    (.setClip g temp-clip)
    (.setStroke g thin-stroke)
    (.setColor g (Color. 100 100 100))
    (.fill g (Rectangle. (- x hex-radius)
                         (- y (* hex-radius 0.25))
                         (* 2 hex-radius)
                         (* hex-radius 0.6)))
    (.setColor g (Color. 75 75 75))
    (.fill g (Rectangle. (- x hex-radius)
                         (- y (* hex-radius 0.1))
                         (* 2 hex-radius)
                         (* hex-radius 0.15)))
    (.setColor g outline-color)
    (.drawLine g (- x hex-radius) (- y (* hex-radius 0.2)) (+ x hex-radius) (- y (* hex-radius 0.2)))
    (.drawLine g (- x hex-radius) (- y (* hex-radius 0.1)) (+ x hex-radius) (- y (* hex-radius 0.1)))
    (.drawLine g (- x hex-radius) (+ y (* hex-radius 0.05)) (+ x hex-radius) (+ y (* hex-radius 0.05)))
    (.drawLine g (- x hex-radius) (+ y (* hex-radius 0.1)) (+ x hex-radius) (+ y (* hex-radius 0.1)))
    (.setClip g clip)))

(def terrain-map
  [[:cake :cake :cake :cake :cake :cake :cake :cake :cake :cake]
      [:wall :twrr :spot :spot :spot :tree :spot :twrl :wall]
   [:spot :spot :spot :spot :spot :spot :spot :spot :spot :spot]
      [:tree :spot :twrl :wall :wall :wall :twrr :spot :spot]
   [:spot :spot :spot :spot :spot :spot :spot :spot :tree :spot]
      [:tree :spot :spot :midd :trmd :midd :spot :tree :tree]
   [:tree :tree :spot :midd :midd :midd :midd :spot :tree :spot]
      [:rivr :tree :spot :midd :midd :midd :spot :spot :spot]
   [:spot :brga :spot :spot :spot :spot :spot :spot :spot :spot]
      [:spot :rivr :rivr :spot :tree :tree :spot :spot :spot]
   [:spot :spot :tree :rivr :brgb :rivr :rivr :spot :spot :tree]
      [:spot :tree :tree :spot :spot :tree :brga :spot :tree]
   [:spot :spot :tree :spot :spot :spot :spot :rivr :brgb :rivr]
      [:spot :spot :tree :spot :spot :spot :spot :spot :tree]
   [:strt :strt :strt :strt :strt :strt :strt :strt :strt :strt]])

(defmulti render (fn [_g terrain _x _y] terrain))
(defmethod render :default [_ _ _ _] nil)

(defmethod render :spot [^Graphics2D g _terrain x y]
  (.setColor g ground-color)
  (.fill g (polygon x y (* 0.95 hex-radius) 6 0)))

(defn render-rubbish [g x y]
  (let [t (.getTransform g)]
    (doseq [light (range 0 256 0.3)]
      (let [angle (rand-0 (/ TAU 4))
            dist (rand (* 0.8 hex-radius))
            x-size (rand (* hex-radius 0.25))
            y-size (rand (* hex-radius 0.25))
            x-midd (+ x (* dist (Math/sin angle)))
            y-midd (+ y (* dist (Math/cos angle)) (- (* 0.32 hex-radius)))
            x-start (- x-midd (/ x-size 2))
            y-start (- y-midd (/ y-size 2))]
        (.setColor g (Color. (rand-int light) (rand-int light) (rand-int light)))
        (.setTransform g (doto (AffineTransform.)
                           (.translate x-midd y-midd)
                           (.rotate (rand TAU))
                           (.translate (- x-midd) (- y-midd))
                           ))
        (.fillArc g x-start y-start x-size y-size 0 360)))
    (.setTransform g t)))

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
          transform-branch (fn [[render-type start end scale :as arg]]
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
  (.setColor g (Color. 100 20 0))
  (doseq [n (range 0 (* hex-radius 0.1) 0.1)]
    (.fill g (polygon x (+ y n n (- (* hex-radius 0.10))) n 10 0 0.5)))
  (doseq [[render-type :as branch] (sort-by (fn [[_ _ [_ y z]]] (- z y)) branches)
          :when (= render-type :branch)]
    (let [[_ [x1 y1 z1] [x2 y2 z2] scale] branch
          gx1 (+ x x1)
          gy1 (- y (* y1 0.5) (* z1 0.4))
          gx2 (+ x x2)
          gy2 (- y (* y2 0.5) (* z2 0.4))
          offset (* 2 scale)]
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
      (.setColor g (Color. 0 (int (+ 100 (rand 155))) 0 30))
      (.fill g (polygon gx1 gy1 (* hex-radius 0.05) 10 0)))))

(defmethod render :trmd [^Graphics2D g _terrain x y]
  (let [branches (create-branches 1.0 identity)]
    (render g :spot x y)
    (render-branches g x (+ y (* hex-radius 0.4)) branches)
    (render-token g x y board-color ground-color)
    (render-rubbish g x y)
    (render-leaves g x (+ y (* hex-radius 0.4)) branches)))

(defmethod render :tree [^Graphics2D g _terrain x y]
  (let [branches (create-branches 1.0 identity)]
    (render g :spot x y)
    (render-branches g x (+ y (* hex-radius 0.4)) branches)
    (render-leaves g x (+ y (* hex-radius 0.4)) branches)))

(defmethod render :cake [^Graphics2D g _terrain x y]
  (render g :spot x y)
  (render-arrow g x (- y (* 1.25 hex-radius)))
  (render-token g x y board-color ground-color)
  (doseq [_ (range 30)]
    (let [angle (rand TAU)
          dist (rand (* hex-radius 0.45))
          size (rand (* hex-radius 0.04))
          cx (+ x (* dist (Math/sin angle)))
          cy (+ y (* dist 0.5 (Math/cos angle)) (* 0.3 hex-radius))]
      (.setColor g (Color. 180 100 110))
      (.fill g (polygon (- cx (* hex-radius 0.01)) (- cy (* hex-radius 0.01)) size (rand-nth [3 5 6 7]) (rand TAU)))
      (.setColor g (Color. 130 50 60))
      (.fill g (polygon cx cy size (rand-nth [3 5 6 7]) (rand TAU)))))
  (let [scale 1.4
        cake (ImageIO/read (io/file "./resources/public/images/cake.png"))
        cake-w (.getWidth cake)
        cake-h (.getHeight cake)
        dx1 (- x (* scale (/ hex-radius 2)))
        dy1 (- y (* scale (/ hex-radius 2.5)))
        dx2 (+ dx1 (* scale hex-radius 1.8 0.6))
        dy2 (+ dy1 (* scale hex-radius 1.8 0.45))]
    (.drawImage g cake
                dx1 dy1 dx2 dy2
                0 0 cake-w cake-h
                nil)))

(defmethod render :strt [^Graphics2D g _terrain x y]
  (render g :spot x y)
  (.setColor g board-color)
  (.fill g (polygon x y (* 0.7 hex-radius) 50 0))
  (render-arrow g x y))

(defmethod render :midd [^Graphics2D g _terrain x y]
  (render g :spot x y)
  (render-token g x y board-color ground-color)
  (render-rubbish g x y))

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

    (.setColor g outline-color)
    (.setStroke g (BasicStroke. (/ hex-radius 5)))
    (.draw g (adj-circle x y -0.5 -1 1.15))
    (.draw g (adj-circle x y 0.5 1 1.15))

    (.setColor g (Color. 100 100 100))
    (.setStroke g (BasicStroke. (/ hex-radius 8)))
    (.draw g (adj-circle x y -0.5 -1 1.15))
    (.draw g (adj-circle x y 0.5 1 1.15))

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

    (.setColor g outline-color)
    (.setStroke g (BasicStroke. (/ hex-radius 5)))
    (.draw g (adj-circle x y -1 0 1.15))
    (.draw g (adj-circle x y 1 0 1.15))

    (.setColor g (Color. 100 100 100))
    (.setStroke g (BasicStroke. (/ hex-radius 8)))
    (.draw g (adj-circle x y -1 0 1.15))
    (.draw g (adj-circle x y 1 0 1.15))

    (.setClip g clip)))

(defmethod render :rivr [^Graphics2D g _terrain x y]
  ; River rendered separately.
  )

(defmethod render :wall [^Graphics2D g _terrain x y]
  (render-wall-outline g x y true true)
  (render-wall g x y true true))

(defmethod render :twrr [^Graphics2D g _terrain x y]
  (let [sides 7
        angle (- (/ TAU 2) (rand (/ TAU 14)))]
    (render-tower-outline g x y sides angle)
    (render-wall-outline g x y true false)
    (render-wall g x y true false)
    (render-tower g x y sides angle)))

(defmethod render :twrl [^Graphics2D g _terrain x y]
  (let [sides 7
        angle (+ (/ TAU 2) (rand (/ TAU 14)))]
    (render-tower-outline g x y sides angle)
    (render-wall-outline g x y false true)
    (render-wall g x y false true)
    (render-tower g x y sides angle)))

(defn board-outline [margin]
  (->> (for [[ix iy] all-xys]
         (polygon (x-pos ix iy) (y-pos iy) (* margin hex-radius) 6 0))
       (reduce shape-add)))

(def river-xys
  (concat [[-1 7]]
          (for [[ix iy] all-xys
                :let [terrain (get-in terrain-map [iy ix])]
                :when (#{:brga :brgb :rivr} terrain)]
            [ix iy])
          [[10 12]]))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce refresh-fn (see/see image :only-draw-when-updated? true))

(defn scale [a b prop]
  (+ (* prop b) (* (- 1 prop) a)))

(defn draw-line [g xa ya xb yb colour width]
  (.setColor g colour)
  (.setStroke g (BasicStroke. width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  (.drawLine g xa ya xb yb))

(defn draw-river [g]
  (let [points (->> (for [[[x1 y1] [x2 y2] [x3 y3]] (partition 3 1 river-xys)]
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
                    (partition 2 1))
        clip (.getClip g)]
    (.setClip g (board-outline 1.0))

    (doseq [[[xa ya] [xb yb]] points
            f (range 0.75 1.1 0.05)]
      (draw-line g xa ya xb yb (Color. 50 40 10 30) (/ hex-radius f)))
    (doseq [[[xa ya] [xb yb]] points]
      (draw-line g xa ya xb yb (Color. 100 100 250 127) (/ hex-radius 1.1)))
    ;(doseq [[[xa ya] [xb yb]] points]
    ;  (draw-line g xa ya xb yb (Color. 80 80 200 127) (/ hex-radius 1.1)))
    ;(doseq [[[xa ya] [xb yb]] points]
    ;  (draw-line g xa ya xb yb (Color. 60 60 150) (/ hex-radius 1.4)))

    (doseq [[[xa ya] [xb yb]] points
            _ (range 1500)
            :let [light (+ 50 (rand-int 200))
                  blue light
                  green (int (* 0.4 light))
                  red (int (* 0.4 green))
                  scale (rand)
                  x-offset (* scale (rand-0 (* hex-radius 0.75)))
                  y-offset (* scale (rand-0 (* hex-radius 0.75)))]]

      (.setColor g (Color. red green blue (int (+ 50 (* 50 (- 1 scale))))))
      (.setStroke g (BasicStroke. 1))
      (.drawLine g
                 (+ x-offset xa)
                 (+ y-offset ya)
                 (+ x-offset xb)
                 (+ y-offset yb)))
    (.setClip g clip)))

(defn draw-board []
  (let [^Graphics2D g (.getGraphics image)]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g board-color)
    (.fillRect g 0 0 width height)

    (.setStroke g thick-stroke)
    (.setColor g outline-color)
    (.draw g (board-outline 1.01))
    (.setColor g (Color. 200 200 200 127))
    (.draw g (board-outline 1.2))

    (.setColor g board-color)
    (.fill g (board-outline 1.01))

    (.setStroke g thin-stroke)
    (.setColor g outline-color)
    (.draw g (board-outline 1.02))

    (draw-river g)

    (doseq [[ix iy] all-xys]
      (let [[x y] (xy-pos ix iy)
            terrain (get-in terrain-map [iy ix])]
        (render g terrain x y)))


    #_(doseq [[ix iy] all-xys
            :let [[x y] (xy-pos ix iy)
                  terrain (get-in terrain-map [iy ix])]
            :when (#{:brga :brgb} terrain)]
      (render g terrain x y))


    (refresh-fn)
    (ImageIO/write image "png" (io/file "board.png"))

    (println (str (Math/round width-mm) " mm X " (Math/round height-mm) " mm"))))

(draw-board)