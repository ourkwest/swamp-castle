(ns domination.support.stone
  (:require
    [domination.support.util :as util]
    [domination.support.draw :as draw]
    [clojure.java.io :as io]
    [domination.support.texture :as texture])
  (:import
    [java.awt Graphics2D Color BasicStroke Rectangle TexturePaint]
    (java.awt.geom Ellipse2D Rectangle2D Rectangle2D$Float Ellipse2D$Float Area AffineTransform)
    (javax.imageio ImageIO)))


#_(defn style []
  (draw/shape-style Color/BLACK 1
                    (-> (draw/rgb 215 190 140)
                        (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                        (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.25)))))

(def style texture/stone-style)

(defn v+ [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn v- [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn v* [[x y] f]
  [(* x f) (* y f)])

(defn stone [^Graphics2D g x y angle z-vec scale]
  (let [x-vec [(* scale (Math/sin angle) 2) (* scale (Math/cos angle) 2)]
        y-vec [(* scale (Math/cos angle)) (* scale (Math/sin angle) -1)]
        a (-> [x y] (v+ x-vec) (v+ y-vec))
        b (-> [x y] (v- x-vec) (v+ y-vec)) ;[(- x x-vec) (+ y y-vec)]
        c (-> [x y] (v- x-vec) (v- y-vec)) ;[(- x x-vec) (- y y-vec)]
        d (-> [x y] (v+ x-vec) (v- y-vec)) ;[(+ x x-vec) (- y y-vec)]
        [a' b' c' d'] (map (partial #(map + %1 %2) z-vec) [a b c d])
        z-angle (Math/atan2 (second z-vec) (first z-vec))
        test? (fn [[x1 y1] [x2 y2]]
                ;(println (- z-angle (Math/atan2 (- x1 x2) (- y1 y2))))
                (< (* util/TAU -1/4)
                   (- z-angle (Math/atan2 (- x1 x2) (- y2 y1)))
                   (* util/TAU 1/4)))
        face-style (draw/shape-style Color/BLACK 1
                                     (-> (draw/rgb 215 190 140)
                                         (draw/rgb-lerp (draw/rgb 190 140 215) (rand 0.25))
                                         (draw/rgb-lerp (draw/rgb 0 0 0) (rand 0.25))))
        draw-face (fn [[n n'] [m m']]
                    ;(println (test? m n))
                    (when (test? m n)
                      (draw/shape g face-style (draw/poly [n m m' n']))))]

    #_(draw/shape g
                (draw/shape-style Color/BLACK 1 (draw/rgb 200 100 0))
                (draw/poly [a' b' c' d']))
    (draw/shape g
                face-style
                (draw/poly [a b c d]))

    (draw-face [a a'] [b b'])
    (draw-face [b b'] [c c'])
    (draw-face [c c'] [d d'])
    (draw-face [d d'] [a a']))
  )

(defn p [x y theta radius aspect-ratio]
  [(+ x (* radius (Math/sin theta)))
   (+ y (* radius (Math/cos theta) aspect-ratio))])

(defn intruct-poly [style z a b c d]
  {:z-order (+ z (apply max (map second [a b c d])))
   :shape   (draw/poly [a b c d])
   :style   style})

(defn do-instructions [g instructions]
  (doseq [{:keys [shape style] :as _i} (remove nil? (sort-by :z-order instructions))]
    ;(println i)
    (draw/shape g style shape)))

(defn arc [x y z-vec n radius inner-radius-proportion target-angle]
  (let [aspect-ratio 1
        arc-range (* util/TAU 0.088)
        oddness 1/5
        angle (* util/TAU (/ 1 n))
        angles (for [i (range n)]
                 (-> (* i angle)
                     (- (* angle oddness))
                     (+ (rand (* angle 2 oddness)))))
        sections (take n (partition 2 1 (cycle angles)))
        instrs (->> (for [[theta-1 theta-2] sections
                         :when (and (< (- arc-range) (- target-angle theta-1) arc-range)
                                    (< (- arc-range) (- target-angle theta-2) arc-range))]
                     (let [inner-radius (* radius inner-radius-proportion)
                           a (p x y theta-1 radius aspect-ratio)
                           b (p x y theta-2 radius aspect-ratio)
                           c (p x y theta-2 inner-radius aspect-ratio)
                           d (p x y theta-1 inner-radius aspect-ratio)
                           [a' b' c' d'] (map (partial #(map + %1 %2) z-vec) [a b c d])
                           s (style)
                           instruct-face (fn [[x1 :as p1] [x2 :as p2] p3 p4]
                                           (when (< x1 x2)
                                             (intruct-poly s 0 p1 p2 p3 p4)))]
                       [(intruct-poly s 500 a b c d) ; top
                        (instruct-face a b b' a') ; outer face
                        (instruct-face b c c' b')
                        (instruct-face c d d' c') ; inner face
                        (instruct-face d a a' d')]))
                    (apply concat)
                    (remove nil?))
        outline {:style (draw/line-style 7 Color/BLACK)
                 :shape (reduce draw/shape-add (map :shape instrs))
                 :z-order -1}]
    (cons outline instrs)))

(defn ring [g x y z-vec n radius inner-radius-proportion layer last? second-last?]
  (let [aspect-ratio 2/3
        oddness 1/5
        angle (* util/TAU (/ 1 n))
        angles (for [i (range n)]
                 (-> (* i angle)
                     (+ (* angle 1/2 layer))
                     (- (* angle oddness))
                     (+ (rand (* angle 2 oddness)))))
        sections (take n (partition 2 1 (cycle angles)))
        sections (if last?
                   (take-nth 2 sections)
                   sections)]
    (->> (for [[theta-1 theta-2] sections]
           (let [inner-radius (* radius inner-radius-proportion)
                 a (p x y theta-1 radius aspect-ratio)
                 b (p x y theta-2 radius aspect-ratio)
                 c (p x y theta-2 inner-radius aspect-ratio)
                 d (p x y theta-1 inner-radius aspect-ratio)
                 [a' b' c' d'] (map (partial #(map + %1 %2) z-vec) [a b c d])
                 s (style)
                 instruct-face (fn [[x1 :as p1] [x2 :as p2] p3 p4]
                                 (when (< x1 x2)
                                   (intruct-poly s (* layer 1000) p1 p2 p3 p4)))]
             [(when (or last? second-last?) (intruct-poly s (+ (* layer 1000) 500) a b c d)) ; top
              (instruct-face a b b' a') ; outer face
              (when last? (instruct-face b c c' b'))
              (instruct-face c d d' c') ; inner face
              (when last? (instruct-face d a a' d'))]))
         (apply concat)
         #_(do-instructions g))))

(defn rings [x y z-vec n radius inner-radius-proportion layers]
  (let [instrs (->> (for [layer (range layers)]
                      (let [[x y] (reduce v- [x y] (repeat layer z-vec))]
                        (ring g x y z-vec n radius inner-radius-proportion layer (= layer (dec layers)) (= layer (dec (dec layers))))))
                    (apply concat)
                    (remove nil?))
        floor (let [floor-depth 3
                    [x y] (reduce v- [x y] (repeat (- layers floor-depth) z-vec))]
                {:style   (draw/fill-style texture/stone-slabs)
                 :shape   (draw/ellipse (- x (* radius 0.95))
                                        (- y (* (* radius 0.95) 1/2))
                                        (* 2 (* radius 0.95))
                                        (* 1 (* radius 0.95)))
                 :z-order (* 1001 (- layers floor-depth))})
        outline {:style (draw/line-style 7 Color/BLACK)
                 :shape (reduce draw/shape-add (map :shape instrs))
                 :z-order -1}
        floor-outline {:style   (draw/line-style 5 Color/BLACK)
                       :shape   (reduce draw/shape-subtract
                                        (:shape floor)
                                        (map :shape (filter #(-> % :z-order (> (:z-order floor))) instrs)))
                       :z-order (inc (:z-order floor))}]
    (concat [outline floor floor-outline] instrs)))

(defn h-wall-layer [x y length x-size y-size z-size layer last?]
  (let [sections (->> (range x (+ x length) x-size)
                      (map #(+ % (rand (* x-size 1/3))))
                      (map #(if (odd? layer)
                              (+ % (* x-size 1/2))
                              %))
                      (partition 2 1))
        sections (if last?
                   (take-nth 2 sections)
                   sections)]
    (->> (for [[x1 x2] sections]
           (let [z (* layer z-size)
                 a [x1 (- y z y-size)]
                 b [x1 (- y z)]
                 c [x2 (- y z)]
                 d [x2 (- y z y-size)]
                 b' (v+ b [0 z-size])
                 c' (v+ c [0 z-size])
                 s (style)]
             [{:shape   (draw/poly [a b c d])
               :style   s
               :z-order (* layer 1000)}
              {:shape   (draw/poly [b b' c' c])
               :style   s
               :z-order (* layer 1000)}]))
         (apply concat))))

(defn outline [instructions]
  {:shape   (reduce draw/shape-add (map :shape instructions))
   :style   (draw/line-style 7 Color/BLACK)
   :z-order -1})

(defn h-wall [x y length x-size y-size z-size layers]
  (let [bricks (apply concat
                      (for [layer (range layers)]
                        (h-wall-layer x y length x-size y-size z-size layer (= layer (dec layers)))))]
    (cons (outline bricks) bricks)))

(defn v-wall [x y length x-size y-size z-size layers]
  (let [bricks (for [[y1 y2] (->> (range y (+ y length) y-size)
                                  (map #(+ % (rand (* y-size 1/3))))
                                  (partition 2 1))]
                 {:shape   (draw/poly [[(- x (* x-size 1/2)) (- y1 (* layers z-size))]
                                       [(+ x (* x-size 1/2)) (- y1 (* layers z-size))]
                                       [(+ x (* x-size 1/2)) (- y2 (* layers z-size))]
                                       [(- x (* x-size 1/2)) (- y2 (* layers z-size))]])
                  :style   (style)
                  :z-order (* layers 1000)})]
    (cons (outline bricks) bricks)))

(do ; test block

  (require '[domination.see.core :as see])
  (import '[java.awt.image BufferedImage]
          '[java.awt RenderingHints])

  (def width 400)
  (def height 400)

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))

  (def g (.getGraphics image))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width height)

  (stone g 40 40 (* util/TAU 1/8) [5 7] 20)
  (doseq [x (range 0 400 40)
          y (range 0 400 40)]
    (stone g x y (rand util/TAU) [5 7] 20))

  (doseq [theta (range 0 util/TAU (/ util/TAU 10))]

    (stone g
           (+ 200 (* -35 (Math/cos theta)))
           (+ 200 (* 35 (Math/sin theta)))
           theta
           [0 3]
           5
           ))

  (ring g 50 50 [0 5] 10 40 0.8 1 false false)
  (do-instructions g (rings 150 150 [0 5] 20 40 0.8 10))
  (do-instructions g (rings 250 250 [0 5] 30 60 0.85 10))

  (do-instructions g (h-wall 50 50 200 20 12 9 5))
  (do-instructions g (v-wall 50 250 200 20 12 9 5))

  (refresh-fn)

  )