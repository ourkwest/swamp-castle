(ns domination.card
  (:require [clojure.string :as string]))




(defn hex-points
  ([r] (hex-points r 0 0))
  ([r x-off y-off]
   (string/join " " (for [theta (range 0 (* 2 Math/PI) (/ Math/PI 3))]
                      (str (+ (* r (Math/sin theta)) x-off)
                           ","
                           (+ (* r (Math/cos theta)) y-off))))))

(defn tri-points
  ([r] (tri-points r 0 0))
  ([r x-off y-off]
   (string/join " " (for [theta (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) 3))]
                      (str (+ x-off (* r (Math/sin theta)))
                           ","
                           (- y-off (* r (Math/cos theta))))))))


(def width 210)
(def height 297)
(def hex-radius 10)
(def x-step (* (/ (Math/sqrt 3) 2) 2 hex-radius))
(def y-step (* (/ 3 2) hex-radius))

(def n-x 12)
(def n-y 17)
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

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))


(defn randomise [[x y] r]
  [(+ x (rand r) (- (/ r 2))) (+ y (rand r) (- (/ r 2)))])

(defn bezier-path [init-char [x1 y1] [x2 y2] [x3 y3] [x4 y4]]
  (str init-char (int x1) \, (int y1)
       " C" (int x2) \, (int y2)
       " " (int x3) \, (int y3)
       " " (int x4) \, (int y4)))

(defn vect [[xa ya] [xb yb]]
  [(- xb xa) (- yb ya)])

(defn scale [[x y] scale]
  [(* x scale) (* y scale)])

(defn plus [[x y] [xx yy]]
  [(+ x xx) (+ y yy)])

(defn interpolate [a b proportion]
  (plus a (scale (vect a b) proportion)))



(defn dot [xx yy price [r g b]]

  (let [x (+ (* xx 24) 15)
        y (+ (* yy 23) 24)
        qq (if (and (= xx 0) (= yy 11)) 6 0)]
    [:g

     [:line {:x1 x
             :y1 y
             :x2 (+ x 16 qq)
             :y2 (- y 16 qq)
             :stroke "black"}]

     ;[:line {:x1 (+ x 10)
     ;        :y1 (- y 10)
     ;        :x2 (+ x 20)
     ;        :y2 (- y 10)
     ;        :stroke "black"}]

     ;[:path {:d      (str "M" x \, y " l10,-10 l10,0")
     ;        :fill   :none
     ;        :stroke :black
     ;        :style  {:stroke-linecap  :round
     ;                 :stroke-linejoin :round}}]

     [:circle {:cx           (+ x 16 qq)
               :cy           (- y 16 qq)
               :r            5
               :stroke       :black
               :stroke-width 0.5
               :fill :white}]

     [:circle {:cx           x
               :cy           y
               :r            11
               :stroke       :black
               :stroke-width 0.5
               :fill         (rgb [r g b])}]

     [:circle {:cx           x
               :cy           y
               :r            11
               :stroke       :black
               :stroke-width 0.5
               :fill         "url(#grad)"}]

     [:text {:x           (+ x 16 qq)
             :y           (- y 13.7 qq)
             :text-anchor "middle"
             :style       {:font-size "6px"}}
      price]])
  )


(defn render-card []

  [:h1 "Card?"]

  [:svg {:id "section-to-print"
         :style    {
                    :width  "100%"
                    ;:height "100%"
                    ;:border "1px solid black"
                    }
         :view-box (string/join " " [0 0 width height])}

   [:defs

    [:radialGradient {:id "grad" :fx 0.5 :fy 0.5 :r 1}
     [:stop {:stop-color "white" :stop-opacity 1 :offset "0%"}]
     [:stop {:stop-color "white" :stop-opacity 1 :offset "40%"}]
     [:stop {:stop-color "white" :stop-opacity 0 :offset "50%"}]
     [:stop {:stop-color "white" :stop-opacity 0 :offset "100%"}]]

    [:filter {:id "Blur2"
              }
     [:feGaussianBlur {:in "SourceGraphic" :stdDeviation "1"}]]

    ]


   (let [shield-prices [1 1 1 2 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21]
         prices-a (take 12 shield-prices)
         prices-b (take 7 (drop 12 shield-prices))]
     [:g
      (for [[idx price] (map-indexed vector prices-a)]
        (dot 0 idx price [50 100 255]))
      (for [[idx price] (map-indexed vector prices-b)]
        (dot (inc idx) 11 price [50 100 255]))])

   [:g
    (for [[idx price] (map-indexed vector [1 2 4 8 16])]
      [:g
       (dot (+ 2 idx) 2 price [255 0 0])
       (dot (+ 2 idx) 4 price [0 255 0])
       (dot (+ 2 idx) 6 price [0 0 255])
       (dot (+ 2 idx) 8 price [255 255 0])
       ])]

   (for [y (range n-y)
         x (range (if (even? y) n-x (dec n-x)))]
     [:use {:key       (str x "-" y)
            :x         (x-pos x y)
            :y         (y-pos y)
            :xlinkHref "#Hex"}])

   (for [y (range n-y)
         x (range (if (even? y) n-x (dec n-x)))]
     [:use {:key       (str x "-" y)
            :x         (x-pos x y)
            :y         (y-pos y)
            :xlinkHref "#Hex2"}])


   ])
