(ns domination.board
  (:require [clojure.string :as string]))




(defn hex-points
  ([r] (hex-points r 0 0))
  ([r x-off y-off]
   (string/join " " (for [theta (range 0 (* 2 Math/PI) (/ Math/PI 3))]
                      (str (+ (* r (Math/sin theta)) x-off)
                           ","
                           (+ (* r (Math/cos theta)) y-off))))))


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

(defn wall [{:keys [x y theta length]}]
  (let [x-loc (x-pos x y)
        y-loc (y-pos y)]
    [:g {:id        (str "Wall-" x "-" y)
         :transform (str "rotate(" theta "," x-loc "," y-loc ")")
         }
     [:rect {:style  {:fill         (rgb [150 125 100])
                      :stroke       (rgb [100 100 100])
                      :stroke-width 1}
             :x      (- x-loc (* x-step length))
             :y      (- y-loc (/ hex-radius 2.4))
             :width  (* x-step length 2)
             :height (/ hex-radius 1.2)}]
     [:rect {:style  {:fill         "none"
                      :stroke       (rgb [150 150 150])
                      :stroke-width 0.6
                      :stroke-dasharray "2,0.2"}
             :x      (- x-loc (* x-step length))
             :y      (- y-loc (/ hex-radius 2.4))
             :width  (* x-step length 2)
             :height (/ hex-radius 1.2)}]
     [:polygon {:style  {                                   :fill         (rgb [100 75 50])}
                :points (hex-points (* 0.8 hex-radius) (+ x-loc (* x-step length)) y-loc)}]

     [:polygon {:style  {                                   :fill         "none"
                         :stroke       (rgb [100 100 100])
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius) (+ x-loc (* x-step length)) y-loc)}]
     [:polygon {:style  {                                   :fill         "none"
                         :stroke       (rgb [150 150 150])
                         :stroke-width 0.6
                         :stroke-dasharray "2,0.2"}
                :points (hex-points (* 0.8 hex-radius) (+ x-loc (* x-step length)) y-loc)}]

     [:polygon {:style  {                                   :fill         (rgb [100 75 50])}
                :points (hex-points (* 0.8 hex-radius) (- x-loc (* x-step length)) y-loc)}]
     [:polygon {:style  {                                   :fill         "none"
                         :stroke       (rgb [100 100 100])
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius) (- x-loc (* x-step length)) y-loc)}]
     [:polygon {:style  {                                   :fill         "none"
                         :stroke       (rgb [150 150 150])
                         :stroke-width 0.6
                         :stroke-dasharray "2,0.2"}
                :points (hex-points (* 0.8 hex-radius) (- x-loc (* x-step length)) y-loc)}]]))

(defn randomise [[x y] r]
  [(+ x (rand r) (- (/ r 2))) (+ y (rand r) (- (/ r 2)))])

(defn bezier-path [[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
  (str "M" (int x1) \, (int y1) " C" (int x2) \, (int y2)
       " " (int x3) \, (int y3) " " (int x4) \, (int y4)))

(defn vect [[xa ya] [xb yb]]
  [(- xb xa) (- yb ya)])

(defn scale [[x y] scale]
  [(* x scale) (* y scale)])

(defn plus [[x y] [xx yy]]
  [(+ x xx) (+ y yy)])

(defn interpolate [a b proportion]
  (plus a (scale (vect a b) proportion)))

(defn river [& points]

  (for [q (range 1 0.2 -0.2)]
    (for [[[x-a y-a] [x-b y-b] [x-c y-c] [x-d y-d]] (partition 4 1 points)]

      (let [loc-a (xy-pos x-a y-a)
            loc-b (xy-pos x-b y-b)
            loc-c (xy-pos x-c y-c)
            loc-d (xy-pos x-d y-d)

            loc-aa loc-a
            loc-bb loc-b                                    ;(plus loc-b (scale (plus (vect loc-a loc-b) (vect loc-c loc-b)) (/ (- 1.0 q) -10.0)))
            loc-cc loc-c                                    ;(plus loc-c (scale (plus (vect loc-d loc-c) (vect loc-b loc-c)) (/ (- 1.0 q) -10.0)))
            loc-dd loc-d

            p0 loc-bb
            p1 (plus loc-bb (scale (vect loc-aa loc-cc) 0.2))
            p2 (plus loc-cc (scale (vect loc-dd loc-bb) 0.2))
            p3 loc-cc

            ;loc-e (interpolate loc-a loc-b 0.5)
            ;loc-f (interpolate loc-b loc-c 0.5)
            ;loc-g (interpolate loc-d loc-e 0.5)
            ;
            ;loc-h (interpolate loc-d loc-e 0.5)
            ;loc-i (interpolate loc-d loc-e 0.5)

            ]

        [:g {:key (str "river-" x-a "-" y-a "-" q)}

         [:path {:stroke         (rgb (map #(int (* (+ 0.5 (/ q 2)) %)) [100 150 250]))
                 :stroke-width   (* x-step (/ 2 5) q)
                 :fill           :none
                 :stroke-linecap "round"
                 ;:filter "url(#Blur)"
                 :d              (bezier-path p0 p1 p2 p3)}]

         ;(for [n (range 0 1 0.1)]
         ;
         ;  (let [
         ;        loc-d (interpolate loc-a loc-b n)
         ;        loc-e (interpolate loc-b loc-c n)
         ;
         ;        [x1 y1] (randomise loc-d (/ x-step 3))
         ;        [x2 y2] (randomise (interpolate loc-d loc-e 0.25) (/ x-step 2))
         ;        [x3 y3] (randomise (interpolate loc-d loc-e 0.75) (/ x-step 2))
         ;        [x4 y4] (randomise loc-e (/ x-step 3))]
         ;
         ;    [:path {:key          (str "river-line-" n)
         ;            :stroke       (rgb [(int (rand 100))
         ;                                (+ 50 (int (rand 100)))
         ;                                (+ 150 (int (rand 100)))])
         ;            :stroke-width (rand)
         ;            :fill         :none
         ;            :d            (str "M" (int x1) \, (int y1) " C" (int x2) \, (int y2)
         ;                               " " (int x3) \, (int y3) " " (int x4) \, (int y4))}]))
         ]
        )
      ))

  )

(defn midden [x y]

(let [[xp yp] (xy-pos x y)]

  [:g
   [:circle {:cx    xp
             :cy    yp
             :r     (* x-step 1.1)
             :style {:fill (rgb [150 100 50])}
             :filter "url(#Blur)"}]

   (for [theta (range 1000)]

     (let [n (rand)]
       [:circle {:cx    (+ xp (* (Math/sin theta) n x-step 1.1))
                 :cy    (+ yp (* (Math/cos theta) n x-step 1.1))
                 :r     (+ (* (rand) (- 1 n)) 0.2)
                 :style {:fill (rgb [(+ 50 (rand-int 100)) (+ 25 (rand-int 100)) (rand-int 100)])}}])

     )

   (let [[xp yp] (xy-pos x y)]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos (inc x) y)]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos (dec x) y)]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos x (dec y))]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos x (dec y))]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos x (inc y))]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])
   (let [[xp yp] (xy-pos x (inc y))]
     [:use {:x         xp
            :y         yp
            :xlinkHref "#Hex2"}])]))

(defn branch [x y theta size q limit]

  (let [x2 (+ x (* size (Math/sin theta) q))
        y2 (+ y (* size (Math/cos theta) q))]
    [:g
     [:line {:x1           x
             :y1           y
             :x2           x2
             :y2           y2
             :stroke       (rgb [150 100 50])
             :stroke-width (/ size 10)}]

     (when (pos? limit)
       [:g
        (branch x2 y2 (+ theta (+ 0.1 (rand 0.9))) (* size 0.7) q (dec limit))
        (branch x2 y2 (- theta (+ 0.1 (rand 0.9))) (* size 0.7) q (dec limit))])
     ])
  )

(defn tree [x y]
  (let [[xp yp] (xy-pos x y)]
    [:g
     ;[:circle {:cx xp :cy yp :r (/ x-step 25) :fill (rgb [150 100 50])}]
     (for [n (range 5)]
        (branch xp yp (+ n (rand)) (/ x-step 5) (+ 0.75 (rand 0.25)) 3))

     (for [theta (range 1000)]

       (let [n (rand)]
         [:circle {:cx    (+ xp (* (Math/sin theta) n x-step 0.5))
                   :cy    (+ yp (* (Math/cos theta) n x-step 0.5))
                   :r     (+ (/ (rand) 2) 0.2)
                   :style {:fill "rgba(25,175,75,0.5)"}}]))]))

(defn bridge [x y theta]
  (let [[xp yp] (xy-pos x y)]
    [:g
     ;{:transform (str "rotate(" theta "," xp "," yp ")")}

     [:line {:x1           (+ xp (* (Math/sin theta) x-step 0.4))
             :y1           (+ yp (* (Math/cos theta) x-step 0.4))
             :x2           (+ xp (* (Math/sin theta) x-step -0.4))
             :y2           (+ yp (* (Math/cos theta) x-step -0.4))
             :stroke-width (/ x-step 2)
             :stroke       (rgb [100 100 100])}]
     [:line {:x1           (+ xp (* (Math/sin theta) x-step 0.4))
             :y1           (+ yp (* (Math/cos theta) x-step 0.4))
             :x2           (+ xp (* (Math/sin theta) x-step -0.4))
             :y2           (+ yp (* (Math/cos theta) x-step -0.4))
             :stroke-width (/ x-step 3)
             :stroke       "white"
             ;:filter "url(#Blur2)"
             }]


     ]))

(defn render-board []

  [:h1 "BOARD?"]

  [:svg {:style    {
                    :width  "30%"
                    ;:height "100%"
                    :border "1px solid black"}
         :view-box (string/join " " [0 0 width height])}

   [:defs

    [:radialGradient {:id "grad" :fx 0.5 :fy 0.5 :r 1}
     [:stop {:stop-color "white" :stop-opacity 1 :offset "0%"}]
     [:stop {:stop-color "white" :stop-opacity 1 :offset "40%"}]
     [:stop {:stop-color "green" :stop-opacity 1 :offset "100%"}]]

    [:radialGradient {:id "grad2" :fx 0.5 :fy 0.5 :r 1}
     [:stop {:stop-color (rgb [150 100 50]) :stop-opacity 1 :offset "0%"}]
     [:stop {:stop-color (rgb [150 100 50]) :stop-opacity 1 :offset "50%"}]
     [:stop {:stop-color "white" :stop-opacity 1 :offset "60%"}]
     [:stop {:stop-color "white" :stop-opacity 1 :offset "100%"}]]

    [:g {:id "Hex"}
           [:polygon {
                      :style {:fill "url(#grad)"
                              :stroke "black"
                              :stroke-width 0.1}
                      :points (hex-points hex-radius)} ]]

    [:g {:id "Hex2"}
     [:polygon {
                :style {:fill "none"
                        :stroke "black"
                        :stroke-width 0.1}
                :points (hex-points hex-radius)} ]]

    [:g {:id "Wall"}
     [:polygon {:x x-step
                :style  {:fill         "white"
                         :stroke       "grey"
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius))} ]
     [:polygon {:x      (- x-step)
                :style  {:fill         "white"
                         :stroke       "grey"
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius))} ]]

    ;[:filter {:id "Water"
    ;          ;:filterUnits "userSpaceOnUse"
    ;          :x "0%"
    ;          :y "0%"
    ;          :width "100%"
    ;          :height "100%"
    ;          }
    ; [:feTurbulence {:baseFrequency "0.05" :numOctaves "8"}]]
    ;
    [:filter {:id "Blur"
              }
     [:feGaussianBlur {:in "SourceGraphic" :stdDeviation "2"}]]

    ]


   (for [y (range n-y)
         x (range (if (even? y) n-x (dec n-x)))]
     [:use {:key       (str x "-" y)
            :x         (x-pos x y)
            :y         (y-pos y)
            :xlinkHref "#Hex"}])

   (midden 5 13)

   (wall {:x 5 :y 5 :length 2 :theta 0})
   (wall {:x 3 :y 4 :length 1 :theta 60})
   (wall {:x 8 :y 4 :length 1 :theta -60})
   (wall {:x 1 :y 1 :length 1 :theta 0})
   (wall {:x 9 :y 1 :length 1 :theta 0})

   (river [-2 7]
          [-1 7]
          [0 7]
          [1 8]
          [1 9]
          [2 9]
          [3 9]
          [4 9]
          ;[4 10]
          [5 10]
          ;[5 11]
          [6 10]
          [7 10]
          [8 10]
          [8 11]
          [9 11]
          [10 12]
          [10 13]
          [11 14]
          [11 15]
          [12 16]
          [12 17])

   (bridge 1 8 (/ Math/PI -3))

   (tree 5 3)
   (tree 5 7)
   (tree 2 8)
   (tree 2 13)


   ])
