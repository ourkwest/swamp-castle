(ns domination.card
  (:require [clojure.string :as string]))


(def width 210)
(def height 297)

(def TAU (* 2.0 Math/PI))

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn vect [[xa ya] [xb yb]]
  [(- xb xa) (- yb ya)])

(defn scale [[x y] scale]
  [(* x scale) (* y scale)])

(defn plus
  ([[x y] [xx yy]]
   [(+ x xx) (+ y yy)])
  ([a b c & more]
   (apply plus (plus a b) c more)))

(defn rotate-90 [[x y]]
  [y (- x)])


(defn spot-price [color [xa ya _] [xb yb price]]
  (let [price-line :black
        price-arc :black
        price-dot (rgb [155 255 0])
        v1 (vect [xa ya] [xb yb])
        v2 (rotate-90 v1)
        [x1 y1] (plus [xa ya]
                      (scale v1 0.53))
        [x2 y2] (plus [xa ya]
                      (scale v1 0.47)
                      (scale v2 0.06))
        [x3 y3] (plus [xa ya]
                      (scale v1 0.47)
                      (scale v2 -0.06))]
    [:g {:key (str "spot-" (int xa) \- (int ya))}
     [:line {:x1     xa
             :y1     ya
             :x2     x1
             :y2     y1
             :stroke price-line}]
     [:line {:x1             x1
             :y1             y1
             :x2             x2
             :y2             y2
             :stroke         price-line
             :stroke-linecap "round"}]
     [:line {:x1             x1
             :y1             y1
             :x2             x3
             :y2             y3
             :stroke         price-line
             :stroke-linecap "round"}]
     [:line {:x1             x2
             :y1             y2
             :x2             x3
             :y2             y3
             :stroke         price-line
             :stroke-linecap "round"}]
     [:circle {:cx           xa
               :cy           ya
               :r            5
               :stroke       price-arc
               :stroke-width 0.5
               :fill         price-dot}]
     [:text {:x           xa
             :y           (+ ya 2)
             :text-anchor "middle"
             :style       {:font-size "6px"}}
      price]]))

(defn spot-place [color [xa ya _] [xb yb price]]
  [:g
   [:circle {:cx           xb
             :cy           yb
             :r            11
             :stroke       :black
             :stroke-width 0.5
             :fill         (rgb color)}]
   [:circle {:cx           xb
             :cy           yb
             :r            11
             :stroke       :black
             :stroke-width 0.5
             :fill         "url(#grad)"}]])

(defn spot [color [xa ya _] [xb yb price] [place? price?]]
  [:g
   (when place? (spot-place color [xa ya _] [xb yb price]))
   (when price? (spot-price color [xa ya _] [xb yb price]))])

(defn spots [color data [place? price?]]
  [:g
   (for [[a b] (partition 2 1 data)]
     (spot color a b [place? price?]))])

(defn render-card []

  [:svg {:id "section-to-print"
         :style    {:width  "100%"}
         :view-box (string/join " " [0 0 width height])}

   [:defs
    [:radialGradient {:id "grad" :fx 0.5 :fy 0.5 :r 1}
     [:stop {:stop-color "white" :stop-opacity 1 :offset "0%"}]
     [:stop {:stop-color "white" :stop-opacity 1 :offset "35%"}]
     [:stop {:stop-color "white" :stop-opacity 0 :offset "50%"}]
     [:stop {:stop-color "white" :stop-opacity 0 :offset "100%"}]]]

   [:rect {:x      (+ 4.5 (* 0 21))
           :y      2
           :width  (+ (* 4 (+ 21 25)) 4)
           :height (- height 10)
           :style  {:fill             (rgb [140 140 140])
                    :stroke           (rgb [100 100 100])
                    :stroke-width     0.6
                    :stroke-dasharray "2,0.2"}}]
   [:rect {:x      (+ 4.5 (* 1 21))
           :y      20
           :width  (+ (* 3 (+ 21 25)) 4)
           :height (- height 30)
           :style  {:fill             (rgb [180 180 180])
                    :stroke           (rgb [100 100 100])
                    :stroke-width     0.6
                    :stroke-dasharray "2,0.2"}}]
   [:rect {:x      (+ 4.5 (* 2 21))
           :y      38
           :width  (+ (* 2 (+ 21 25)) 4)
           :height (- height 50)
           :style  {:fill             (rgb [220 220 220])
                    :stroke           (rgb [100 100 100])
                    :stroke-width     0.6
                    :stroke-dasharray "2,0.2"}}]

   [:text {:x           (+ 4.5 (* 4 21) 2)
           :y           50
           :text-anchor "middle"
           :style       {:font-size "10px"}}
    "2 Players"]

   [:text {:x           (+ 4.5 (* 4 21) 2)
           :y           32
           :text-anchor "middle"
           :style       {:font-size "10px"}}
    "3 Players"]

   [:text {:x           (+ 4.5 (* 4 21) 2)
           :y           14
           :text-anchor "middle"
           :style       {:font-size "10px"}}
    "4 Players"]


   (let [shield-prices (range 0 17)

         shield-xs (interleave (repeat 36) (repeat 15))
         shield-ys (range 60 290 12)

         shield-color [50 150 205]]
     [:g
      (spots shield-color (map vector shield-xs shield-ys shield-prices) [1 nil])
      (spots shield-color (map vector (map #(+ 21 %) (drop 1 shield-xs)) shield-ys shield-prices) [1 nil])
      (spots shield-color (map vector (map #(+ 42 %) shield-xs) shield-ys shield-prices) [1 nil])

      (spots shield-color (map vector shield-xs shield-ys shield-prices) [nil 1])
      (spots shield-color (map vector (map #(+ 21 %) (drop 1 shield-xs)) shield-ys shield-prices) [nil 1])
      (spots shield-color (map vector (map #(+ 42 %) shield-xs) shield-ys shield-prices) [nil 1])
      ])


   (let [minion-prices [0 1 2 4 6 8]
         start-x (iterate #(+ 25 %) 105)
         start-y 60
         angle (/ TAU 4)
         step-size 24
         x-step (* (Math/cos angle) step-size)
         y-step (* (Math/sin angle) step-size)
         minion-xs (map (partial iterate #(+ x-step %)) start-x)
         minion-ys (iterate #(+ y-step %) start-y)
         ]
     [:g
      (spots [255 0 0] (map vector (nth minion-xs 0) minion-ys minion-prices) [1 1])
      (spots [0 255 0] (map vector (nth minion-xs 1) minion-ys minion-prices) [1 1])
      (spots [0 0 255] (map vector (nth minion-xs 2) minion-ys minion-prices) [1 1])
      (spots [255 255 0] (map vector (nth minion-xs 3) minion-ys minion-prices) [1 1])])



   ])
