(ns domination.card
  (:require [clojure.string :as string]))


(def width 210)
(def height 297)

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

(defn spot [color [xa ya _] [xb yb price]]
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
             :x2     xb
             :y2     yb
             :stroke price-line}]
     [:line {:x1     x1
             :y1     y1
             :x2     x2
             :y2     y2
             :stroke price-line
             :stroke-linecap "round"}]
     [:line {:x1     x1
             :y1     y1
             :x2     x3
             :y2     y3
             :stroke price-line
             :stroke-linecap "round"}]
     [:line {:x1     x2
             :y1     y2
             :x2     x3
             :y2     y3
             :stroke price-line
             :stroke-linecap "round"}]
     [:circle {:cx           xa
               :cy           ya
               :r            5
               :stroke       price-arc
               :stroke-width 0.5
               :fill         price-dot}]
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
               :fill         "url(#grad)"}]
     [:text {:x           xa
             :y           (+ ya 2)
             :text-anchor "middle"
             :style       {:font-size "6px"}}
      price]]))

(defn spots [color data]
  [:g
   (for [[a b] (partition 2 1 data)]
     (spot color a b))])

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

   (let [shield-prices (concat [0 1 1 1 1] (range 2 32))
         shield-xs (concat (reverse (take 6 (drop 1 (range 36 999 24))))
                           (take 23 (interleave (repeat 36) (repeat 15)))
                           (drop 1 (range 36 999 24)))
         shield-ys (concat (repeat 6 15)
                           (range 15 290 12)
                           (repeat 279))
         shield-color [50 150 205]]
     (spots shield-color (map vector shield-xs shield-ys shield-prices)))

   (let [minion-prices [0 1 2 4 8 16]
         minion-xs (reverse (take 6 (drop 1 (range 46 999 24))))
         minion-ys (range 15 279 (/ (- 279 15) 5))]
     [:g
      (spots [255 0 0] (map vector minion-xs (repeat (nth minion-ys 1)) minion-prices))
      (spots [0 255 0] (map vector minion-xs (repeat (nth minion-ys 2)) minion-prices))
      (spots [0 0 255] (map vector minion-xs (repeat (nth minion-ys 3)) minion-prices))
      (spots [255 255 0] (map vector minion-xs (repeat (nth minion-ys 4)) minion-prices))])])
