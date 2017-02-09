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

(defn wall [x y theta]
  (let [x-loc (x-pos x y)
        y-loc (y-pos y)]
    [:g {:id        (str "Wall-" x "-" y)
         :transform (str "rotate(" theta "," x-loc "," y-loc ")")
         }
     [:rect {:style  {:fill         "white"
                      :stroke       "grey"
                      :stroke-width 1}
             :x      (- x-loc (* hex-radius 2))
             :y      (- y-loc (/ hex-radius 2.4))
             :width  (* hex-radius 4)
             :height (/ hex-radius 1.2)}]
     [:polygon {:style  {:fill         "white"
                         :stroke       "grey"
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius) (+ x-loc x-step) y-loc)}]
     [:polygon {:style  {:fill         "white"
                         :stroke       "grey"
                         :stroke-width 1}
                :points (hex-points (* 0.8 hex-radius) (- x-loc x-step) y-loc)}]]))

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

    [:g {:id "Hex"}
           [:polygon {
                      :style {:fill "url(#grad)"
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
    ]

   (for [y (range n-y)
         x (range (if (even? y) n-x (dec n-x)))]
     [:use {:key       (str x "-" y)
            :x         (x-pos x y)
            :y         (y-pos y)
            :xlinkHref "#Hex"}])


   (wall 5 5 0)
   (wall 3 4 60)
   (wall 8 4 -60)
   (wall 1 1 0)
   (wall 9 1 0)

   ])
