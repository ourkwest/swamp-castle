(ns domination.reference
  (:require [clojure.string :as string]
            [domination.simulator :as sim]
            [reagent.core :as reagent]))



(def width 500)
(def height 500)

(defn rgba
  ([r g b]
   (str "rgb(" r \, g \, b \)))
  ([r g b a]
   (str "rgba(" r \, g \, b \, a\))))

(defn rgb [cs]
  (apply rgba cs))

(defn link [_ id canvas]
  (let [link (.getElementById js/document id)]
    (aset link "download" (str id ".png"))
    (aset link "innerText" (str id ".png"))
    (aset link "href" (.toDataURL canvas "image/png"))))

(def TAU (* 2 Math/PI))

(defn clear [ctx colour]
  (aset ctx "fillStyle" (rgb colour))
  (.fillRect ctx 0 0 width height))

(defn circle [ctx x y radius fill-colour lw stroke-colour]
  (aset ctx "fillStyle" (rgb fill-colour))
  (aset ctx "strokeStyle" (rgb stroke-colour))
  (aset ctx "lineWidth" lw)
  (.beginPath ctx)
  (.arc ctx x y radius 0 TAU)
  (.fill ctx)
  (.stroke ctx))

(defn text
  ([ctx x y size msg fill-colour]
   (when msg
     (aset ctx "fillStyle" (rgb fill-colour))
     (aset ctx "font" (str size "px Verdana"))
     (aset ctx "textAlign" "center")
     (.fillText ctx msg x y)))
  ([ctx x y size msg fill-colour lw stroke-colour]
   (when msg
     (text ctx x y size msg fill-colour)
     (aset ctx "strokeStyle" (rgb stroke-colour))
     (aset ctx "lineWidth" lw)
     (.strokeText ctx msg x y))))

(defn badge [ctx colour amount x y]

  (circle ctx x y 50 colour 2 [0 0 0])
  (text ctx x (+ y 32) 90 amount [0 0 0])
  )

(defn connect [_ link canvas label]
  (aset link "download" (str "A_" label ".png"))
  ;(aset link "innerText" (str label ".png"))
  (aset link "href" (.toDataURL canvas "image/png")))

(let [minion [:b "Minion"]
      shield [:b "Shield"]]
  (defn render-instructions [c]
    (fn []
      [:div {:style {:text-align "center"}}
       ;[:br]
       [:h1 (:label c)]
       [:span
        (when (:desc c)
          [:span (interpose [:span "." [:br]] (string/split (:desc c) #"\.")) [:br]])
        (condp = (:move c)
          nil nil
          1 [:span "You may move this " minion " to " [:span {:class "move"} "an adjacent hex."] [:br]]
          [:span "You may move this " minion " " [:span {:class "move"} "up to " [:b (:move c) " hexes"]] "." [:br]])
        (when (:coin c) [:span "You have " [:span {:class "coin"} [:b (:coin c) " more to spend"]] " at turn's end." [:br]])
        (when (:shield c) [:span "You may take a " shield " from the piece card." [:br]])

        (when (:damage c)
          (if (:range c)
            [:span
             "You may attack one other " minion [:br]
             [:span {:class "range"} "up to " [:b (:range c) " hexes"] " away "] [:br]
             "with " [:span {:class "damage"} [:b (:damage c) " damage"]] " points." [:br]]
            [:span
             "You may attack one other " minion [:br]
             " on an adjacent hex " [:br]
             "with " [:span {:class "damage"} [:b (:damage c) " damage"]]
             (if (= (:damage c) 1)
               " point."
               " points.") [:br]]))

        [:span [:br] (str "Cost to buy: ") [:b (:price c)] [:br]]
        ]
       ;[:br]
       ])))

(defn render-tokens []


  ;[:svg {:id       "section-to-print"
  ;       :style    {:width "100%"}
  ;       :view-box (string/join " " [0 0 width height])}
  ;
  ; ]

  ;(sim/render-item (first sim/characters))

  ;var image = canvas.toDataURL("image/png").replace("image/png", "image/octet-stream");  // here is the most important part because if you dont replace you will get a DOM 18 exception.
  ;window.location.href=image;


  (js/setTimeout
    #(doseq [{:keys [move damage range coin shield label colour money? price br] :as character}
             ;(apply concat (interpose [{:br true}] (partition 3 sim/characters)))
             sim/characters
             ]


       (if br

         (let [break (.createElement js/document "br")]
           (.appendChild (.getElementById js/document "container") break))

         (let [div (.createElement js/document "div")
               div2 (.createElement js/document "div")
               canvas (.createElement js/document "canvas")
               link (.createElement js/document "a")

               inset 45
               margin 20

               [label1 label2 label3] (if (= label "Chocolate Cake")
                                        ["Chocolate" nil "Cake"]
                                        [nil label nil])]


           (aset canvas "width" width)
           (aset canvas "height" height)
           (.appendChild (.getElementById js/document "container") div)
           (.appendChild div link)
           (.appendChild link canvas)
           (.appendChild div div2)

           (.setAttribute canvas "style"
                          (str "margin: -" inset "px;"))
           (.setAttribute div "style"
                          (str "display: inline-block; "
                               "width: " (- width inset inset) "px; "
                               "background-color: white;"
                               "border-radius: 30px;"
                               "border: 1px solid black;"
                               "margin: 5px;"))
           (.setAttribute div2 "style"
                          (str "display: inline-block; "
                               "width: " (- width inset inset margin margin) "px; "
                               "vertical-align: top;"
                               "margin: " margin "px;"
                               "margin-top: -20px;"))

           (reagent/render-component [(render-instructions character)] div2)

           (let [ctx (.getContext canvas "2d")]

             ;(doto ctx
             ;  (clear [0 0 0])
             ;  ;(circle (/ width 2) (/ height 2) (* width 0.35) colour 2 [0 0 0])
             ;  )

             (doto ctx
               ;(clear colour)
               (circle (/ width 2) (/ height 2) (* width 0.38) colour 2 [0 0 0])
               (circle (/ width 2) (/ height 2) (* width 0.35) colour 2 [0 0 0])

               )

             (if money?

               (do
                 (text ctx (/ width 2) (+ (/ height 2) 35 (* coin 5)) (/ width (- 6 (/ coin 1.25)))
                       label
                       [0 0 0 0.5] 5 [0 0 0])
                 (let [badge-v 90
                       badge-h 60
                       mid (/ width 2)]
                   (when coin (badge ctx [255 255 0] coin (if move (+ mid badge-h) mid) (- mid badge-v)))))

               (do
                 (text ctx (/ width 2) (+ (/ height 2) 90) (/ width 1.4) "♟" [0 0 0 0.2] 5 [0 0 0 0.4])

                 (let [badge-v 90
                       badge-h 60
                       mid (/ width 2)]
                   (when move (badge ctx [0 255 0] move (if coin (- mid badge-h) mid) (- mid badge-v)))
                   (when coin (badge ctx [255 255 0] coin (if move (+ mid badge-h) mid) (- mid badge-v)))
                   (when damage (badge ctx [255 55 0] damage (if range (- mid badge-h) mid) (+ mid badge-v)))
                   (when range (badge ctx [255 125 50] range (if damage (+ mid badge-h) mid) (+ mid badge-v)))
                   (when shield (badge ctx [50 200 255] shield mid (+ mid badge-v))))


                 (if (= label "Chocolate Cake")
                   (doto ctx
                     (.drawImage (.getElementById js/document "image-of-cake") 150 80 200 200)
                     (text (/ width 2) (+ (/ height 2) 60) (/ width 9) "Chocolate" [0 0 0])
                     (text (/ width 2) (+ (/ height 2) 120) (/ width 9) "Cake" [0 0 0]))
                   (text ctx (/ width 2) (+ (/ height 2) 20) (/ width 9) label [0 0 0]))))

             (badge ctx [200 200 200] price (+ (/ width 2) 140) (+ (/ width 2) 140))


             ;(doto ctx
             ;  (connect link canvas label)
             ;  ;(circle (/ width 2) (/ height 2) (* width 0.38) [0 0 0 0] 1 [255 0 0])
             ;  )
             )

           ))

       ;(doto ctx
      ;  clear
      ;  (circle (/ width 2) (/ height 2) (* width 0.45) [200 200 200])
      ;  (text (/ width 2) (/ height 2) (/ width 10) label [200 200 200])
      ;
      ;  (link "img1" canvas))
      )


    1000)

  [:div {:id "container"}

   [:div {:style {:display "none"}}
    [:img {:src     "images/cake.png"
           :id      "image-of-cake"}]]

   ;[:canvas {:id     "canvas1"
   ;          :width  width
   ;          :height height}]
   ;
   ;
   ;[:a {:id "img1"}]

   ]

  )
