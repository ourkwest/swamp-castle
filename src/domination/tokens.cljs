(ns domination.tokens
  (:require [clojure.string :as string]
            [domination.simulator :as sim]))



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

(defn render-tokens []



  ;var image = canvas.toDataURL("image/png").replace("image/png", "image/octet-stream");  // here is the most important part because if you dont replace you will get a DOM 18 exception.
  ;window.location.href=image;


  (js/setTimeout
    #(doseq [{:keys [move damage range coin shield label colour money?]} (concat sim/characters
                                                                                     [sim/coin-a
                                                                                      sim/coin-b
                                                                                      sim/coin-c])]

      (let [canvas (.createElement js/document "canvas")
            link (.createElement js/document "a")

            [label1 label2 label3] (if (= label "Chocolate Cake")
                                     ["Chocolate" nil "Cake"]
                                     [nil label nil])]


        (aset canvas "width" width)
        (aset canvas "height" height)
        (.appendChild (.getElementById js/document "container") link)
        (.appendChild link canvas)

        (let [ctx (.getContext canvas "2d")]
          (doto ctx
            (clear colour)
            (circle (/ width 2) (/ height 2) (* width 0.35) colour 2 [0 0 0]))

          (if money?
            (text ctx (/ width 2) (+ (/ height 2) 35 (* coin 5)) (/ width (- 6 (/ coin 1.25)))
                  (condp = coin
                    1 "Bronze"
                    2 "Silver"
                    "Gold")
                  [0 0 0 0.5] 5 [0 0 0])
            (do

              (text ctx (/ width 2) (+ (/ height 2) 90) (/ width 1.4) "♟" [0 0 0 0.2] 5 [0 0 0 0.4])

              ;(let [badge-v 90
              ;      badge-h 60
              ;      mid (/ width 2)]
              ;  (when move (badge ctx [0 255 0] move (if coin (- mid badge-h) mid) (- mid badge-v)))
              ;  (when coin (badge ctx [255 255 0] coin (if move (+ mid badge-h) mid) (- mid badge-v)))
              ;  (when damage (badge ctx [255 55 0] damage (if range (- mid badge-h) mid) (+ mid badge-v)))
              ;  (when range (badge ctx [255 125 50] range (if damage (+ mid badge-h) mid) (+ mid badge-v)))
              ;  (when shield (badge ctx [50 200 255] shield mid (+ mid badge-v))))


              (if (= label "Chocolate Cake")
                (doto ctx
                  (.drawImage (.getElementById js/document "image-of-cake") 150 80 200 200)
                  (text (/ width 2) (+ (/ height 2) 60) (/ width 9) "Chocolate" [0 0 0])
                  (text (/ width 2) (+ (/ height 2) 120) (/ width 9) "Cake" [0 0 0]))
                (text ctx (/ width 2) (+ (/ height 2) 20) (/ width 9) label [0 0 0]))))

          (doto ctx
            (connect link canvas label)
            (circle (/ width 2) (/ height 2) (* width 0.38) [0 0 0 0] 1 [255 0 0])))

        )

      ;(doto ctx
      ;  clear
      ;  (circle (/ width 2) (/ height 2) (* width 0.45) [200 200 200])
      ;  (text (/ width 2) (/ height 2) (/ width 10) label [200 200 200])
      ;
      ;  (link "img1" canvas))
      )


    1000)

  [:div {:id "container"}

   [:img {:src "images/cake.png"
          :display "hidden"
          :id "image-of-cake"}]

   ;[:canvas {:id     "canvas1"
   ;          :width  width
   ;          :height height}]
   ;
   ;
   ;[:a {:id "img1"}]

   ]

  )
