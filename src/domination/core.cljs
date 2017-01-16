(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/domination/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(def coin-1 {:label    "Coin"                               ; copper silver gold jewels bag-of-gold
             :colour   [255 255 150]
             :category :coin
             :value    1
             :price    1})
(def coin-2 {:label    "Handful of Coins"                   ; copper silver gold jewels bag-of-gold
             :colour   [255 255 75]
             :category :coin
             :value    2
             :price    2})
(def coin-4 {:label    "Bag of Coins"                       ; copper silver gold jewels bag-of-gold
             :colour   [255 255 0]
             :category :coin
             :value    4
             :price    4})

(def move-1 {:label    "Step"                               ; lunge dash run
             :colour   [150 150 255]
             :category :movement
             :value    1
             :price    1})
(def move-2 {:label    "Lunge"                              ; lunge dash run
             :colour   [100 100 255]
             :category :movement
             :value    2
             :price    2})
(def move-4 {:label    "Dash"                               ; lunge dash run
             :colour   [50 50 255]
             :category :movement
             :value    4
             :price    4})

(def train  {:label     "Train"                             ; flood, train, hire
             :colour   [150 255 150]
             :category :summon
             :value    1
             :price    3})

(def club   {:label    "Club"                               ; Damage 1, Range 1
             :colour   [255 200 200]
             :category :weapon
             :price    2})
(def sword  {:label    "Sword"                              ; Damage 3, Range 1
             :colour   [255 150 150]
             :category :weapon
             :price    4})
(def bow    {:label    "Bow"                              ; Damage (3 - Range), Range up to 3
             :colour   [255 100 100]
             :category :weapon
             :price    8})


(defonce app-state (atom {:text "Domination"
                          :hand-count 0
                          :hand-size 4
                          :draw [coin-1 coin-1 coin-1
                                 move-1 move-1
                                 train]
                          :hand []
                          :discard []}))

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn draw-tokens [n draw hand discard]
  (cond
    (zero? n) [draw hand discard]
    (empty? draw) (draw-tokens n (shuffle discard) hand [])
    :else (draw-tokens (dec n) (rest draw) (conj hand (first draw)) discard)))


(defn draw-hand [{:keys [hand-size draw hand discard hand-count] :as state}]
  (let [[new-draw new-hand new-discard] (draw-tokens hand-size draw [] (concat discard hand))]
    (assoc state :draw new-draw
                 :hand new-hand
                 :discard new-discard
                 :hand-count (inc hand-count))))

(defn buy [item]
  (swap! app-state update :discard conj item))

(defn buy-button [item]
  [:input {:type     :button
           :value    (:label item)
           :on-click #(buy item)
           :style    {:background-color (rgb (:colour item))}}])

(defn sum-categories [m {:keys [category value]}]
  (update m category (fnil + 0) value))

(defn hello-world []

  (let [{:keys [text hand-size draw hand discard hand-count]} @app-state]

    [:div
     [:h1 text]

     [:div
      [:span {:class "h-spaced"} (str " Draw pile: " (count draw))]
      [:span {:class "h-spaced"} (str " Hand #: " hand-count)]
      [:span {:class "h-spaced"} (str " Discard pile: " (count discard))]
      [:span {:class "h-spaced"} (str " Total deck: " (+ (count discard) (count hand) (count draw)))]
      ]

     [:div {:style {:border "1px solid black"
                    :border-radius "5px"
                    :display "inline-block"}}

      [:div
       (for [[index item] (map-indexed vector (sort-by (juxt :category :value) hand))]
         [:span {
                 :class "h-spaced"
                 :key   (str "token-" index)
                 :style {:background-color (rgb (:colour item))}
                 }
          (:label item)])]


      (let [totals (reduce sum-categories {} hand)]

        [:div {:class "available"}
         [:span {:class "h-spaced"} (str "Spend: " (or (:coin totals) \-))]
         [:span {:class "h-spaced"} (str "Movement: " (or (:movement totals) \-))]
         [:span {:class "h-spaced"} (str "Summon: " (or (:summon totals) \-))]

         (println hand)
         (println (filter #(= :weapon (:cateogry %)) hand))

         [:span {:class "h-spaced"} (str "Weapons: " (frequencies
                                                       (map :label (filter #(= :weapon (:category %)) hand))))]
         ])]

     [:br]

     [:input {:type :button
              :value (str "Draw " hand-size)
              :on-click #(swap! app-state draw-hand)
              :style {:color :green
                      :background-color :lightgreen}}]


     [:h3 "Buy"]

     (buy-button coin-1) (buy-button coin-2) (buy-button coin-4) [:br]
     (buy-button move-1) (buy-button move-2) (buy-button move-4) [:br]
     (buy-button club) (buy-button sword) (buy-button bow) [:br]
     (buy-button train)


     ]))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
