(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]))

(enable-console-print!)

;(println "This text is printed from src/domination/core.cljs. Go ahead and edit it and see reloading in action.")

(def coin-1 {:label    "Coin"                               ; copper silver gold jewels bag-of-gold
             :desc     "Spend 1"
             :colour   [255 255 150]
             :category :coin
             :value    1
             :price    1})
(def coin-2 {:label    "Handful of Coins"                   ; copper silver gold jewels bag-of-gold
             :desc     "Spend 2"
             :colour   [255 255 75]
             :category :coin
             :value    2
             :price    2})
(def coin-4 {:label    "Bag of Coins"                       ; copper silver gold jewels bag-of-gold
             :desc     "Spend 4"
             :colour   [255 255 0]
             :category :coin
             :value    4
             :price    4})

(def move-1 {:label    "Step"                               ; lunge dash run
             :desc     "Move 1"
             :colour   [150 150 255]
             :category :movement
             :value    1
             :price    2})
(def move-2 {:label    "Lunge"                              ; lunge dash run
             :desc     "Move 2"
             :colour   [100 100 255]
             :category :movement
             :value    2
             :price    4})
(def move-3 {:label    "Dash"                               ; lunge dash run
             :desc     "Move 3"
             :colour   [50 50 255]
             :category :movement
             :value    3
             :price    8})

(def train  {:label    "Train"                             ; flood, train, hire
             :desc     "Place a new minion"
             :colour   [150 255 150]
             :category :summon
             :value    1
             :price    3})
;(def gc     {:label    "Cleanse"                             ; or trash any unused from hand on every turn?
;             :desc     "Trash ≥0 tokens from your hand"
;             :colour   [255 200 50]
;             :category :summon
;             :value    1
;             :price    3})

(def gift   {:label    "Chocolate Cake"
             :desc     "Earn a Victory Point When in the Castle"
             :colour   [255 105 180]
             :category :victory
             :price    5})

(def club   {:label    "Club"
             :desc     "Dmg: 1, Rng: 1"
             :colour   [255 200 200]
             :category :weapon
             :price    2})
(def sword  {:label    "Sword"
             :desc     "Dmg: 2, Rng: 1"
             :colour   [255 150 150]
             :category :weapon
             :price    4})
(def bsword {:label    "Big Sword"
             :desc     "Dmg: 4, Rng: 1"
             :colour   [255 100 100]
             :category :weapon
             :price    6})
(def bow    {:label    "Bow"                              ; Damage (3 - Range), Range up to 3
             :desc     "Dmg: 4-Rng, Rng: ≤3"
             :colour   [255 75 75]
             :category :weapon
             :price    8})


(defonce app-state (atom {:text "Knights of Swamp Castle"
                          :hand-count 0
                          :hand-size 5
                          :draw [coin-1 coin-1 coin-1
                                 move-1
                                 train]
                          :hand []
                          :discard []
                          :trash []
                          :log '()}))

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn draw-tokens [n draw hand discard]
  (cond
    (zero? n) [draw hand discard]
    (empty? draw) (draw-tokens n (shuffle discard) hand [])
    :else (draw-tokens (dec n) (rest draw) (conj hand (first draw)) discard)))


(defn draw-hand [{:keys [hand-size draw hand discard hand-count log] :as state}]
  (let [[new-draw new-hand new-discard] (draw-tokens hand-size draw [] (concat discard hand))]
    (assoc state :draw new-draw
                 :hand new-hand
                 :discard new-discard
                 :hand-count (inc hand-count)
                 :log (conj log (str "Drew: " (string/join ", " (map :label new-hand)))))))

(defn buy [item]
  (swap! app-state update :discard conj item)
  (swap! app-state update :log conj (str "Bought " (:label item))))

(defn buy-button [item]
  [:div
   {                                                        ;:on-click #(buy item)
    :class    "outlined"
    :style    {:display :inline-block
               :text-align :center
               :background-color (rgb (:colour item))}}
   ;[:div {:style {:display :inline-block}}]
   [:span {:style {:font-size "150%"}} (:label item)]
   [:br]
   [:span {:style {:font-size "75%"}} (:desc item)]
   [:br]
   [:input {:type     :button
            :value    (str "Buy for " (:price item))
            :on-click #(buy item)
            :style    {:background-color (rgb (:colour item))}}]])

(defn trash-from-hand [{:keys [hand trash log] :as state} item]
  (let [like-item (filter #(= % item) hand)
        not-item (remove #(= % item) hand)]
    (assoc state :trash (vec (conj trash item))
                 :hand (vec (sort-by (juxt :category :value)
                                     (concat not-item (rest like-item))))
                 :log (conj log (str "Trashed: " (:label item))))))

(defn untrash-to-hand [{:keys [hand trash log] :as state} item]
  (let [like-item (filter #(= % item) trash)
        not-item (remove #(= % item) trash)]
    (assoc state :hand (vec (sort-by (juxt :category :value) (conj hand item)))
                 :trash (vec (concat not-item (rest like-item)))
                 :log (conj log (str "Un-trashed: " (:label item))))))

(defn sum-categories [m {:keys [category value]}]
  (update m category (fnil + 0) value))

(defn hello-world []

  (let [{:keys [text hand-size draw hand discard hand-count trash log]} @app-state]

    [:div
     [:h1 text]

     [:div
      [:span {:class "h-spaced"} (str " Draw pile: " (count draw))]
      [:span {:class "h-spaced"} (str " Hand #: " hand-count)]
      [:span {:class "h-spaced"} (str " Discard pile: " (count discard))]
      [:span {:class "h-spaced"} (str " Total deck: " (+ (count discard) (count hand) (count draw)))]
      ]

     [:div {:class "outlined"
            :style {:display "inline-block"}}

      [:div
       (for [[index item] (map-indexed vector (sort-by (juxt :category :value) hand))]
         [:span {:class "outlined h-spaced"
                 :key   (str "token-" index)
                 :style {:background-color (rgb (:colour item))}
                 :on-click #(swap! app-state trash-from-hand item)}
          (:label item)])]

      (let [totals (reduce sum-categories {} hand)]
        [:div {:class "available"}
         [:span {:class "h-spaced"} (str "Spend: " (or (:coin totals) \-))]
         [:span {:class "h-spaced"} (str "Movement: " (or (:movement totals) \-))]
         [:span {:class "h-spaced"} (str "Summon: " (or (:summon totals) \-))]
         [:span {:class "h-spaced"} (str "Weapons: " (frequencies
                                                       (map :label (filter #(= :weapon (:category %)) hand))))]])]
     [:div {:class "outlined"
            :style {:display "inline-block"}}
      "Trash"
      [:div
       (for [[index item] (map-indexed vector (sort-by (juxt :category :value) trash))]
         [:span {:key      (str "token-" index)
                 :class    "outlined"
                 :style    {:background-color (rgb (:colour item))}
                 :on-click #(swap! app-state untrash-to-hand item)}
          (:label item)])]]

     [:br]

     [:input {:type :button
              :value (str "Draw " hand-size)
              :on-click #(swap! app-state draw-hand)
              :style {:color :green
                      :font-size "150%"
                      :background-color :lightgreen}}]


     [:h3 "Buy"]

     (buy-button coin-1) (buy-button coin-2) (buy-button coin-4) [:br]
     (buy-button move-1) (buy-button move-2) (buy-button move-3) [:br]
     (buy-button club) (buy-button sword) (buy-button bsword) (buy-button bow) [:br]
     (buy-button train) (buy-button gift)


     [:div
      {:style {:font-size "50%"}}
      (for [[id msg] (map-indexed vector log)]
        [:span {:key (str "log-" id)} msg [:br]])]

     ]))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
