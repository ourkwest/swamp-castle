(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]))

(enable-console-print!)

(def iteration 3)

;(println "This text is printed from src/domination/core.cljs. Go ahead and edit it and see reloading in action.")

(def coin-1 {:label    "Coin"                               ; copper silver gold jewels bag-of-gold
             :desc     "Spend 1"
             :colour   [255 255 150]
             :coin     1
             :price    1})
(def coin-2 {:label    "Handful of Coins"                   ; copper silver gold jewels bag-of-gold
             :desc     "Spend 2"
             :colour   [255 255 75]
             :coin     2
             :price    2})
(def coin-4 {:label    "Bag of Coins"                       ; copper silver gold jewels bag-of-gold
             :desc     "Spend 4"
             :colour   [255 255 0]
             :coin     4
             :price    4})

(def move-1 {:label    "Step"                               ; lunge dash run
             :desc     "Move 1"
             :colour   [150 150 255]
             :move     1
             :price    2})
(def move-2 {:label    "Lunge"                              ; lunge dash run
             :desc     "Move 2"
             :colour   [100 100 255]
             :move     2
             :price    4})
(def move-3 {:label    "Dash"                               ; lunge dash run
             :desc     "Move 3"
             :colour   [50 50 255]
             :move     3
             :price    8})

(def train  {:label    "Train"                             ; flood, train, hire
             :desc     "Place a new minion"
             :colour   [150 255 150]
             :price    3})

(def gc     {:label    "Cleanse"                             ; or trash any unused from hand on every turn?
             :desc     "Trash ≥0 tokens from your hand"
             :colour   [255 200 50]
             :price    3})

(def gift   {:label    "Chocolate Cake"
             :desc     "Earn a Victory Point When in the Castle"
             :colour   [255 105 180]
             :price    15})

(def club   {:label    "Club"
             :desc     "Dmg: 1, Rng: 1"
             :colour   [255 200 200]
             :damage   [1]
             :price    2})
(def sword  {:label    "Sword"
             :desc     "Dmg: 2, Rng: 1"
             :colour   [255 150 150]
             :damage   [2]
             :price    4})
(def bsword {:label    "Big Sword"
             :desc     "Dmg: 4, Rng: 1"
             :colour   [255 100 100]
             :damage   [4]
             :price    6})
(def bow    {:label    "Bow"                              ; Damage (3 - Range), Range up to 3
             :desc     "Dmg: 4-Rng, Rng: ≤3"
             :colour   [255 75 75]
             :damage   [3 2 1]
             :price    8})


(defn add-desc [{:keys [move damage] :as item}]
  (assoc item :desc
              (string/join " "
                           (remove nil? [(when move (str "M: " move))
                                         (when damage (str "D: " (string/join "." damage)))]))))

(defn add-colour [{:keys [move damage] :as item}]
  (let [total-d (reduce + damage)
        df (- 1 (/ total-d 10))
        mf (- 1 (/ move 10))
        r (int (* 255 mf))
        g (int (* 255 mf df))
        b (int (* 255 df))]
    (assoc item :colour [r g b])))

(defn add-price [{:keys [move damage] :as item}]
  (let [total-d (reduce + (map + damage (range)))
        d-cost (+ total-d 1)
        m-cost (if move (Math/pow 2 move) 0)]
    (assoc item :price (+ m-cost d-cost))))

(def prepare-item (comp add-desc add-colour add-price))

(def peasant (prepare-item {:label  "Peasant"
                            :move   1}))
(def knave (prepare-item {:label  "Knave"
                            :damage [1]}))

(def scout (prepare-item {:label  "Scout"
                          :move 3}))

(def knight (prepare-item {:label  "Knight"
                           :move 3
                           :damage [5]}))

(def axeman (prepare-item {:label  "Axeman"
                           :move 1
                           :damage [3]}))

(def pikeman (prepare-item {:label  "Pikeman"
                           :move 2
                           :damage [1]}))

(def swordsman (prepare-item {:label  "Swordsman"
                           :move 2
                           :damage [2]}))

(def archer (prepare-item {:label  "Archer"
                           :move 1
                           :damage [1 1 1 1]}))

(def longbowman (prepare-item {:label  "Longbowman"
                               :damage [3 2 1]}))


(defonce app-state (atom {:text "Knights of Swamp Castle"
                          :hand-count 0
                          :hand-size 5
                          :draw [coin-1 coin-1 coin-1 peasant train]
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

(defn append-str
  ([] nil)
  ([string] string)
  ([string x]
   (if string
     (str string ", " x)
     x)))

(defn hello-world []

  (let [{:keys [text hand-size draw hand discard hand-count trash log]} @app-state]

    [:div
     [:h1 (str text " (" iteration ")")]

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
         [:span {:class "h-spaced"} (str "Spend: " (transduce (map :coin) + hand))]
         [:span {:class "h-spaced"} (str "Movement: " (transduce (comp (map :move) (remove nil?)) append-str hand))]
         ;[:span {:class "h-spaced"} (str "Summon: " (transduce (map :move) + hand))]
         [:span {:class "h-spaced"} (str "Weapons: "
                                         (transduce (comp (filter :damage) (map :label)) append-str hand)
                                         ;(frequencies
                                         ;              (map :label (filter #(= :weapon (:category %)) hand)))
                                         )]])]
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
     ;(buy-button move-1) (buy-button move-2) (buy-button move-3) [:br]
     ;(buy-button club) (buy-button sword) (buy-button bsword) (buy-button bow) [:br]
     (buy-button train) (buy-button gc) (buy-button gift) [:br]
     (buy-button peasant) (buy-button knave) (buy-button scout) [:br]
     (buy-button axeman) (buy-button swordsman) (buy-button pikeman) [:br]
     (buy-button archer) (buy-button longbowman) (buy-button knight)


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
