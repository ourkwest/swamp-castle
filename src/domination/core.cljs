(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]))

(enable-console-print!)

(def iteration 6)

(def max-coins 10)
(defn make-money [amount price]
  {:label  (str amount " Coins")
   :desc   (str "Spend " amount)
   :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
   :coin   amount
   :price  price})

(def coin-a (make-money 1 2))
(def coin-b (make-money 2 4))
(def coin-c (make-money 4 8))


(def train  {:label    "Train"                             ; flood, train, hire
             :desc     "Place a new minion"
             :colour   [150 255 150]
             :price    3})

(def gc     {:label    "Cleanse"                             ; or trash any unused from hand on every turn?
             :desc     "Trash ≥0 tokens from your hand"
             :colour   [255 200 50]
             :price    1})

(def gift   {:label    "Chocolate Cake"
             :desc     "Earn a Victory Point When in the Castle"
             :colour   [255 105 180]
             :price    10})

(def shield   {:label    "Blacksmith"
               :desc     "Take a shield for free"
               :colour   [0 150 255]
               :price    8})

(defn add-desc [{:keys [move damage range] :as item}]
  (assoc item :desc
              (string/join " "
                           (remove nil? [(when move (str "M: " move))
                                         (when damage (str "D: " damage))
                                         (when range (str "R: " range))]))))

(defn add-colour [{:keys [move damage] :as item}]
  (let [df (- 1 (/ damage 7))
        mf (- 1 (/ move 7))
        r (int (* 255 mf))
        g (int (* 255 mf df))
        b (int (* 255 df))]
    (assoc item :colour [r g b])))

(defn add-price [{:keys [move damage range price] :as item}]
  (let [r-cost (if range 1 0)
        cost (- (* 2 (+ move damage r-cost)) (Math/abs (- move damage)))]
    (assoc item :price (or price cost))))

(def prepare-item (comp add-desc add-colour add-price))

(def peasant (prepare-item {:label  "Peasant"
                            :move   1}))
(def knave (prepare-item {:label  "Knave"
                          :damage  1}))

(def scout (prepare-item {:label  "Scout"
                          :move 3
                          :price 5}))

(def knight (prepare-item {:label  "Knight"
                           :move   3
                           :damage  4}))

(def axeman (prepare-item {:label  "Axeman"
                           :move   1
                           :damage  3}))

(def pikeman (prepare-item {:label  "Pikeman"
                            :move   2
                            :damage  1}))

(def swordsman (prepare-item {:label  "Swordsman"
                              :move   2
                              :damage  2}))

(def archer (prepare-item {:label  "Archer"
                           :move   2
                           :damage 1
                           :range 4}))

(def longbowman (prepare-item {:label  "Longbowman"
                               :move 1
                               :damage [3 2 1]}))


(defonce app-state (atom {:text       "Knights of Swamp Castle"
                          :hand-count 0
                          :hand-size  5
                          :draw       (shuffle [coin-a coin-a coin-a peasant train])
                          :hand       []
                          :discard    []
                          :trash      []
                          :log        '()}))

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn draw-tokens [n draw hand discard]
  (cond
    (zero? n) [draw hand discard]
    (empty? draw) (draw-tokens n (shuffle discard) hand [])
    :else (draw-tokens (dec n) (rest draw) (conj hand (first draw)) discard)))


(defn draw-to-hand
  ([{:keys [hand-size draw hand discard hand-count log] :as state} draw-type]
   (let [[n hand' discard' hand-count'] (case draw-type
                                          :hand [hand-size [] (concat discard hand) (inc hand-count)]
                                          :token [1 hand discard hand-count])
         [new-draw new-hand new-discard] (draw-tokens n draw hand' discard')]
     (assoc state :draw new-draw
                  :hand new-hand
                  :discard new-discard
                  :hand-count hand-count'
                  :log (conj log (str "Drew: " (string/join ", " (map :label (take-last n new-hand)))))))))

(defn buy [item]
  (swap! app-state update :discard conj item)
  (swap! app-state update :log conj (str "Bought " (:label item))))

(defn buy-button [item]
  [:div
   {                                                        ;:on-click #(buy item)
    :key      (:label item)
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
              :on-click #(swap! app-state draw-to-hand :hand)
              :style {:color :green
                      :font-size "150%"
                      :background-color :lightgreen}}]
     [:input {:type :button
              :value (str "Draw 1")
              :on-click #(swap! app-state draw-to-hand :token)
              :style {:color :green
                      :font-size "125%"
                      :background-color :lightgreen}}]


     [:h3 "Buy"]

     (buy-button coin-a) (buy-button coin-b) (buy-button coin-c) [:br]
     (buy-button train) (buy-button shield) (buy-button gift) [:br]


     (for [item (sort-by :price [peasant scout axeman swordsman archer knight])]
       (buy-button item))

     ;(buy-button peasant) (buy-button scout) [:br]
     ;(buy-button axeman) (buy-button swordsman) [:br]
     ;(buy-button archer) (buy-button knight)


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
