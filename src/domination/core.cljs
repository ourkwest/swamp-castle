(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [domination.board :as board]
            [domination.card :as card]))

(enable-console-print!)

(def iteration 10)

(defn add-syms [{:keys [move damage range coin shield] :as item}]
  (let [syms (remove nil? [(when move [:span {:class "smallpad"
                                               :key   "m"
                                               :style {:background-color "rgb(0,255,0)"
                                                       :border           "1px solid black"
                                                       :border-radius    "10px"}} (str "\u00A0" move "\u00A0")])
                            (when damage [:span {:class "smallpad"
                                                 :key   "d"
                                                 :style {:background-color "rgb(255,55,0)"
                                                         :border           "1px solid black"
                                                         :border-radius    "10px"}} (str "\u00A0" damage "\u00A0")])
                            (when range [:span {:class "smallpad"
                                                :key   "r"
                                                :style {:background-color "rgb(255, 125, 0)"
                                                        :border           "1px solid black"
                                                        :border-radius    "10px"}} (str "\u00A0" range "\u00A0")])
                            (when coin [:span {:class "smallpad"
                                               :key   "c"
                                               :style {:background-color "rgb(255,225,0)"
                                                       :border           "1px solid black"
                                                       :border-radius    "10px"}} (str "\u00A0" coin "\u00A0")])
                            (when shield [:span {:class "smallpad"
                                                :key   "t"
                                                :style {:background-color "rgb(0,200,250)"
                                                        :border           "1px solid black"
                                                        :border-radius    "10px"}} (str "\u00A0" shield "\u00A0")])])]
    (if (empty? syms)
      item
      (assoc item :syms [:span syms]))))

(def max-coins 4)
(defn make-money [amount price]
  (add-syms {:label  (if (= 1 amount) "1 Coin" (str amount " Coins"))
             :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
             :coin   amount
             :price  price}))

(def coin-a (make-money 1 0))
(def coin-b (make-money 2 4))
(def coin-c (make-money 4 8))

(defn add-colour [{:keys [move damage colour] :as item}]
  (let [df (- 1 (/ damage 7))
        mf (- 1 (/ move 7))
        r (int (* 255 mf))
        g (int (* 255 mf df))
        b (int (* 255 df))]
    (assoc item :colour (or colour [150 200 255]))))

(defn add-price [{:keys [move damage range token coin price] :as item}]
  (let [r-cost (if range 1 0)
        cost (+ (- (* 2 (+ move damage r-cost token)) (Math/abs (- move damage))) coin)]
    (assoc item :price (or price cost))))

(def prepare-item (comp add-syms add-colour add-price))

(defn character [label move damage coin shield range price colour desc]
  (let [character-label (.replace label " " "-")
        character-symbol (symbol character-label)]
    (prepare-item {:label (str "♟ " character-label)
                   :move move
                   :damage damage
                   :shield shield
                   :coin coin
                   :range range
                   :price price
                   :colour colour
                   :desc desc})))

(def characters
  [
;                               Move  Dmg.  Coin  Shield Range Price Color Description

   (character "Farmer"          1     nil   1     nil    nil   2     nil   nil)
   (character "Horse Rider"     3     nil   nil   nil    nil   3     nil   nil)
   (character "Brigand"         1     1     2     nil    nil   4     nil   nil)
   (character "Archer"          2     1     nil   nil    4     5     nil   nil)
   (character "Blacksmith"      nil   nil   nil   1      nil   6     nil   nil)
   (character "Bezerker"        1     3     nil   nil    nil   7     nil   nil)
   (character "Assassin"        2     2     2     nil    nil   8     nil   nil)
   (character "Chocolate Cake"  nil   nil   nil   nil    nil   10    [255 50 200] "Sacrifice this token and ♟ in the Castle for a Victory Point.")

   ;(character "Plough"          1     nil   nil   1     nil   2     nil   nil)
   ;(character "Horse"           3     nil   1     nil   nil   3     nil   nil)
   ;(character "Dagger"          1     1     1     nil   nil   4     nil   nil)
   ;(character "Bow"             2     1     nil   1     4     5     nil   nil)
   ;(character "Furnace"         nil   nil   nil   1     nil   6     [255 100 0] " + Take a shield for free")
   ;(character "Axe"             1     3     nil   1     nil   7     nil   nil)
   ;(character "Sword"           2     2     2     nil   nil   8     nil   nil)
   ;(character "Chocolate Cake"  nil   nil   nil   nil   nil   10    [255 50 200] "Sacrifice this token and ♟ in the Castle for a Victory Point.")
   ])

(defn uniqueness []
  [(int (rand 255)) (int (rand 255)) (int (rand 255))])

(def state-history (atom '()))

;; normally defonce...
(def app-state (add-watch (atom {:text       "Knights of Swamp Castle"
                                 :uniqueness (uniqueness)
                                 :hand-count 0
                                 :hand-size  4
                                 :draw       (shuffle [coin-a coin-a coin-a coin-a])
                                 :hand       []
                                 :discard    []
                                 :trash      []
                                 :log        '()})
                          :history
                          (fn [_ _ old _]
                            (swap! state-history conj old))))

(defn undo-state-change []
  (let [[prev & history] @state-history]
    (when prev
      (reset! app-state prev)
      (reset! state-history history))))

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn draw-tokens [n draw hand discard]
  (cond
    (zero? n) [draw hand discard]
    (and (empty? draw) (empty? discard)) [draw hand discard]
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
  (swap! app-state #(-> %
                        (update :discard conj item)
                        (update :log conj (str "Bought " (:label item))))))

(defn buy-button [item]
  [:div
   {                                                        ;:on-click #(buy item)
    :key      (:label item)
    :class    "outlined"
    :style    {:display :inline-block
               :text-align :center
               :background-color (rgb (:colour item))}}
   ;[:div {:style {:display :inline-block}}]
   [:span {:class "bigpad"
           :style {:font-size "150%"}} (:label item)]
   [:br]
   (when-let [syms (:syms item)]
     [:span
      [:span {:style {:font-size "75%"}} syms]])
   (when-let [desc (:desc item)]
     [:span
      [:span {:style {:font-size "75%"}} desc]])
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

  (let [{:keys [text uniqueness hand-size draw hand discard hand-count trash log]} @app-state]

    [:div

     {:style {:border (str "3px solid " (rgb uniqueness))
              :border-top-width "20px"
              :padding "10px"
              :margin "0px"}}

     [:h3 {:style {:margin-top 0}} (str text " (" iteration ") ")]
     ;[:div {:style {:background-color (rgb uniqueness)
     ;               :text-align :center}} "\u00A0"]

     [:div
      [:span {:class "h-spaced"} (str " Draw pile: " (count draw))]
      [:span {:class "h-spaced"} (str " Hand #: " hand-count)]
      [:span {:class "h-spaced"} (str " Discard pile: " (count discard))]
      [:span {:class "h-spaced"} (str " Total deck: " (+ (count discard) (count hand) (count draw)))]
      ]

     [:div {:class "outlined"
            :style {:display "inline-block"}}
      "Hand"
      [:div
       (for [[index item] (map-indexed vector (sort-by (juxt :category :value) hand))]
         [:span {:class "outlined h-spaced"
                 :key   (str "token-" index)
                 :style {:background-color (rgb (:colour item))}
                 :on-click #(swap! app-state trash-from-hand item)}
           (:label item) [:span {:style {:font-size "75%"}} (:syms item)]])]

      ;(let [totals (reduce sum-categories {} hand)]
      ;  [:div {:class "available"}
      ;   [:span {:class "h-spaced"} (str "Spend: " (transduce (map :coin) + hand))]
      ;   ;[:span {:class "h-spaced"} (str "Movement: " (transduce (comp (map :move) (remove nil?)) append-str hand))]
      ;   ;[:span {:class "h-spaced"} (str "Summon: " (transduce (map :move) + hand))]
      ;   ;[:span {:class "h-spaced"} (str "Weapons: "
      ;   ;                                (transduce (comp (filter :damage) (map :label)) append-str hand)
      ;   ;                                ;(frequencies
      ;   ;                                ;              (map :label (filter #(= :weapon (:category %)) hand)))
      ;   ;                                )]
      ;   ])
      ]
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
     "\u00A0"
     [:input {:type :button
              :value (str "Draw 1")
              :on-click #(swap! app-state draw-to-hand :token)
              :style {:color :green
                      :font-size "125%"
                      :background-color :lightgreen}}]


     [:h3 "Buy"]

     [:div (str "New minions (per player): " (string/join " | " [1 2 4 6 8]))]
     [:div (str "Shields (global): " (string/join " | " [1 1 1 2 2 3 4 5 6 7 8 9 10 11 12 "etc."]))]

     [:span "Money Tokens:"] [:br]
     (buy-button coin-a) (buy-button coin-b) (buy-button coin-c) [:br]
     ;(buy-button train) (buy-button shield) (buy-button gift) [:br]

     [:span "Character Tokens: play up to one per minion. You may draw a new token in to your hand to replace each played Character Token."] [:br]
     (for [item (sort-by :price characters)]
       (buy-button item))

     ;(buy-button peasant) (buy-button scout) [:br]
     ;(buy-button axeman) (buy-button swordsman) [:br]
     ;(buy-button archer) (buy-button knight)


     [:div
      {:style {:font-size "50%"}}
      (for [[id msg] (map-indexed vector log)]
        [:span {:key (str "log-" id)} msg [:br]])]

     [:input {:type :button
              :value "Undo"
              :on-click undo-state-change}]

     ]))

(when-let [element (. js/document (getElementById "app"))]
  (reagent/render-component [hello-world] element))

(when-let [element (. js/document (getElementById "board"))]
  (reagent/render-component [board/render-board] element))

(when-let [element (. js/document (getElementById "card"))]
  (reagent/render-component [card/render-card] element))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
