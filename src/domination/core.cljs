(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.events :as events]
            [domination.board :as board]
            [domination.card :as card]))

(enable-console-print!)

(def iteration 11)

(defn rgb [[r g b]]
  (str "rgb(" r \, g \, b \)))

(defn badge [colour number x y]
  [:g
   [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                        :fill   (rgb colour)}
                :cx    x
                :cy    y
                :r     14}]
   [:text {:x           x
           :y           (+ y 10)
           :fill        "black"
           :font-size   "28"
           :text-anchor "middle"}
    (str number)]])

(defn render-item [{:keys [move damage range coin shield label colour money?] :as item}]

  [:svg {:class "tokensvg"
         :view-box (string/join " " [0 0 100 100])}

   [:circle {:style {:stroke "black"
                     :fill (rgb colour)}
             :cx 50
             :cy 50
             :r 49}]

   (cond
     money?
     [:g
      [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                        :fill (rgb colour)}
                :cx 50
                :cy 50
                :r 40}]
      [:text {:x           50
              :y           (+ 50 30)
              :stroke "rgba(0,0,0,0.25)"
              :fill "black"
              :font-size   "85"
              :text-anchor "middle"}
       coin]]
     (= label "Chocolate Cake")
     [:g                                                    ;cake
      [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                        :fill "none"}
                :cx 50
                :cy 50
                :r 47}]
      [:text {:x           50
              :y           (+ 50 23)
              :fill      "rgba(0,0,0,0.25)"
              ;:fill        "none"
              :font-size   "85"
              :text-anchor "middle"} "♟"]
      [:image {:x         (- 50 25)
               :y         (- 50 50)
               :height    "48"
               :width     "48"
               :xlinkHref "images/cake.png"}]
      [:text {:x           50
              :y           (+ 50 6)
              :fill        "black"
              :font-size   "18"
              :text-anchor "middle"} "Chocolate"]
      [:text {:x           50
              :y           (+ 50 30)
              :fill        "black"
              :font-size   "18"
              :text-anchor "middle"} "Cake"]]
     :else
     [:g
      [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                        :fill "none"}
                :cx 50
                :cy 50
                :r 47}]
      [:text {:x           50
              :y           (+ 50 23)
              :fill      "rgba(0,0,0,0.25)"
              ;:fill        "none"
              :font-size   "85"
              :text-anchor "middle"} "♟"]
      [:text {:x           50
              :y           (+ 50 6)
              :fill        "black"
              :font-size   "16"
              :text-anchor "middle"} label]
      (let [badge-v 23 badge-h 17]
        [:g
         (when move (badge [0 255 0] move (if coin (- 50 badge-h) 50) (- 50 badge-v)))
         (when coin (badge [255 255 0] coin (if move (+ 50 badge-h) 50) (- 50 badge-v)))
         (when damage (badge [255 55 0] damage (if range (- 50 badge-h) 50) (+ 50 badge-v)))
         (when range  (badge [255 125 50] range (if damage (+ 50 badge-h) 50) (+ 50 badge-v)))
         (when shield (badge [50 200 255] shield 50 (+ 50 badge-v)))])])])

(defn add-syms [{:keys [move damage range coin shield] :as item}]
  (let [syms (remove nil? [(when move [:span {:class "badge"
                                               :key   "m"
                                               :style {:background-color "rgb(0,255,0)"}}
                                       [:span {:class "badgecontent"} (str move)]])
                            (when damage [:span {:class "badge"
                                                 :key   "d"
                                                 :style {:background-color "rgb(255,55,0)"}}
                                          [:span {:class "badgecontent"} (str damage)]])
                            (when range [:span {:class "badge"
                                                :key   "r"
                                                :style {:background-color "rgb(255, 125, 0)"}}
                                         [:span {:class "badgecontent"} (str range)]])
                            (when coin [:span {:class "badge"
                                               :key   "c"
                                               :style {:background-color "rgb(255,225,0)"}}
                                        [:span {:class "badgecontent"} (str coin)]])
                            (when shield [:span {:class "badge"
                                                :key   "t"
                                                :style {:background-color "rgb(0,200,250)"}}
                                          [:span {:class "badgecontent"} (str shield)]])])]
    (if (empty? syms)
      item
      (assoc item :syms [:span syms]))))

(def max-coins 4)
(defn make-money [amount price]
  (add-syms {:label  (if (= 1 amount) "1 Coin" (str amount " Coins"))
             :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
             :coin   amount
             :money? true
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
        ;character-symbol (symbol character-label)
        ]
    (prepare-item {:label label                             ;character-label
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

(defonce state-history (atom '()))

;; normally defonce... - do this properly!!!
(defonce app-state (add-watch (atom {:text       "Knights of Swamp Castle"
                                     :uniqueness (uniqueness)
                                     :colour     "rgb(175,175,175)"
                                     :hand-count 0
                                     :hand-size  4
                                     :draw       (shuffle [coin-a coin-a coin-a coin-a])
                                     :hand       []
                                     :played     []
                                     :discard    []
                                     :trashed    []
                                     :log        '()})
                              :history
                              (fn [_ _ old _]
                                (swap! state-history conj old))))

(defn undo-state-change []
  (let [[prev & history] @state-history]
    (when prev
      (reset! app-state prev)
      (reset! state-history history))))



(defn draw-tokens [n draw hand discard]
  (cond
    (zero? n) [draw hand discard]
    (and (empty? draw) (empty? discard)) [draw hand discard]
    (empty? draw) (draw-tokens n (shuffle discard) hand [])
    :else (draw-tokens (dec n) (rest draw) (conj hand (first draw)) discard)))

(defn draw-to-hand
  ([{:keys [hand-size draw hand played discard hand-count log] :as state} draw-type]
   (let [[n hand' played' discard' hand-count'] (case draw-type
                                          :hand [hand-size [] [] (concat discard played hand) (inc hand-count)]
                                          :token [1 hand played discard hand-count])
         [new-draw new-hand new-discard] (draw-tokens n draw hand' discard')]
     (assoc state :draw new-draw
                  :hand (vec (remove :money? new-hand))
                  :played (vec (concat played' (filter :money? new-hand)))
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

;(defn trash-from-hand [{:keys [hand trash log] :as state} item]
;  (let [like-item (filter #(= % item) hand)
;        not-item (remove #(= % item) hand)]
;    (assoc state :trash (vec (conj trash item))
;                 :hand (vec (sort-by (juxt :category :value)
;                                     (concat not-item (rest like-item))))
;                 :log (conj log (str "Trashed: " (:label item))))))

(defn play [{:keys [hand played log] :as state} hand-index]
  (-> state
      (assoc :played (vec (conj played (nth hand hand-index)))
             :hand (vec (concat (take hand-index hand) (drop (inc hand-index) hand)))
             :log (conj log (str "Played: " (:label (nth hand hand-index)))))
      (draw-to-hand :token)))

(defn trash-from-hand [{:keys [hand trashed log] :as state} hand-index]
  (assoc state :trashed (vec (conj trashed (nth hand hand-index)))
               :hand (vec (concat (take hand-index hand) (drop (inc hand-index) hand)))
               :log (conj log (str "Trashed: " (:label (nth hand hand-index))))))

(defn trash-from-played [{:keys [played trashed log] :as state} played-index]
  (assoc state :trashed (vec (conj trashed (nth played played-index)))
               :played (vec (concat (take played-index played) (drop (inc played-index) played)))
               :log (conj log (str "Trashed: " (:label (nth played played-index))))))

;(defn untrash-to-hand [{:keys [hand trash log] :as state} item]
;  (let [like-item (filter #(= % item) trash)
;        not-item (remove #(= % item) trash)]
;    (assoc state :hand (vec (sort-by (juxt :category :value) (conj hand item)))
;                 :trash (vec (concat not-item (rest like-item)))
;                 :log (conj log (str "Un-trashed: " (:label item))))))

(defn sum-categories [m {:keys [category value]}]
  (update m category (fnil + 0) value))

(defn hello-world []

  (let [{:keys [text colour uniqueness hand-size draw hand played discard hand-count trashed trash-mode log]} @app-state]

    [:div

     {:id "mounteddiv"
      ;:on-key-down (fn [e]
      ;               (println "key press" (.-charCode e))
      ;               (if (= 13 (.-charCode e))
      ;                 (println "ENTER")
      ;                 (println "NOT ENTER")))
      :tabIndex 1
      :style {;:border (str "3px solid " colour)
              ;:border-top (str "20px solid " colour)
              :padding "0px"
              :margin  "0px"}}

     [:div
      {:style {:background-color colour}}
      (str text " (Iteration " iteration ") ")
      [:input
       {:type "text" :placeholder "Enter your name..."}]
      [:select {:defaultValue "rgb(175,175,175)"
                :on-change    #(swap! app-state assoc :colour (.-value (.-target %)))}
       [:option {:value "rgb(175,175,175)" :disabled true} "Pick a colour"]
       [:option {:value "rgb(255,0,0)"} "red"]
       [:option {:value "rgb(255,255,0)"} "yellow"]
       [:option {:value "rgb(0,255,0)"} "green"]
       [:option {:value "rgb(100,120,255)"} "blue"]
       [:option {:value "rgb(200,200,200)"} "white"]
       [:option {:value "rgb(50,50,50)"} "black"]]]


     [:div
     [:input {:type :button
              :class "drawbutton"
              :value "Draw Hand"
              :on-click #(swap! app-state draw-to-hand :hand)}]
     ;"\u00A0"
     ;[:input {:type :button
     ;         :value (str "Draw 1")
     ;         :on-click #(swap! app-state draw-to-hand :token)
     ;         :style {:color :green
     ;                 :font-size "125%"
     ;                 :background-color :lightgreen}}]


      [:span {:class "h-spaced"} (str " Hand #: " hand-count)]
      [:span {:class "h-spaced"} (str " Draw pile: " (count draw))]
      [:span {:class "h-spaced"} (str " Discard pile: " (count discard))]
      [:span {:class "h-spaced"} (str " Total deck: " (+ (count discard) (count hand) (count draw)))]]

     [:div {:class "section"}
      [:div {:class "sectionback"}]
      [:div {:class "sectionlabel"}
       "Hand"]
      (for [[index item] (map-indexed vector hand)]
        [:div (merge {:class    "token"
                      :key      (str "token-" hand-count "-" index)
                      :style    {:cursor "pointer"}
                      :on-click #(swap! app-state play index)}
                     (when trash-mode
                       {:on-click #(swap! app-state trash-from-hand index)
                        :style    {:cursor "crosshair"}}))
         (render-item item)])

      ]
     [:div {:class "section"}
      [:div {:class "sectionback"}]
      [:div {:class "sectionlabel"}
       "Played"]
      (for [[index item] (map-indexed vector played)]
        [:div (merge {:class "token"
                      :key   (str "token-" hand-count "-" index)
                      :style {:cursor "default"}}
                     (when trash-mode
                       {:on-click #(swap! app-state trash-from-played index)
                        :style    {:cursor "crosshair"}}))
         (render-item item)])]

     (when trash-mode
       [:div {:class "trash"}
        [:span "Trash"]
        (for [[index item] (map-indexed vector trashed)]
          [:div {:class "token"
                 :key   (str "token-" index)}
           (render-item item)])])



     ;[:br]

     ;[:input {:type :button
     ;         :value (str "Draw " hand-size)
     ;         :on-click #(swap! app-state draw-to-hand :hand)
     ;         :style {:color :green
     ;                 :font-size "150%"
     ;                 :background-color :lightgreen}}]
     ;"\u00A0"
     ;[:input {:type :button
     ;         :value (str "Draw 1")
     ;         :on-click #(swap! app-state draw-to-hand :token)
     ;         :style {:color :green
     ;                 :font-size "125%"
     ;                 :background-color :lightgreen}}]


     [:div {:style {:background-color colour}} (str "Buy " (reduce + (remove nil? (map :coin played))))]

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
  (reagent/render-component [hello-world] element)
  (println (dom/getElement "mounteddiv"))
  (events/listen (dom/getElement "mounteddiv") (.-KEYDOWN events/EventType)
                 (fn [e]
                   ;(println (.-keyCode e))
                   (when (= 18 (.-keyCode e))
                     (swap! app-state assoc :trash-mode true))))
  (events/listen (dom/getElement "mounteddiv") (.-KEYUP events/EventType)
                 (fn [e]
                   (when (= 18 (.-keyCode e))
                     (swap! app-state assoc :trash-mode false)))))

(when-let [element (. js/document (getElementById "board"))]
  (reagent/render-component [board/render-board] element))

(when-let [element (. js/document (getElementById "card"))]
  (reagent/render-component [card/render-card] element))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
