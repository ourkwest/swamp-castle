(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.events :as events]
            [domination.board :as board]
            [domination.card :as card]))

(enable-console-print!)

(def iteration 11)

(defn refocus-soon []
  (js/setTimeout #(.focus (dom/getElement "mounteddiv")) 200))

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

(defn render-item
  ([item] (render-item item nil nil))
  ([{:keys [move damage range coin shield label colour money?] :as item} price max-price]

   [:svg {:class    "tokensvg"
          :view-box (string/join " " [0 0 100 100])}

    [:circle {:style {:stroke "black"
                      :fill   (rgb colour)}
              :cx    50
              :cy    50
              :r     49}]
    (if money?
      [:g
       [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                         :fill   (rgb colour)}
                 :cx    50
                 :cy    50
                 :r     40}]
       [:text {:x           50
               :y           (+ 50 30)
               :stroke      "rgba(0,0,0,0.25)"
               :fill        "black"
               :font-size   "85"
               :text-anchor "middle"}
        coin]]
      [:g
       [:circle {:style {:stroke "rgba(0,0,0,0.25)"
                         :fill   "none"}
                 :cx    50
                 :cy    50
                 :r     47}]
       [:text {:x           50
               :y           (+ 50 23)
               :fill        "rgba(0,0,0,0.25)"
               :stroke      "rgba(0,0,0,0.75)"
               :font-size   "85"
               :text-anchor "middle"} "♟"]

       (if (= label "Chocolate Cake")
         [:g
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
         [:g
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
             (when range (badge [255 125 50] range (if damage (+ 50 badge-h) 50) (+ 50 badge-v)))
             (when shield (badge [50 200 255] shield 50 (+ 50 badge-v)))])])])

    (when price
      (let [affordable?
            true
            ;(<= price max-price)
            ]
        [:g
         [:circle {:style {;:stroke "black"
                           :fill (if affordable? (rgb [155 255 0]) (rgb [55 55 55]))}
                   :cx    85
                   :cy    85
                   :r     15}]

         [:text {:x           85
                 :y           (+ 85 (if (<= 10 price) 8 10))
                 :fill        (if affordable? (rgb [0 0 0]) (rgb [155 155 155]))
                 :font-size   (if (<= 10 price) "22" "28")
                 :text-anchor "middle"}
          (str price)]]))]))

(def max-coins 4)
(defn make-money [amount price]
  {:label  (if (= 1 amount) "1 Coin" (str amount " Coins"))
   :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
   :coin   amount
   :money? true
   :price  price})

(def coin-a (make-money 1 0))
(def coin-b (make-money 2 4))
(def coin-c (make-money 4 8))

(defn character [label move damage coin shield range price colour desc]
  {:label label
   :move move
   :damage damage
   :shield shield
   :coin coin
   :range range
   :price price
   :colour (or colour [150 200 255])
   :desc desc})

(def characters
  [
;                               Move  Dmg.  Coin  Shield Range Price Color Description

   (character "Farmer"          1     nil   1     nil    nil   2     nil   nil)
   (character "Horse Rider"     3     nil   nil   nil    nil   3     nil   nil)
   (character "Brigand"         2     1     1     nil    nil   4     nil   nil)
   (character "Archer"          2     1     nil   nil    4     5     nil   nil)
   (character "Blacksmith"      nil   nil   nil   1      nil   6     nil   nil)
   (character "Bezerker"        2     3     nil   nil    nil   7     nil   nil)
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

;(defn uniqueness []
;  [(int (rand 255)) (int (rand 255)) (int (rand 255))])

(defonce state-history (atom '()))

;; normally defonce... - do this properly!!!
(defonce app-state (add-watch (atom {:text       "Knights of Swamp Castle"
                                     ;:uniqueness (uniqueness)
                                     :colour     "rgb(200,200,200)"
                                     :hand-size  4
                                     :trash-mode false
                                     :log        '()

                                     :hand-no    0
                                     :draw       (shuffle [coin-a coin-a coin-a coin-a])
                                     :hand       []
                                     :played     []
                                     :discard    []
                                     :trashed    []})
                              :history
                              (fn [_ _ old new]
                                (when (not= (select-keys old [:hand-no :draw :hand :played :discard :trashed])
                                            (select-keys new [:hand-no :draw :hand :played :discard :trashed]))
                                  (swap! state-history conj old)))))

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
  ([{:keys [hand-size draw hand played discard hand-no log] :as state} draw-type]
   (let [[n hand' played' discard' hand-no'] (case draw-type
                                          :hand [hand-size [] [] (concat discard played hand) (inc hand-no)]
                                          :token [1 hand played discard hand-no])
         [new-draw new-hand new-discard] (draw-tokens n draw hand' discard')]
     (assoc state :draw new-draw
                  :hand (vec (remove :money? new-hand))
                  :played (vec (concat played' (filter :money? new-hand)))
                  :discard new-discard
                  :hand-no hand-no'
                  :log (conj log (str "Drew: " (string/join ", " (map :label (take-last n new-hand)))))))))

(defn buy [item]
  (swap! app-state #(-> %
                        (update :discard conj item)
                        (update :log conj (str "Bought " (:label item))))))

(defn button-like [f]
  {:on-click    f
   :on-key-down #(do
                   (when (#{13 32} (.-keyCode %)) (f))
                   (.focus (dom/getElement "mounteddiv")))})

(defn buy-button [item max-price index]
  [:div (merge {:class    "token"
                :key      (str "buy-token-" (:label item))
                :style    {:cursor "pointer"}
                :tabIndex (+ 300 index)}
               (button-like #(buy item)))
   (render-item item (:price item) max-price)])

(defn trash-from-hand [{:keys [hand trashed log] :as state} hand-index]
  (refocus-soon)
  (assoc state :trashed (vec (conj trashed (nth hand hand-index)))
               :hand (vec (concat (take hand-index hand) (drop (inc hand-index) hand)))
               :log (conj log (str "Trashed: " (:label (nth hand hand-index))))))

(defn play [{:keys [hand played log] :as state} hand-index]
  (refocus-soon)
  (let [item (nth hand hand-index)]
    (if (-> item :label (= "Chocolate Cake"))
      (-> state
          (assoc :log (conj log "Played Chocolate Cake :-D "))
          (trash-from-hand hand-index)
          (draw-to-hand :token))
      (-> state
          (assoc :played (vec (conj played item))
                 :hand (vec (concat (take hand-index hand) (drop (inc hand-index) hand)))
                 :log (conj log (str "Played: " (:label item))))
          (draw-to-hand :token)))))

(defn trash-from-played [{:keys [played trashed log] :as state} played-index]
  (refocus-soon)
  (assoc state :trashed (vec (conj trashed (nth played played-index)))
               :played (vec (concat (take played-index played) (drop (inc played-index) played)))
               :log (conj log (str "Trashed: " (:label (nth played played-index))))))

(defn hello-world []

  (let [{:keys [text colour hand-size draw hand played discard hand-no trashed trash-mode log]} @app-state
        to-spend (reduce + (remove nil? (map :coin played)))]

    [:div

     {:id "mounteddiv"
      :tabIndex 1
      :style {:padding "0px"
              :margin  "0px"}}

     [:div
      {:class "widelabel"
       :style {:background-color colour}}
      [:input
       {:type "text" :placeholder "Enter your name..."}]
      [:select {:defaultValue "rgb(200,200,201)"
                :on-change    #(swap! app-state assoc :colour (.-value (.-target %)))}
       [:option {:value "rgb(200,200,201)" :disabled true} "Pick a colour"]
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

      (let [draw-count (count draw)
            discard-count (count discard)
            ;deck-count (+ discard-count (count hand) (count played) draw-count)
            deck-str (string/join (concat (repeat discard-count \|)
                                          [\<]
                                          (repeat (+ (count hand) (count played)) \|)
                                          [\>]
                                          (repeat draw-count \|)
                                          ))]
        [:span
         [:span {:class "h-spaced"} (str "Hand #: " hand-no)]

         [:span {:class "h-spaced"}
          [:span "Deck: "]
          [:span {:class "flashlabel"
                  :key   (str "deck-" deck-str)}
           deck-str]]

         ;[:span {:class "h-spaced flashlabel"
         ;        :key   (str "draw-pile-count-" draw-count)} (string/join (cons "Draw pile: " (repeat draw-count \|)))]
         ;[:span {:class "h-spaced flashlabel"
         ;        :key   (str "discard-pile-count-" discard-count)} (string/join (cons "Discard pile: " (repeat discard-count \|)))]
         ;[:span {:class "h-spaced flashlabel"
         ;        :key   (str "deck-count-" deck-count)} (string/join (cons "Total deck: " (repeat deck-count \|)))]
         ])]

     [:div {:class "section"}
      [:div {:class "sectionback"}]
      [:div {:class "sectionlabel"}
       "Hand"]
      (for [[index item] (map-indexed vector hand)]
        [:div (merge {:class    "token"
                      :key      (str "token-" hand-no "-" index)
                      :style    {:cursor (if trash-mode "crosshair" "pointer")}
                      :tabIndex (+ 100 index)}
                     (button-like
                       (if trash-mode
                         #(swap! app-state trash-from-hand index)
                         #(swap! app-state play index))))
         (render-item item)])]
     [:div {:class "section"}
      [:div {:class "sectionback"}]
      [:div {:class "sectionlabel"}
       "Played"]
      (for [[index item] (map-indexed vector played)]
        [:div (merge {:class "token"
                      :key   (str "token-" hand-no "-" index)
                      :style {:cursor "default"}}
                     (when (and trash-mode (:money? item))
                       (merge
                         (button-like #(swap! app-state trash-from-played index))
                         {:style    {:cursor "crosshair"}
                          :tabIndex (+ 200 index)}))
                     )
         (render-item item)])]

     (when trash-mode
       [:div {:class "trash"}
        [:span "Trash"]
        (for [[index item] (map-indexed vector trashed)]
          [:div {:class "token"
                 :key   (str "token-" index)}
           (render-item item)])])

     [:div {:class "widelabel"
            :style {:background-color colour}}
      (str "Buy (" to-spend " to spend)")]

     [:div {:class "section"}
      [:div {:class "sectionback"}]
      [:div {:class "sectionlabel"}
       "Buy"]
      (for [[index item] (map-indexed vector (sort-by :price (concat characters [coin-a coin-b coin-c])))]
        (buy-button item to-spend index))]

     [:input {:type :button
              :class "drawbutton"
              :tabIndex 1000
              :value "Undo"
              :on-click undo-state-change}]

     [:span {:class "badge-label" :style {:background-color (rgb [0 255 0])}} "Movement"]
     [:span {:class "badge-label" :style {:background-color (rgb [255 255 0])}} "Spend"]
     [:span {:class "badge-label" :style {:background-color (rgb [255 55 0])}} "Damage"]
     [:span {:class "badge-label" :style {:background-color (rgb [255 125 50])}} "Range"]
     [:span {:class "badge-label" :style {:background-color (rgb [50 200 255])}} "Shield"]
     [:span {:class "badge-label" :style {:background-color (rgb [155 255 0])}} "Cost to Buy"]

     [:div
      {:style {:font-size "50%"}}
      (for [[id msg] (map-indexed vector log)]
        [:span {:key (str "log-" id)} msg [:br]])]


     [:div
      {:class "widelabel"
       :style {:background-color colour}}
      (str text " (Iteration " iteration ") ")]

     ]))

(when-let [element (. js/document (getElementById "app"))]
  (reagent/render-component [hello-world] element)
  ;(println (dom/getElement "mounteddiv"))
  (events/listen (dom/getElement "mounteddiv") (.-KEYDOWN events/EventType)
                 (fn [e]
                   ;(println (.-keyCode e))
                   (when (= 18 (.-keyCode e))
                     (swap! app-state assoc :trash-mode true))))
  (events/listen (dom/getElement "mounteddiv") (.-KEYUP events/EventType)
                 (fn [e]
                   (when (= 18 (.-keyCode e))
                     (swap! app-state assoc :trash-mode false))))
  (refocus-soon))

(when-let [element (. js/document (getElementById "board"))]
  (reagent/render-component [board/render-board] element))

(when-let [element (. js/document (getElementById "card"))]
  (reagent/render-component [card/render-card] element))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


; TODO:
;  tidy up dead code

