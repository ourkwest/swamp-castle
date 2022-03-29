(ns board-game.images.instructions
  (:require
    [clj-pdf.core :as pdf]
    [board-game.support.util :as util]
    [board-game.support.draw :as draw]
    [board-game.support.data :as data]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [board-game.support.stone :as stone]
    [board-game.support.symbol :as symbol])
  (:import
    [java.awt Color RenderingHints Graphics2D]
    (javax.imageio ImageIO)))



(def minion [:chunk.special.minion "Villager"])
(def minions [:chunk.special.minion "Villagers"])
(def shield [:chunk.special.shield "Shield"])
(def shields [:chunk.special.shield "Shields"])
(def vp [:chunk.special.vp "Victory Point"])
(def vps [:chunk.special.vp "Victory Points"])
(def token [:chunk.special.token "Token"])
(def tokens [:chunk.special.token "Tokens"])
(def bonus-token [:chunk.special.token "Bonus Token"])
(def bonus-tokens [:chunk.special.token "Bonus Tokens"])
(def draw-bag [:chunk.special.bag "Draw Bag"])
(def draw-bags [:chunk.special.bag "Draw Bags"])
(def discard-bag [:chunk.special.bag "Discard Bag"])
(def discard-bags [:chunk.special.bag "Discard Bags"])
(def cake [:chunk.special.cake "Chocolate Cake"])
(def midden [:chunk.special.midden "Midden"])
(def bronze [:chunk.special.bronze "Bronze"])

(def characters data/characters)

(def stylesheet
  {:foo {:color [255 0 0]
         :family :helvetica}
   :bar {:color [0 0 255]
         :family :helvetica}

   :bold {:style :bold}
   :bigger {:style {:size 30}}
   :smaller {:style {:size 12}}
   :emphasis {:styles [:italic :underline]}
   :center {:align :center}
   :special {:style :bold}})

(defn draw-image [^Graphics2D g filename x y scale]
  (let [image (ImageIO/read (io/file filename))
        width (.getWidth image)
        height (.getHeight image)]
        (.drawImage g image (- x (* width 1/2)) (- y (* height 1/2)) (* width scale) (* height scale) nil)))

(defn text-cell [label text]
  [:cell {:colspan 1
          :leading 14}
   [:heading.smaller [:chunk.bold label]]
   text])

(defn image-cell [image-filename]
  [:cell {} [:image {:scale 35} image-filename]])

(defn cell-center [[_ attrs & content]]
  (into [:cell (assoc attrs :align :center)] content))

(defn cell-right [[_ attrs & content]]
  (into [:cell (assoc attrs :align :right)] content))

(defn alternate-alignment [idx [image-cell text-cell spare-cell :as row]]
  (if (odd? idx)
    [spare-cell (cell-right text-cell) (cell-center image-cell)]
    row))

(defn draw-border [^Graphics2D g]
  (let [x1 60
        x2 1130
        y1 100
        y2 1630
        outset 20]
    (stone/do-instructions g
      (concat
        (stone/h-wall x1 (- y1 outset) (- x2 x1) 10 5 5 5)
        (stone/h-wall x1 (+ y2 #_outset) (- x2 x1) 10 5 5 5)
        (stone/v-wall (- x1 outset) y1 (- y2 y1) 10 7 5 5)
        (stone/v-wall (+ x2 outset) y1 (- y2 y1) 10 7 5 5)
        (stone/rings x1 y1 [0 5] 30 40 0.8 10)
        (stone/rings x1 y2 [0 5] 30 40 0.8 10)
        (stone/rings x2 y1 [0 5] 30 40 0.8 10)
        (stone/rings x2 y2 [0 5] 30 40 0.8 10)))
    (symbol/flag g x1 (- y1 60) 20 1.2)
    (symbol/flag g x1 (- y2 60) 20 1.2)
    (symbol/flag g x2 (- y1 60) 20 1.2)
    (symbol/flag g x2 (- y2 60) 20 1.2)))

(defn render-instructions []
  (pdf/pdf
    [{:title       "Instructions"
      :author      "Rachel K. Westmacott"
      :creator     "Rachel K. Westmacott"
      :orientation :portrait
      :size        :a4
      :subject     "Instructions for a board game"
      :stylesheet  stylesheet
      :font {:encoding :unicode
             :ttf-name "./resources/fonts/ah/AtkinsonHyperlegible-Regular.ttf"}
      :footer {:align :center}}

     [:heading.center.bigger "Cake Walk"]
     [:graphics {:under true
                 :translate [100 100]}
      (fn [^Graphics2D g2d]
        (let [image (ImageIO/read (io/file "./generated/for-instructions/cake.png"))
              scale 1/8]
          (.drawImage g2d image 100 -40 (* (.getWidth image) scale) (* (.getHeight image) scale) nil)
          (.drawImage g2d image 270 -40 (* (.getWidth image) scale) (* (.getHeight image) scale) nil)))]
     [:spacer 3]
     [:paragraph.center util/tagline]
     ;[:spacer 1]
     [:paragraph.center util/copyright-text]
     [:paragraph.center "Font: Atkinson Hyperlegible"]
     [:spacer 3]

     [:heading.center "Setting"]
     [:paragraph
      "The Dark Ages. In the Kingdom of the Britons, the rule of Queen Æcclescone the Mighty has crumbled with her untimely demise. "
      "Her son, the Prince Æcclescrumb waits in the castle for his coronation. "
      "Meanwhile, a mysterious but exquisitely delicious artefact has fallen through a rift in time. "
      "The villagers are calling it \"" cake "\". "
      "Seize the opportunity! "
      "Hire some " minions "! "
      "Grab some " cake " and use it to woo the Prince in the Castle! "]

     [:heading.center "Pieces"]
     [:list
      [:phrase "These instructions"]
      [:phrase "A game board with a map of the terrain approaching Prince Æcclescrumb's castle"]
      [:phrase "A piece card with spaces for unbought " minions " and " shields]
      [:phrase "8 bags to be used as " draw-bags " and " discard-bags]
      [:phrase "20 " minion " pieces (5 in each of 4 colours)"]
      [:phrase "24 " shield " pieces"]
      [:phrase "144 " tokens " in " (count characters) " different types:"
       [:list
        (let [moneys (->> characters (filter :money?) (map :label))]
          [:phrase (count moneys) " money tokens: " (string/join ", " moneys)])
        (let [roles (->> characters (remove :money?) (map :label))]
          [:phrase (count roles) " role tokens: " (string/join ", " roles)])]]
      [:phrase "5 " vps]
      [:phrase "5 optional " bonus-tokens]]

     [:graphics {:under true :scale 0.5} draw-border]

     [:graphics {:under true
                 :translate [100 100]}
      (fn [^Graphics2D g2d]
        (let [line-height 16
              y-offset 181.5
              y (fn [lines] (+ y-offset (* line-height lines)))
              x (fn [x n] (+ x (* n 24)))
              token-scale 1/8
              img (fn [x-pos y-pos scale & file-names]
                    (doseq [[idx file-name] (map-indexed vector file-names)]
                      (let [image (ImageIO/read (io/file file-name))]
                        (.drawImage g2d image
                                    (x x-pos idx) y-pos
                                    (* (.getWidth image) scale)
                                    (* (.getHeight image) scale)
                                    nil))))]
          (img 20 (y 6) token-scale "./generated/for-instructions/shield.png")
          (img 113 (y 8) token-scale
               "./generated/for-instructions/token_0.png"
               "./generated/for-instructions/token_1.png"
               "./generated/for-instructions/token_2.png")
          (img 253 (y 9) token-scale
               "./generated/for-instructions/token_3.png"
               "./generated/for-instructions/token_4.png"
               "./generated/for-instructions/token_5.png"
               "./generated/for-instructions/token_6.png"
               "./generated/for-instructions/token_7.png"
               "./generated/for-instructions/token_8.png")
          ;(img 50 (y 10) token-scale "./generated/for-instructions/token_6.png")
          (img 15 (y 10) token-scale "./generated/for-instructions/vp.png")
          (img 55 (y 11) token-scale
               "./generated/for-instructions/bonus_0.png"
               "./generated/for-instructions/bonus_1.png"
               "./generated/for-instructions/bonus_2.png"
               "./generated/for-instructions/bonus_3.png"
               "./generated/for-instructions/bonus_4.png")))]
     [:spacer]

     [:heading.center "Aim"]
     [:paragraph
      "The aim of the game is to acquire the most tokens of the Prince's affection (\"" vps "\"). "
      "These are acquired by having a " minion " on the final row of the board and playing "
      "a " cake " " token " from your hand."]
     [:spacer]

     [:heading.center "Setting up"]
     [:paragraph
      "Choose how many " vps " to play with and place them in a pile. "
      "It is suggested that you use one more " vp " than there are players, but you can vary this for a shorter or longer game. "
      "Note: If you pick the same number of " vps " as there are players then a draw is not unlikely. "]
     [:paragraph
      "Layout the " (count characters) " types of " token " each in their own pile. (Or all in one big pile if you are short on time.) This is the Bank."]
     [:paragraph "Place the coloured player " minions " and " shields " on the piece card. "
      "If there are fewer than 4 players then " [:phrase.emphasis "only use the corresponding number of rows for the " shields]
      " and the appropriate coloured spots for the " minions ". "]
     [:paragraph "Give each player 4 " bronze " " tokens " in a " draw-bag ". "
      "Give each player an empty discard " discard-bag ". "]
     [:paragraph [:phrase.emphasis "Optionally,"] " place a random " bonus-token " in each player's " draw-bag "."]

     [:pagebreak]
     [:graphics {:under true :scale 0.5} draw-border]

     [:spacer]
     [:heading.center "Playing"]
     [:paragraph "The game is played in turns and the player who has brought the most baked goods to the table takes the first turn. "]
     [:spacer]

     [:heading.smaller "Turn Overview"]
     [:paragraph "Each turn is played as follows:"]
     [:list
      [:phrase [:chunk.emphasis "Draw"] " 4 " tokens " from your " draw-bag " into your hand. "]
      [:phrase [:chunk.emphasis "Place"] " up to 1 " token " on each " minion " of yours on the board."]
      [:phrase [:chunk.emphasis "Perform the actions"] " for the " tokens " that you have placed on your " minions ". "]
      [:phrase [:chunk.emphasis "Buy"] " some " tokens ", " shields " and / or " minions ". "]
      [:phrase [:chunk.emphasis "Discard"] " all played and unplayed " tokens " from your hand and all newly acquired " tokens " into your " discard-bag ". "]]

     [:spacer]
     [:heading.smaller "0. Pre-turn checks"]
     [:paragraph "Ensure that your " draw-bag " is on your left and your " discard-bag
      " is on your right and that the previous player has left no " tokens " on the board."]

     [:spacer]
     [:heading.smaller "1. Drawing " tokens]
     [:paragraph
      "Draw 4 " tokens " from your " draw-bag " at random into your hand at the start of your turn. "
      "You will also often draw " tokens " during your turn. "
      "If at any time you need to draw a token and your " draw-bag " is empty, then you immediately swap your " draw-bag
      " and your " discard-bag " and draw from your new " draw-bag ". "
      "If both bags are empty then you cannot draw more " tokens " even if you are entitled to."]

     [:spacer]
     [:heading.smaller "2. Placing " tokens]
     [:paragraph
      "You may place 1 role " token " on each of your " minions " that are on the board. "
      "You may only place role " tokens " on your minions (those marked with a brown mask symbol). "
      "You may not place money " tokens " on your minions. "
      "You may only place " cake " " tokens " on " minions " that are on the final row of the board (those marked with the cake symbol). "
      "For each role " token " that you place you may draw another " token " in accordance with the above rules about drawing. "
      "When you place a " token " you may choose not to draw another " token " if you wish, "
      "such as when you know there's something in your " draw-bag " that might be more useful on the following turn."]

     [:spacer]
     [:heading.smaller "3. Performing " token " actions"]
     [:paragraph "Each of your " minions " on the board may perform the action of the " token " placed on it. "]

     [:spacer]
     [:paragraph {:indent-left 25}
      [:heading.smaller "Moving"]
      [:paragraph
       minions " may not occupy the same hex, nor move through occupied hexes "
       "(even if they both belong to the same player). "
       "Hence, paths may be blocked. "]

      [:paragraph {:size 10}]
      [:heading.smaller "Attacking"]
      [:paragraph
       "When attacking the outcome is determined by the strength of the attack. "
       "Start with the amount of damage dealt by the role " token " making the attack. "
       "Reduce the damage by 1 if the defending " minion " is on a tree hex bearing a " shield " symbol. "
       "The defending player may then choose to return any number of " shields " that they have to the bank. "
       "For each shield that they choose to return the damage is further reduced by 1. "
       "If the remaining damage is greater than 0 then the defending " minion " is defeated and must be returned to the bank. "]

      [:paragraph
       "When returning " shields " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the number of " shield " spot rows corresponding to the number of players. "
       "When returning " minions " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the spots of the correct player's colour. "]

      [:spacer]
      [:heading.smaller "Moving & Attacking"]
      [:paragraph
       "If the " token " permits the " minion " movement as well as attack, "
       "you may perform these in either order, "
       "but you cannot split your movement or attack into two parts. "
       "e.g. If a " token " grants up to 3 movement and 2 attack"
       " you may move the " minion " by 1 and then attack causing 2 damage, "
       " but you may not move 1 then attack and then move the remaining 2. "
       "If you choose to attack first and defeat a " minion ", you may move onto or through the hex where the defeated " minion " was."]

      [:spacer]
      [:heading.smaller "Attacking at Range"]
      [:paragraph "If a " token " has a ranged attack, count the range as if it were movement. "
       "e.g. When an Archer attacks, imagine an arrow starting on the Archer's hex and "
       "moving up to 3 hexes to reach it's target."]

      [:spacer]
      [:heading.smaller "Chocolate Cake"]
      [:paragraph
       "Your " minion " is admitted to the castle to seduce Prince Æcclescrumb with a slice of delicious & moist Chocolate Cake. "]
      [:paragraph
       "Your " minion " must be on a 'Cake' hex to perform this action. "
       "Your " minion " is completely retired from the game; they are removed from the board and can no longer be bought. "
       "Your " cake " " token " is returned to the bank. "
       "You receive 1 " vp "."]]

     [:spacer]
     [:heading.smaller "4. Buying"]
     [:paragraph
      "At the end of your turn, you may buy as many things as you can afford. "
      "You can afford to buy things up to the value of all the money " tokens " in your hand "
      "plus all the money values of the role " tokens " you have played this turn. "
      "Do not count the money values on role " tokens " that you have not played this turn. "]
     [:paragraph
      "Bought " tokens " are placed in your " discard-bag ". "
      "Bought " minions " are immediately placed on any unoccupied starting hex marked with an empty mask symbol. "
      "Bought " shields " are placed in a pile by the player where all other players can see them. "]

     [:spacer]
     [:heading.smaller "5. Discarding"]
     [:paragraph
      "At the end of your turn place all played and unplayed " tokens " from your hand and all newly acquired "
      tokens " into your " discard-bag ". "]
     [:paragraph
      "If at the end of your turn any of your " minions " are on one of the " (->> data/terrain-map flatten (filter #{:midd :trmd}) count) " central " midden
      " hexes (marked with a pile of discarded tokens), then you may choose to return any "
      "number of the discarded " tokens " to the bank instead of to your " discard-bag
      ", regardless of whether you have used them this turn. "]

     [:spacer]
     [:heading.center "Bonus Tokens"]
     [:paragraph "You may choose to play with the bonus " tokens " added randomly to each players starting " tokens ". "
      "When a bonus " token " is drawn the player may immediately draw another token. "
      "Bonus " tokens " do not need to be placed on a minion to be used. "
      "Bonus " tokens " cannot be bought, but can be discarded to the " midden ". "
      "Bonus " tokens " are discarded to the " discard-bag " just like other " tokens "."]

     [:graphics {:under true :scale 0.5} draw-border]
     [:pagebreak]
     [:graphics {:under true :scale 0.5} draw-border]

     [:spacer]
     [:heading.center "Tokens"]
     [:spacer]

     (into [:table {:border      false
                    :cell-border false
                    :widths      [15 35 15 35]
                    :spacing     2}]
           (->> (concat
                  (for [[idx c] (->> (map-indexed vector characters)
                                     (sort-by #(= "Chocolate Cake" (:label (second %)))))]
                    (let [desc [:phrase
                                (when (= "Chocolate Cake" (:label c))
                                  [:phrase "Sacrifice this " token " and " minion " for a " vp ". "])
                                (condp = (:move c)
                                  nil ""
                                  1 [:phrase "You may move this " minion " to an adjacent hex. "]
                                  [:phrase "You may move this " minion " up to " [:phrase.bold (:move c) " hexes"] ". "])
                                (when (:coin c) [:phrase "You have " [:chunk.bold (:coin c)] " more to spend at turn's end. "])
                                (when (:shield c) [:phrase "You may take the next " (:shield c) " cheapest " shields " from the piece card from any row. "])
                                (when (:damage c)
                                  (if (:range c)
                                    [:phrase "You may attack one other " minion " up to " [:phrase.bold (:range c) " hexes"] " away "
                                     "doing " [:chunk.bold (:damage c)] " damage."]
                                    [:phrase "You may attack one other " minion " on an adjacent hex "
                                     "doing " [:chunk.bold (:damage c)] " damage."]))]]
                      [(image-cell (str "./generated/for-instructions/token_" idx ".png"))
                       (text-cell (:label c) desc)]))
                  [[(image-cell "./generated/for-instructions/bonus_0.png")
                    (text-cell "Movement Bonus" "One minion may move 1 extra hex this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_1.png")
                    (text-cell "Spend Bonus" "You have 1 extra spend at the end of your turn.")]
                   [(image-cell "./generated/for-instructions/bonus_2.png")
                    (text-cell "Range Bonus" "One minion may attack with 1 extra range this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_3.png")
                    (text-cell "Damage Bonus" "One minion may attack with 1 extra damage this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_4.png")
                    (text-cell "Shield Bonus" "You may take 1 free shield from the bank this turn.")]])
                (apply concat)
                (partition-all 4)))


     #_(doto (into [:table {
                            :border false
                            :cell-border false
                            :widths      [15 70 15]
                            :spacing     -17 ; or shrink the margins?
                            ;:padding 1
                          ;:no-split-cells? true
                          }]
                 (->> (concat
                        (for [[idx c] (->> (map-indexed vector characters)
                                           ;(sort-by (juxt :money? (comp :price second)))
                                           (sort-by #(= "Chocolate Cake" (:label (second %))))
                                           #_(take 2))
                              ;:when (not (:money? c))
                              ]
                          (let [desc [:phrase
                                      (when (= "Chocolate Cake" (:label c))
                                        [:phrase "Sacrifice this " token " and " minion " for a " vp ". "])
                                      (condp = (:move c)
                                        nil ""
                                        1 [:phrase "You may move this " minion " to an adjacent hex. "]
                                        [:phrase "You may move this " minion " up to " [:phrase.bold (:move c) " hexes"] ". "])
                                      (when (:coin c) [:phrase "You have " [:chunk.bold (:coin c)] " more to spend at turn's end. "])
                                      (when (:shield c) [:phrase "You may take the next " (:shield c) " cheapest " shields " from the piece card from any row. "])
                                      (when (:damage c)
                                        (if (:range c)
                                          [:phrase "You may attack one other " minion " up to " [:phrase.bold (:range c) " hexes"] " away "
                                           "doing " [:chunk.bold (:damage c)] " damage."]
                                          [:phrase "You may attack one other " minion " on an adjacent hex "
                                           "doing " [:chunk.bold (:damage c)] " damage."]))]]
                            [;[:cell {:colspan 1} [:image {:scale 25} (str "./generated/for-instructions/token_" idx ".png")]]
                             (image-cell (str "./generated/for-instructions/token_" idx ".png"))
                             (text-cell (:label c) desc)
                             [:cell ""]]))
                        [[(image-cell "./generated/for-instructions/bonus_0.png")
                          (text-cell "Movement Bonus" "One minion may move 1 extra hex this turn.")
                          [:cell ""]]
                         [(image-cell "./generated/for-instructions/bonus_1.png")
                          (text-cell "Spend Bonus" "You have 1 extra spend at the end of your turn.")
                          [:cell ""]]
                         [(image-cell "./generated/for-instructions/bonus_2.png")
                          (text-cell "Range Bonus" "One minion may attack with 1 extra range this turn.")
                          [:cell ""]]
                         [(image-cell "./generated/for-instructions/bonus_3.png")
                          (text-cell "Damage Bonus" "One minion may attack with 1 extra damage this turn.")
                          [:cell ""]]
                         [(image-cell "./generated/for-instructions/bonus_4.png")
                          (text-cell "Shield Bonus" "You may take 1 free shield from the bank this turn.")
                          [:cell ""]]])
                      (map-indexed alternate-alignment)
                      (cons ["" [:cell {:align :center} [:heading "Tokens"] [:spacer 2]] ""])))
       clojure.pprint/pprint)
     ]

    "generated/instructions.pdf")
  )

(render-instructions)

#_(defn render-instructions [image]
    (let [g (.getGraphics image)
        width (.getWidth image)
        height (.getHeight image)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 width height)

    (draw/text g (draw/text-style 20 Color/BLACK) "Cake Walk" 50 50)

    ))

#_(do ; test block

  (require '[see.core :as see])
  (import '[java.awt.image BufferedImage])

  (def width (util/mm->px util/a4-height-mm))
  (def height (util/mm->px util/a4-width-mm))

  (defonce ^BufferedImage image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  (defonce refresh-fn (see/see image :only-draw-when-updated? true))


  (render-instructions image)

  (refresh-fn)

  )

(comment
  ([[:image {:scale 25} ./generated/for-instructions/token_0.png]
    [:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Bronze]]
     [:phrase nil  [:phrase You have  [:chunk.bold 1]  more to spend at turn's end. ] nil nil]]]
   [[:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Silver]]
     [:phrase nil  [:phrase You have  [:chunk.bold 2]  more to spend at turn's end. ] nil nil]]
    [:image {:scale 25} ./generated/for-instructions/token_1.png]]
   [[:image {:scale 25} ./generated/for-instructions/token_2.png]
    [:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Gold]]
     [:phrase nil  [:phrase You have  [:chunk.bold 3]  more to spend at turn's end. ] nil nil]] ]
   [[:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Farmer]]
     [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  to an adjacent hex. ] [:phrase You have  [:chunk.bold 2]  more to spend at turn's end. ] nil nil]]
    [:image {:scale 25} ./generated/for-instructions/token_3.png] ]
   [[:image {:scale 25} ./generated/for-instructions/token_4.png]
    [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Scout]]
     [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 4  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  on an adjacent hex  doing  [:chunk.bold 1]  damage.]]]]
   [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Archer]]
     [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 2  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  up to  [:phrase.bold 3  hexes]  away  doing  [:chunk.bold 2]  damage.]]]
    [:image {:scale 25} ./generated/for-instructions/token_5.png] ]
   [[:image {:scale 25} ./generated/for-instructions/token_7.png]
    [:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Smithy]]
     [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  to an adjacent hex. ] nil [:phrase You may take the next  2  cheapest  [:chunk.special.shield Shields]  from the piece card from any row. ] nil]] ]
   [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Knight]]
     [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 4  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  on an adjacent hex  doing  [:chunk.bold 3]  damage.]]]
    [:image {:scale 25} ./generated/for-instructions/token_8.png] ]
   [[:image {:scale 25} ./generated/for-instructions/token_6.png]
    [:cell {:leading 14, :colspan 2}
     [:heading.smaller [:chunk.bold Chocolate Cake]]
     [:phrase [:phrase Sacrifice this  [:chunk.special.token Token]  and  [:chunk.special.minion Minion]  for a  [:chunk.special.vp Victory Point] . ]  nil nil nil]]]
   [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may move 1 extra hex this turn.]
    [:image {:scale 25} ./generated/for-instructions/bonus_0.png] ]
   [[:image {:scale 25} ./generated/for-instructions/bonus_1.png]
    [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] You have 1 extra spend at the end of your turn.] ]
   [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may attack with 1 extra range this turn.]
    [:image {:scale 25} ./generated/for-instructions/bonus_2.png] ]
   [[:image {:scale 25} ./generated/for-instructions/bonus_3.png]
    [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may attack with 1 extra damage this turn.] ]
   [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] You may take 1 free shield from the bank this turn.]
    [:image {:scale 25} ./generated/for-instructions/bonus_4.png] ])


  )