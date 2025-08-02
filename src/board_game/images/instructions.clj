(ns board-game.images.instructions
  (:require
    [clj-pdf.core :as pdf]
    [pdf-to-image.core :as to-image]
    [board-game.support.util :as util]
    [board-game.support.draw :as draw]
    [board-game.support.data :as data]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [board-game.support.stone :as stone]
    [board-game.support.symbol :as symbol])
  (:import
    [java.awt Color RenderingHints Graphics2D]
    (javax.imageio ImageIO)
    [java.awt.image BufferedImage]))



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
   :title {:style {:size 25}}
   :section {:style {:size 12}}
   :bigger {:style {:size 16}}
   :smaller {:style {:size 8}}
   :emphasis {:styles [:italic :underline]}
   :center {:align :center}
   :special {:style :bold}})

(defn draw-image [^Graphics2D g filename x y scale]
  (let [image (ImageIO/read (io/file filename))
        width (.getWidth image)
        height (.getHeight image)]
        (.drawImage g image (- x (* width 1/2)) (- y (* height 1/2)) (* width scale) (* height scale) nil)))

(defn token-table [rows]
  (into [:table {:border      false
                 :cell-border false
                 :widths      [10 40]
                 :spacing     -3}]
        rows))

(defn text-cell [label text]
  [:cell {:colspan 1
          :leading 10}
   [:heading.smaller [:chunk.bold label]]
   [:paragraph #_{:leading 10} text]])

(defn image-cell [image-filename]
  [:cell {} [:image {:scale 20} image-filename]])

(defn cell-center [[_ attrs & content]]
  (into [:cell (assoc attrs :align :center)] content))

(defn cell-right [[_ attrs & content]]
  (into [:cell (assoc attrs :align :right)] content))

(defn alternate-alignment [idx [image-cell text-cell spare-cell :as row]]
  (if (odd? idx)
    [spare-cell (cell-right text-cell) (cell-center image-cell)]
    row))

(defn draw-border [^Graphics2D g]
  #_(let [x1 60
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

(defn mm-size "convert from mm to whatever weird intenal format clj-pdf uses" [mm]
  (Math/round (float (* 17/6 mm))))

(def page-width (mm-size 88.9))
(def page-height (mm-size 146))

(def small-spacer [:spacer {:leading 3} 1])
(def medium-spacer [:spacer {:leading 6} 1])

(defn section-heading [text]
  [:heading.center.section {:leading 10} text])

(defn render-instructions []
  (pdf/pdf
    [{:title       "Instructions"
      :author      "Rachel K. Westmacott"
      :creator     "Rachel K. Westmacott"
      :orientation :portrait
      :size        [page-width page-height] ; https://www.boardgamesmaker.com/print/custom-accordion-instructions-booklet-large-size.html
      :left-margin   10
      :right-margin  10
      :top-margin    -30
      :bottom-margin -10
      :subject     "Instructions for a board game"
      :stylesheet  stylesheet
      :font        {:encoding :unicode
                    :ttf-name "./resources/fonts/ah/AtkinsonHyperlegible-Regular.ttf"
                    :size 8}
      :footer      {:align :center}}

     [:heading.center.title "Cake Walk"]
     [:graphics {:under true
                 :translate [100 100]}
      (fn [^Graphics2D g2d]
        (let [image (ImageIO/read (io/file "./generated/for-instructions/cake.png"))
              scale 1/8]
          (.drawImage g2d image -80 -80 (* (.getWidth image) scale) (* (.getHeight image) scale) nil)
          (.drawImage g2d image 106 -80 (* (.getWidth image) scale) (* (.getHeight image) scale) nil)
          ;(.drawImage g2d image 270 -40 (* (.getWidth image) scale) (* (.getHeight image) scale) nil)
          ))]
     [:spacer 1]
     [:paragraph.center [:chunk {:size 8} (subs util/tagline 0 49)]]
     [:paragraph.center [:chunk {:size 8} (subs util/tagline 49)]]
     ;[:spacer 1]
     [:paragraph.center [:chunk {:size 6} util/copyright-text]]
     [:paragraph.center [:chunk {:size 6} (str util/contact " Font: Atkinson Hyperlegible")]]
     small-spacer
     ;small-spacer
     ;small-spacer
     ;small-spacer
     medium-spacer
     medium-spacer
     medium-spacer

     (section-heading "Setting")
     small-spacer
     [:paragraph {:leading 10}
      "The Dark Ages. In the Kingdom of the Britons, the rule of Queen Æcclescone the Mighty has crumbled with her untimely demise. "
      "Her son, the Prince Æcclescrumb waits in the castle for his coronation. "
      "Meanwhile, a mysterious but exquisitely delicious artefact has fallen through a rift in time. "
      "The villagers are calling it \"" cake "\". "
      "Seize the opportunity! "
      "Hire some " minions "! "
      "Grab some " cake " and use it to woo the Prince in the Castle! "]

     medium-spacer
     medium-spacer
     (section-heading "Contents")
     small-spacer
     [:list {:leading 10}
      [:phrase "These instructions"]
      [:phrase "A game board with a map of the terrain approaching Prince Æcclescrumb's castle"]
      [:phrase "A piece card with spaces for unbought " minions " and " shields]
      [:phrase "8 bags to be used as " draw-bags " and " discard-bags]
      [:phrase "16 " minion " pieces (4 in each of 4 colours)"]
      [:phrase "24 " shield " pieces"]
      [:phrase "A large number of " tokens " in " (count characters) " different types:"
       [:list
        (let [moneys (->> characters (filter :money?) (map :label))]
          [:phrase (count moneys) " money tokens: " (string/join ", " moneys)])
        (let [roles (->> characters (remove :money?) (map :label) (map #(string/replace % "Chocolate" "")))]
          [:phrase (count roles) " role tokens: " (string/join ", " roles)])]]
      [:phrase "5 optional " bonus-tokens]]

     #_[:graphics {:under true :scale 0.5} draw-border]

     #_[:graphics {:under true
                 :translate [100 100]}
      (fn [^Graphics2D g2d]
        (let [line-height 16
              y-offset 181.5
              y (fn [lines] (+ y-offset (* line-height (inc lines))))
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
          (img 238 (y 9) token-scale
               "./generated/for-instructions/token_3.png"
               "./generated/for-instructions/token_4.png"
               "./generated/for-instructions/token_5.png"
               "./generated/for-instructions/token_6.png"
               "./generated/for-instructions/token_7.png"
               "./generated/for-instructions/token_8.png")
          ;(img 50 (y 10) token-scale "./generated/for-instructions/token_6.png")
          ;(img 15 (y 10) token-scale "./generated/for-instructions/vp.png")
          (img 55 (y 10) token-scale
               "./generated/for-instructions/bonus_0.png"
               "./generated/for-instructions/bonus_1.png"
               "./generated/for-instructions/bonus_2.png"
               "./generated/for-instructions/bonus_3.png"
               "./generated/for-instructions/bonus_4.png")))]
     ;[:spacer]

     [:pagebreak]
     medium-spacer
     (section-heading "Aim")
     small-spacer
     ;(section-heading "Aim")
     ;small-spacer
     [:paragraph {:leading 10}
      "The aim of the game is to win Prince Æcclescrumb's affection by getting the most " minions " into his castle to woo him with " cake ". "]
     ;[:spacer]

     small-spacer
     small-spacer
     (section-heading "Setting up")
     small-spacer

     [:paragraph {:leading 10}
      "The player who has brought the most baked goods to the table is player 1. Player 2 is to their left, etc."]

     [:paragraph {:leading 10}
      "Choose how many slices of " cake " will be required to win the Prince's heart. If in doubt, play first to 2. "]
     [:paragraph {:leading 10}
      "Layout the " (count characters) " types of " token " where they can easily be reached by all players. This is the Bank."]
     [:paragraph {:leading 10}
      "Place the coloured player " minions " and " shields " on the piece card. "
      "If there are fewer than 4 players then " [:phrase.emphasis "only use the corresponding number of rows for the " shields]
      " and the appropriate coloured spots for the " minions ". "
      "Give player 2 the cheapest shield from the piece card. Give player 3 (if there is one) the next 2 shields. Give player 4 (if there is one) the next 3 shields."]
     [:paragraph {:leading 10}
      "Give each player 4 " bronze " " tokens " in a " draw-bag ". "
      "Give each player an empty " discard-bag ". "]
     [:paragraph {:leading 10}
      [:phrase.emphasis "Optionally,"] " place a random " bonus-token " in each player's " draw-bag "."]

     #_[:graphics {:under true :scale 0.5} draw-border]

     medium-spacer
     (section-heading "Playing")
     small-spacer
     ;[:spacer]
     ;[:heading.center.section "Playing"]
     [:paragraph {:leading 10}
      "The game is played in turns clockwise around the table."]
     [:spacer]

     ;[:heading.smaller "Turn Overview"]
     [:paragraph {:leading 10} "Each turn consists of the following " [:chunk.emphasis "ordered"] " steps:"]
     [:list {:leading  10
             :numbered true}
      [:phrase [:chunk.emphasis "Draw"] " 4 " tokens " from your " draw-bag " into your hand. "]
      [:phrase [:chunk.emphasis "Place"] " up to 1 " token " on each " minion " of yours on the board."]
      [:phrase [:chunk.emphasis "Perform the actions"] " for the " tokens " that you have placed on your " minions ". "]
      [:phrase [:chunk.emphasis "Buy"] " some " tokens ", " shields " and / or " minions ". "]
      [:phrase [:chunk.emphasis "Discard"] " all played and unplayed " tokens " from your hand and all newly acquired " tokens " into your " discard-bag ". "]]

     [:spacer]
     [:heading.smaller "0. Pre-turn checks"]
     [:paragraph {:leading 10}
      "Ensure that your " draw-bag " is on your left and your " discard-bag
      " is on your right and that the previous player has left no " tokens " on the board."]

     [:spacer]
     [:heading.smaller "1. Drawing " tokens]
     [:paragraph {:leading 10}
      "Draw 4 " tokens " from your " draw-bag " at random into your hand at the start of your turn. "
      "You will also often draw " tokens " during your turn. "
      "If at any time you need to draw a token and your " draw-bag " is empty, then you immediately swap your " draw-bag
      " and your " discard-bag " and draw from your new " draw-bag ". "
      "If both bags are empty then you cannot draw more " tokens " even if you are entitled to."]

     [:spacer]
     [:heading.smaller "2. Placing " tokens]
     [:paragraph {:leading 10}
      "You may place 1 role " token " on each of your " minions " that are on the board. "
      "If you have bought " minions " that are not on the board, you may add them to a starting hex on the board as you place " tokens ". "
      "You may only place " [:chunk.emphasis "role"] " " tokens " on your " minions " (those " tokens " marked with a brown mask symbol). "
      "You may not place " [:chunk.emphasis "money"] " " tokens " on your " minions ". "
      "You may only place " cake " " tokens " on " minions " that are adjacent to the entrances of the castle (those marked with the cake symbol). "
      "For each role " token " that you place you may draw another " token " in accordance with the above rules about drawing. "
      "When you place a " token " you may choose " [:chunk.emphasis "not"] " to draw another " token " if you wish, "
      "such as when you know there's something in your " draw-bag " that might be more useful on the following turn."]

     [:spacer]
     [:heading.smaller "3. Performing " token " actions"]
     [:paragraph {:leading 10}
      "Each of your " minions " on the board may perform the action of the " token " placed on it. "
      "See \"" tokens "\" below for details on specific actions."]

     [:spacer 0]
     [:paragraph {:leading 10
                  :indent-left 25}
      [:heading.smaller "3.1 Moving"]
      [:paragraph {:leading 10
                   ;:spacing-before 180
                   ;:spacing-after 150
                   }
       minions " may not occupy the same hex, nor move through occupied hexes "
       "(even if they both belong to the same player). "
       "Hence, paths may be blocked. "]

      [:spacer 0]
      [:heading.smaller "3.2 Attacking"]
      [:paragraph {:leading 10}
       "When attacking, the outcome is determined by the strength of the attack. "
       "Start with the amount of damage dealt by the role " token " making the attack. "
       "Reduce the damage by 1 if the defending " minion " is on a tree hex or starting hex bearing a " shield " symbol. "
       "The defending player may then choose to return any number of " shields " that they have to the piece card. "
       "For each shield that they choose to return the damage is further reduced by 1. "
       "If the remaining damage is greater than 0 then the defending " minion " is defeated and must be returned to the piece card. "]

      [:spacer 0]
      [:paragraph {:leading 10}
       "When returning " shields " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the number of " shield " spot rows corresponding to the number of players. "
       "When returning " minions " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the spots of the correct player's colour. "]

      [:spacer 0]
      [:heading.smaller "3.3 Moving & Attacking"]
      [:paragraph {:leading 10}
       "If the " token " permits the " minion " movement as well as attack, "
       "you may perform these in either order, "
       "but you cannot split your movement or attack into two parts. "
       "e.g. If a " token " grants up to 3 movement and 2 attack"
       " you may move the " minion " by 1 and then attack causing 2 damage, "
       " but you may not move 1 then attack and then move the remaining 2. "
       "If you choose to attack first and defeat a " minion ", you may move onto or through the hex where the defeated " minion " was."]

      [:spacer 0]
      [:heading.smaller "3.4 Attacking at Range"]
      [:paragraph {:leading 10}
       "If a " token " has a ranged attack, count the range as if it were movement. "
       "e.g. When an Archer attacks, imagine an arrow starting on the Archer's hex and "
       "moving up to 3 hexes to reach it's target. Arrows may pass over other " minions ", rivers and walls as if they were not there."]

      [:spacer 0]
      [:heading.smaller "3.5 Chocolate Cake"]
      [:paragraph {:leading 10}
       "Your " minion " is admitted to the castle to seduce Prince Æcclescrumb on your behalf with a slice of delicious"
       " & moist Chocolate Cake. "]
      [:paragraph {:leading 10}
       "Your " minion " must be on a 'Cake' hex to perform this action. "
       "Your " minion " is completely retired from the game; they are placed in the castle at the end of the board and can no longer be bought. "
       "Your " cake " " token " is returned to the bank. "
       "Each time you do this counts towards your victory."]]

     [:spacer]
     [:heading.smaller "4. Buying"]
     [:paragraph {:leading 10}
      "At the end of your turn, you may buy as many things as you can afford. "
      "You can afford to buy things up to the value of all the money " tokens " in your hand "
      "plus all the money values of the role " tokens " you have played this turn. "
      "Do not count the money values on role " tokens " that you have not played this turn. "]
     [:paragraph {:leading 10}
      "Bought " tokens " are placed in your " discard-bag ". "
      "Bought " minions " are placed in front of you. "
      "You may place them at any time during this or any of your later turns onto any unoccupied starting hex marked with "
      "an empty mask symbol. "
      "Bought " shields " are placed in a pile by the player where all other players can see them. "
      "Each player " [:chunk.emphasis "may only hold up to 6 shields"] ", regardless of how they are acquired."]

     [:pagebreak]
     [:heading.smaller "5. Discarding"]
     [:paragraph {:leading 10}
      "At the end of your turn place all played and unplayed " tokens " from your hand and all newly acquired "
      tokens " into your " discard-bag ". "]
     [:paragraph {:leading 10}
      "If at the end of your turn any of your " minions " are on one of the "
      (->> data/terrain-map flatten (filter #{:midd :trmd}) count) " central " midden
      " hexes (marked with a pile of discarded tokens), then you may choose to return some "
      "of the discarded " tokens " to the bank instead of to your " discard-bag
      ", regardless of whether you have used them this turn. "
      [:chunk.emphasis "However"] ", you may only return as many " tokens " to the bank as you have " minions " on the "
      midden " at the end of your turn."]

     ;[:spacer]
     medium-spacer
     (section-heading "Bonus Tokens")
     small-spacer
     [:paragraph {:leading 10}
      "You may choose to play with the bonus " tokens " added randomly to each players starting " tokens ". "
      "When a bonus " token " is drawn the player may immediately draw another token. "
      "Bonus " tokens " do not need to be placed on a minion to be used. "
      "Bonus " tokens " cannot be bought, but can be discarded to the " midden ". "
      "Bonus " tokens " are discarded to the " discard-bag " just like other " tokens "."]

     [:graphics {:under true :scale 0.5} draw-border]
     ;[:pagebreak]
     [:graphics {:under true :scale 0.5} draw-border]

     ;[:spacer]
     medium-spacer
     medium-spacer
     (section-heading tokens)
     ;[:spacer]

     (token-table
       (for [[idx c] (->> (map-indexed vector characters)
                          (sort-by #(= "Chocolate Cake" (:label (second %)))))
             :when (:money? c)]
         [(image-cell (str "./generated/for-instructions/token_" idx ".png"))
          (text-cell (:label c) [:phrase "You have " [:chunk.bold (:coin c)] " more to spend at turn's end. "])]))

     [:pagebreak]

     (token-table
       (for [[idx c] (->> (map-indexed vector characters)
                          (sort-by #(= "Chocolate Cake" (:label (second %)))))
             :when (not (:money? c))]
         (let [desc [:phrase
                     (when (= "Chocolate Cake" (:label c))
                       [:phrase "Sacrifice this " token " and " minion " towards seducing Prince Æcclescrumb. "])
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
            (text-cell (:label c) desc)])))

     [:pagebreak]

     (token-table [[(image-cell "./generated/for-instructions/bonus_0.png")
                    (text-cell "Movement Bonus" "One minion may move 1 extra hex while it is moving this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_1.png")
                    (text-cell "Spend Bonus" "You have 1 extra spend when buying at the end of your turn.")]
                   [(image-cell "./generated/for-instructions/bonus_2.png")
                    (text-cell "Range Bonus" "One minion may attack with 1 extra range while it is attacking this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_3.png")
                    (text-cell "Damage Bonus" "One minion may attack with 1 extra damage while it is attacking this turn.")]
                   [(image-cell "./generated/for-instructions/bonus_4.png")
                    (text-cell "Shield Bonus" "You may take 1 free shield from the piece card when buying at the end of your turn.")]])

     #_(into [:table {:border      false
                    :cell-border false
                    :widths      [10 40]
                    :spacing     -3}]
           (concat
             (for [[idx c] (->> (map-indexed vector characters)
                                (sort-by #(= "Chocolate Cake" (:label (second %)))))]
               (let [desc [:phrase
                           (when (= "Chocolate Cake" (:label c))
                             [:phrase "Sacrifice this " token " and " minion " towards seducing Prince Æcclescrumb. "])
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
               (text-cell "Shield Bonus" "You may take 1 free shield from the bank this turn.")]]
             ))

     #_(into [:table {:border      false
                    :cell-border false
                    :widths      [15 35 15 35]
                    :spacing     2}]
           (->> (concat
                  (for [[idx c] (->> (map-indexed vector characters)
                                     (sort-by #(= "Chocolate Cake" (:label (second %)))))]
                    (let [desc [:phrase
                                (when (= "Chocolate Cake" (:label c))
                                  [:phrase "Sacrifice this " token " and " minion " towards seducing Prince Æcclescrumb. "])
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

(defn build-instruction-image []

  ;(to-image/convert (io/file "generated" "instructions.pdf"))

  (let [i1 (ImageIO/read (io/file "generated" "instructions-1.png"))
        i2 (ImageIO/read (io/file "generated" "instructions-2.png"))
        i3 (ImageIO/read (io/file "generated" "instructions-3.png"))
        i4 (ImageIO/read (io/file "generated" "instructions-4.png"))
        i5 (ImageIO/read (io/file "generated" "instructions-5.png"))
        i6 (ImageIO/read (io/file "generated" "instructions-6.png"))
        i7 (ImageIO/read (io/file "generated" "instructions-7.png"))
        i8 (ImageIO/read (io/file "generated" "instructions-8.png"))
        border 40
        front (BufferedImage. (+ (* 4 700) (* 2 border)) (+ 1150 (* 2 border)) BufferedImage/TYPE_INT_ARGB)
        back (BufferedImage. (+ (* 4 700) (* 2 border)) (+ 1150 (* 2 border)) BufferedImage/TYPE_INT_ARGB)]

    (let [g (.getGraphics front)]
      (.drawImage g i6 (+ 0 border) (+ 0 border) nil)
      (.drawImage g i7 (+ 700 border) (+ 0 border) nil)
      (.drawImage g i8 (+ 1400 border) (+ 0 border) nil)
      (.drawImage g i1 (+ 2100 border) (+ 0 border) nil)
      (ImageIO/write front "png" (io/file "generated" "instructions-front.png")))

    (let [g (.getGraphics back)]
      (.drawImage g i2 (+ 0 border) (+ 0 border) nil)
      (.drawImage g i3 (+ 700 border) (+ 0 border) nil)
      (.drawImage g i4 (+ 1400 border) (+ 0 border) nil)
      (.drawImage g i5 (+ 2100 border) (+ 0 border) nil)
      (ImageIO/write back "png" (io/file "generated" "instructions-back.png")))))

(comment

  (render-instructions)
  (build-instruction-image)

  )

(comment
  ;([[:image {:scale 25} ./generated/for-instructions/token_0.png]
  ;  [:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Bronze]]
  ;   [:phrase nil  [:phrase You have  [:chunk.bold 1]  more to spend at turn's end. ] nil nil]]]
  ; [[:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Silver]]
  ;   [:phrase nil  [:phrase You have  [:chunk.bold 2]  more to spend at turn's end. ] nil nil]]
  ;  [:image {:scale 25} ./generated/for-instructions/token_1.png]]
  ; [[:image {:scale 25} ./generated/for-instructions/token_2.png]
  ;  [:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Gold]]
  ;   [:phrase nil  [:phrase You have  [:chunk.bold 3]  more to spend at turn's end. ] nil nil]] ]
  ; [[:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Farmer]]
  ;   [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  to an adjacent hex. ] [:phrase You have  [:chunk.bold 2]  more to spend at turn's end. ] nil nil]]
  ;  [:image {:scale 25} ./generated/for-instructions/token_3.png] ]
  ; [[:image {:scale 25} ./generated/for-instructions/token_4.png]
  ;  [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Scout]]
  ;   [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 4  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  on an adjacent hex  doing  [:chunk.bold 1]  damage.]]]]
  ; [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Archer]]
  ;   [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 2  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  up to  [:phrase.bold 3  hexes]  away  doing  [:chunk.bold 2]  damage.]]]
  ;  [:image {:scale 25} ./generated/for-instructions/token_5.png] ]
  ; [[:image {:scale 25} ./generated/for-instructions/token_7.png]
  ;  [:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Smithy]]
  ;   [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  to an adjacent hex. ] nil [:phrase You may take the next  2  cheapest  [:chunk.special.shield Shields]  from the piece card from any row. ] nil]] ]
  ; [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold Knight]]
  ;   [:phrase nil [:phrase You may move this  [:chunk.special.minion Minion]  up to  [:phrase.bold 4  hexes] . ] nil nil [:phrase You may attack one other  [:chunk.special.minion Minion]  on an adjacent hex  doing  [:chunk.bold 3]  damage.]]]
  ;  [:image {:scale 25} ./generated/for-instructions/token_8.png] ]
  ; [[:image {:scale 25} ./generated/for-instructions/token_6.png]
  ;  [:cell {:leading 14, :colspan 2}
  ;   [:heading.smaller [:chunk.bold Chocolate Cake]]
  ;   [:phrase [:phrase Sacrifice this  [:chunk.special.token Token]  and  [:chunk.special.minion Minion]  for a  [:chunk.special.vp Victory Point] . ]  nil nil nil]]]
  ; [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may move 1 extra hex this turn.]
  ;  [:image {:scale 25} ./generated/for-instructions/bonus_0.png] ]
  ; [[:image {:scale 25} ./generated/for-instructions/bonus_1.png]
  ;  [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] You have 1 extra spend at the end of your turn.] ]
  ; [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may attack with 1 extra range this turn.]
  ;  [:image {:scale 25} ./generated/for-instructions/bonus_2.png] ]
  ; [[:image {:scale 25} ./generated/for-instructions/bonus_3.png]
  ;  [:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] One minion may attack with 1 extra damage this turn.] ]
  ; [[:cell {:leading 14, :colspan 2} [:heading.smaller [:chunk.bold nil]] You may take 1 free shield from the bank this turn.]
  ;  [:image {:scale 25} ./generated/for-instructions/bonus_4.png] ])


  )
