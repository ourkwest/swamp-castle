(ns domination.instructions
  (:require
    [domination.data :refer [characters terrain-map]]
    [clojure.string :as string]))


(def minion [:span.special.minion "Minion"])
(def minions [:span.special.minion "Minions"])
(def shield [:span.special.shield "Shield"])
(def shields [:span.special.shield "Shields"])
(def vp [:span.special.vp "Victory Point"])
(def vps [:span.special.vp "Victory Points"])
(def token [:span.special.token "Token"])
(def tokens [:span.special.token "Tokens"])
(def bag [:span.special.bag "Bag"])
(def bags [:span.special.bag "Bags"])
(def cake [:span.special.cake "Chocolate Cake"])
(def midden [:span.special.midden "Midden"])
(def bronze [:span.special.bronze "Bronze"])
(def sacrifice [:span.special.sacrifice "Sacrifice"])
(def sacrificed [:span.special.sacrifice "Sacrificed"])

(defn render-card []

  [:div [:h1.center "Cake Walk"]

   [:div.center "A deck-building game of strategic warfare and cake-based seduction for 2-4 players."]

   [:div [:h2 "Setting"]
    [:p
     "The Dark Ages. In the Kingdom of the Britons, the rule of Queen Æthelscone the Mighty has crumbled with her untimely demise. "
     "Her son, the Prince Æthelcrumb waits in the castle for his coronation. "
     "A mysterious but exquisitely delicious artefact has fallen through a rift in time. "
     "The villagers are calling it \"" cake "\". "
     "Seize the opportunity! "
     "Hire some " minions "! "
     "Grab some " cake " and use it to woo the Prince in the Castle! "]]

   [:div [:h2 "Work in Progress"]
    [:p
     "This game is currently a work in progress. "
     ;"You will need to print out the board, the piece card and as many copies of the rules as you require. "
     ;"You will also need 5 playing pieces (\"" minions "\") per player (in the player's colour) and a large supply "
     ;"(20 should suffice, 44 to be sure) of shield pieces (\"" shields "\"). "
     ;"You will also need many coloured " tokens " (in " (count characters) " different colours) and " bags " to put them in (or cards to shuffle)."
     ]]

   [:div [:h2 "Pieces"]
    [:ul
     [:li "These instructions."]
     [:li "A game board with a map of the terrain."]
     [:li "5 " minion " pieces per player (in the player's colour)."]
     [:li "44 " shield " pieces."]
     [:li "A piece card with spaces for unbought " minions " and " shields "."]
     [:li "4 " tokens " each of " (count characters) " different types:"
      [:ul
       (let [moneys (->> characters (filter :money?) (map :label))]
         [:li (count moneys) " money types: " (string/join ", " moneys)])
       (let [roles (->> characters (remove :money?) (remove :cake?) (map :label))]
         [:li (count roles) " role types: " (string/join ", " roles)])
       [:li "...and " cake "!"]]]
     [:li "2 " token " " bags " per player"]
     [:li "5 " vps]]]

   [:div [:h2 "Aim"]
    [:p
     "The aim of the game is to acquire the most tokens of the Prince's affection (\"" vps "\"). "
     "These are acquired by having a " minion " on the final row of the board (marked 'Cake') and playing "
     "a " cake " "token" from your hand."]]

   [:div [:h2 "Setting up"]
    [:p
     "Choose how many " vps " to play with and place them in a pile. "
     "It is suggested that you use one more " vp " than there are players, but you can vary this for a shorter or longer game. "
     "N.b. If you pick the same number of " vps " as there are players then a draw is not unlikely. "]
    [:p
     "Layout the " (count characters) " types of " token " each in their own pile. (Or all in one big pile if you are short on time.) This is the Bank."]
    [:p "Place the coloured player " minions " and " shields " on the piece card. "
     "If there are fewer than 4 players then only use the corresponding number of rows for the " shields
     " and the appropriate coloured spots for the " minions ". "]
    [:p "Give each player 4 " bronze " " tokens " in a draw " bag ". "
     "Give each player an empty discard " bag ". "]]

   [:div [:h2 "Playing"]
    [:p "The game is played in turns and the player who has brought the most baked goods to the table takes the first turn. "]

    ;[:p "There are two types of " token ": money " tokens " and action " tokens "."] ; TODO: explain this, explain that money gets kept in discard pile
    [:p "Each turn is played as follows:"]
    [:ol
     [:li "Draw 4 " tokens " from your draw " bag " into your hand. "]
     [:li "Place up to 1 " token " on each " minion " of yours on the board. "]
     [:li "Perform the actions for the " tokens " that you have placed on your " minions ". "]
     [:li "Buy some " tokens ", " shields " or " minions ". "]
     [:li "Discard all played and unplayed " tokens " from your hand and all newly acquired " tokens " into your discard " bag ". "]]

    [:div [:h3 "1. Drawing " tokens]
     [:p
      "Draw 4 " tokens " into your hand at the start of your turn. "
      "Sometimes you will also draw " tokens " during your turn. "
      "When and only when your draw " bag " is empty swap it immediately with your discard " bag ". "
      "If you need to swap " bags " part way through drawing then do so and continue drawing until you have drawn the required number of " tokens ". "
      "If both your draw " bag " and your discard " bag " are empty then you must stop drawing, even if you are entitled to draw more "tokens". "]]

    [:div [:h3 "2. Placing " tokens]
     [:p
      "You may place 1 " token " on each of your " minions " that are on the board. "
      "You may only place role " tokens " and " cake " " tokens " on your minions. "
      "You may not place money " tokens " on your minions. "
      "You may only place " cake " " tokens " on " minions " that are on the final row of the board marked 'Cake'. "
      "For each " token " that you place you may draw another " token " in accordance with the above rules about drawing. "
      "When you place a " token " you may choose not to draw another "token" if you wish."]]

    [:div [:h3 "3. Performing " token " actions"]
     [:p
      "Each of your " minions " on the board may perform the action of the " token " placed on it. "]

     [:ul
      (for [c (->> characters
                   (sort-by :price)
                   (sort-by #(= "Chocolate Cake" (:label %)))) :when (not (:money? c))]
        (let [desc [:span
                    (when (= "Chocolate Cake" (:label c))
                      [:span "Sacrifice this " token " and " minion " for a " vp ". "])
                    (condp = (:move c)
                      nil ""
                      1 [:span "You may move this " minion " to an adjacent hex. "]
                      [:span "You may move this " minion " up to " [:b (:move c) " hexes"] ". "])
                    (when (:coin c) [:span "You have " [:b (:coin c)] " more to spend at turn's end. "])
                    (when (:shield c) [:span "You may take the next " (:shield c) " " shields " from the piece card. "])
                    (when (:damage c)
                      (if (:range c)
                        [:span "You may attack one other " minion " up to " [:b (:range c) " hexes"] " away "
                         "with " [:b (:damage c)] " damage points."]
                        [:span "You may attack one other " minion " on an adjacent hex "
                         "with " [:b (:damage c)] " damage points."]))]]
          [:li {:key (:label c)} [:b (:label c)] " - " desc]))]

     [:div [:h4 "Moving"]
      [:p
       minions " may not occupy the same hex, nor move through occupied hexes "
       "(even if they both belong to the same player). "
       "Hence, paths may be blocked. "]]

     [:div [:h4 "Attacking"]
      [:p
       "If you attack a " minion " they are damaged by the damage points of the attack. "
       "If the defending " minion " is on a tree hex, the attack is reduced by 1 damage point. "
       "If the defending player chooses they may return any number of " shields " that they have "
       "to reduce the attack by 1 damage point per " shield " played. "
       "If any damage points remain then the " minion " is returned. "]

      [:p
       "When returning " shields " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the number of " shield " spot rows corresponding to the number of players. "
       "When returning " minions " place them in the most expensive unoccupied spot on the piece card, but only using "
       "the spots of the correct player's colour. "]]

     [:div [:h4 "Moving & Attacking"]
      [:p
       "If the " token " permits the " minion " movement as well as attack, "
       "you may perform these in either order, "
       "but you cannot split your movement or attack into two parts. "
       "e.g. If a " token " grants up to 3 movement and 2 attack"
       " you may move the " minion " by 1 and then attack causing 2 damage, "
       " but you may not move 1 then attack and then move the remaining 2."]]

     [:div [:h4 "Attacking at Range"]
      [:p "If a " token " has a ranged attack, count the range as if it were movement. "
       "e.g. When an Archer attacks, imagine an arrow starting on the Archer's hex and "
       "moving up to 3 hexes to reach it's target."]]

     [:div [:h4 "Chocolate Cake"]
      [:p
       "Your " minion " is admitted to the castle to seduce Prince Æthelcrumb with a slice of delicious & moist Chocolate Cake. "]
      [:p
       "Your " minion " must be on a 'Cake' hex to perform this action. "
       "Your " minion " is completely retired from the game; they are removed from the board and can no longer be bought. "
       "Your " cake " " token " is returned to the bank. "]]]

    [:div [:h3 "4. Buying"]
     [:p
      "At the end of your turn, you may buy as many things as you can afford. "
      "You can afford to buy things up to the value of all the money " tokens " in your hand "
      "plus all the money values of the role " tokens " you have played this turn. "
      "Do not count the money values on role " tokens " that you have not played this turn. "]
     [:p
      "Bought " tokens " are placed in your discard " bag ". "
      "Bought " minions " are immediately placed on any unoccupied starting hex (marked 'Start'). "
      "Bought " shields " are placed in a pile by the player where all other players can see them. "]]

    [:div [:h3 "5. Discarding"]
     [:p
      "At the end of your turn place all played and unplayed " tokens " from your hand and all newly acquired "
      tokens " into your discard " bag ". "]
     [:p
      "If any of your " minions " are on one of the " (->> terrain-map flatten (filter #{:midd :trmd}) count) " central " midden
      " hexes at the end of your turn, then you may choose to return any "
      " number of the discarded "tokens" to the bank instead of to your discard " bag ". "]]

    ]])