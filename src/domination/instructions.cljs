(ns domination.instructions
  (:require
    [domination.simulator :refer [characters]]
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

   [:div [:h2 "Work in Progress"]
    [:p
     "This game is a work in progress. "
     "You will need to print out the board, the piece card and as many copies of the rules as you require. "
     "You will also need 5 playing pieces (\"" minions "\") per player (in the player's colour) and a large supply "
     "(20 should suffice) of shield pieces (\"" shields "\"). "
     "You will also need many coloured " tokens " (in " (count characters) " different colours) and " bags " to put them in (or cards to shuffle)."
     ]]

   [:div [:h2 "Setting"]
    [:p
     "The Dark Ages. In the Kingdom of the Britons, the rule of Queen Æthelscone the Mighty has crumbled with her untimely demise. "
     "Her son, the prince Æthelcrumb waits in the castle for his coronation. "
     "A mysterious but exquisitely delicious artefact has fallen through a rift in time. "
     "The villagers are calling it \"" cake "\". "
     "Seize the opportunity! "
     "Hire some " minions "! "
     "Grab some " cake " and use it to woo the Prince in the Castle! "]]

   [:div [:h2 "Aim of the Game"]
    [:p
     "The aim of the game is to acquire the most tokens of the Prince's affection (\"" vps "\"). "
     "These are acquired by having a " minion " on the final row of the board (marked 'Cake') and playing "
     "a " cake " token from your hand."]]

   [:div [:h2 "Set up the Game"]
    [:p
     "Choose how many " vps " to play with and place them in a pile. "
     "It is suggested that you use one more " vp " than there are players, but you can vary this for a shorter or longer game. "
     "Layout the " (count characters) " types of " token " each in their own pile. "
     "Place the coloured player " minions " and " shields " on the piece card. "
     "Give each player 4 " bronze " " tokens " in a draw " bag ". "
     "Give each player an empty discard " bag ". "]]

   [:div [:h2 "Playing the Game"]
    [:p "The player who has brought the most baked goods to the table plays first. "]
    [:p "There are two types of " token ": money " tokens " and action " tokens "."] ; TODO: explain this, explain that money gets kept in discard pile
    [:p "Players take it in turns to play as follows:"]
    [:ol
     [:li "Draw 4 " tokens " from your draw " bag " into your hand."]
     [:li "Perform up to 1 " minion " action per " minion " on the board."]
     [:li "Buy some " tokens ", " shields " or " minions "."]
     [:li "Discard all played and unplayed " tokens " from your hand and all newly acquired " tokens " into your discard " bag "."]]

    [:div [:h3 "1. Drawing"]
     [:p
      "Draw 4 " tokens " into your hand at the start of your turn. "
      "Sometimes you will also draw " tokens " during your turn."
      "When your draw " bag " is empty swap it with your discard " bag ". "
      "If you need to swap " bags " part way through drawing, then continue drawing until you have drawn the required number of " tokens "."
      "If both your draw " bag " and your discard " bag " are empty then you must stop drawing, even if you are entitled to draw more tokens."]]

    [:div [:h3 "2. Performing " minion " actions"]
     [:p
      "Each " minion " on the board may only take one action per turn. "
      "There are two kinds of " minion " action: "]
     [:ul
      [:li "those that occur when you choose to play a " token " with a " minion " symbol on it "]
      [:li " those that occur when you choose to play the " midden ". "]]

     [:div [:h4 "Playing " minion " " token " actions"]
      [:p
       "If you have a " minion " token in your hand, "
       "and a " minion " on the board that has taken no action yet this turn then you may play the " token ". "
       "Follow the instructions for that " token " and apply them to that " minion ". "
       "That " token " is now played, but you may draw a replacement " token " into your hand."]
      [:p
       "If the " token " permits the " minion " movement as well as attack, "
       "you may perform these in either order, "
       "but you cannot split your movement or attack into two parts. "
       "e.g. If a " token " grants up to 3 movement and 2 attack"
       " you may move the " minion " by 1 and then attack causing 2 damage, "
       " but you may not move 1 then attack and then move the remaining 2."]]

     [:div [:h4 "Playing the " midden]
      [:p
       "If you have a " minion " that has taken no action yet this turn "
       "and it is located on one of the 7 " midden " hexes "
       "then you may perform the " midden " action. "
       sacrifice " one unplayed " token " from your hand. "
       "You may draw a replacement " token " into your hand."]]

     [:div [:h4 "List of all " minion " actions"]
      [:ul
       (cons [:li {:key "Midden"} [:b "Midden"] " - " sacrifice " one unplayed " token " from your hand."]
             (for [c characters]
               (let [desc [:span
                           (when (= "Chocolate Cake" (:label c))
                             [:span sacrifice " this " token " and " minion " for a " vp ". Your " minion " must be on a 'Cake' hex to perform this action."])
                           (condp = (:move c)
                             nil ""
                             1 [:span "You may move this " minion " to an adjacent hex. "]
                             [:span "You may move this " minion " up to " [:b (:move c) " hexes"] ". "])
                           (when (:coin c) [:span "You have " [:b (:coin c)] " more to spend at turn's end. "])
                           (when (:shield c) [:span "You may take the next " shield " from the piece card. "])
                           (when (:damage c)
                             (if (:range c)
                               [:span "You may attack one other " minion " up to " [:b (:range c) " hexes"] " away "
                                "with " [:b (:damage c)] " damage points."]
                               [:span "You may attack one other " minion " on an adjacent hex "
                                "with " [:b (:damage c)] " damage points."]))]]
                 [:li {:key (:label c)} [:b (:label c)] " - " desc])))]]

     [:div [:h4 "Moving"]
      [:p
       minions " may not occupy the same hex, nor move through occupied hexes "
       "(even if they both belong to the same player). "
       "Hence, bridges may be blocked. "]]

     [:div [:h4 "Attacking"]
      [:p
       "If you attack a " minion " they are damaged by the damage points of the attack. "
       "If the defending " minion " is on a tree hex, the attack is reduced by 1 damage point. "
       "If the defending player chooses they may " sacrifice " any number of " shields " that they have "
       "to reduce the attack by 1 damage point per " shield " played. "
       "If any damage points remain then the " minion " is " sacrificed ". "]]

     [:div [:h4 "Sacrificing"]
      [:p
       sacrificed " " tokens " return to the bank. "
       sacrificed " " minions " and " shields " return to the piece card. "
       "When returning " minions " and " shields ", place them in the most expensive unoccupied spot."]]]

    [:div [:h3 "3. Buying"]
     [:p
      "At the end of your turn, you may buy as many things as you can afford. "
      "You can afford to buy things up to the value of all the money " tokens " in your hand "
      "and all the money values of the " minion " " tokens " you have played. "
      "Do not count the money values on " minion " " tokens " that you have not played. "]
     [:p
      "Bought " tokens " are placed in your discard " bag ". "
      "Bought " minions " are placed on any unoccupied starting hex (marked 'Start'). "
      "Bought " shields " are placed in a pile by the player where all other players can see them. "
      "The price for " minions " and " shields " is the visible number printed in the adjacent spot. "]]

    [:div [:h3 "4. Discarding"]
     [:p "Place all played and unplayed " tokens " from your hand and all newly acquired " tokens " into your discard " bag "."]]

    ]])