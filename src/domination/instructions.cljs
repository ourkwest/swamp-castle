(ns domination.instructions
  (:require
    [domination.simulator :refer [characters]]
    [clojure.string :as string]))




; Rules


; Setting "Knights of ..."


; Aim of the Game
;
;

(def minion [:b "Minion"])
(def minions [:b "Minions"])

(def shield [:b "Shield"])
(def shields [:b "Shields"])

(def vp [:b "Victory Point"])
(def vps [:b "Victory Points"])

(def token [:b "Token"])
(def tokens [:b "Tokens"])

(def bag [:b "Bag"])
(def bags [:b "Bags"])

(def cake [:b "Chocolate Cake"])

(def midden [:b "Midden"])

(def coin-1 [:b "Single Coin"])

(defn render-card []

  [:div

   [:h1 "Knights of Bog Castle (working title)"]

   [:div
    [:h2 "Work in Progress"]
    [:p
     "This game is a work in progress. "
     "You will need to print out the board, the piece card and as many copies of the rules as you require. "
     "You will also need 5 playing pieces (\"" minions "\") per player (in the player's colour) and a large supply "
     "(20 should suffice) of shield pieces (\"" shields "\"). "
     "The " tokens " and " bags " can be simulated by the code."
     ]]

   [:div
    [:h2 "Setting"]
    [:p
     "A mysterious but exquisitely delicious artefact has fallen through a rift in time. "
     "The villagers are calling it \"" cake "\". "
     "Seize the opportunity! "
     "Hire some " minions "! "
     "Grab some " cake " and use it to woo the Prince in the Castle! "]]

   [:div
    [:h2 "Aim of the Game"]
    [:p
     "The aim of the game is to acquire the most tokens of the Prince's affection (\"" vps "\"). "
     "These are acquired by having a " minion " on the final row of the board (marked 'Cake') and playing "
     "a " cake " token from your hand."]]

   [:div
    [:h2 "Set up the Game"]
    [:p
     "Place the coloured player " minions " and " shields " on the piece card. "
     "Give each player 4 " coin-1 " " tokens " in a draw " bag "."
     "Give each player an empty discard " bag "."]]

   [:div
    [:h2 "Playing the Game"]
    [:p
     "The player who has brought the most baked goods to the table plays first. "

     "Players take it in turns to play as follows:"

     [:ol
      [:li "Draw 4 " tokens " from your draw " bag " into your hand."]
      [:li "Perform up to 1 " minion " action per " minion "."]
      [:li "Buy some " tokens ", " shields " or " minions "."]
      [:li "Place all played and unplayed " tokens " from your hand and all newly purchased " tokens " in your discard " bag "."]]

     [:h3 "Drawing"]

     [:p
      "Draw 4 " tokens " into your hand at the start of your turn. "
      "Sometimes you will also draw " tokens " during your turn."
      "When your draw " bag " is empty swap it with your discard " bag ". "
      "If you need to swap " bags " part way through drawing, then continue drawing until you have drawn the required number of " tokens "."
      "If both your draw " bag " and your discard " bag " are empty then you must stop drawing, even if you are entitled to draw more tokens."]

     [:h3 "Performing " minion " actions"]


     [:p
      "There are two kinds of " minion " action: "
      "those that occur when you choose to play a " token " with a " minion " symbol on it "
      "and those that occur when you choose to play the " midden ". "
      "You may only play one " minion " action per " minion " per turn. "
      ]

     [:h4 minion " " tokens]
     [:p
      "If you have a " minion " " token " in your hand and you have a " minion " that you have not yet played this turn "
      "then you may play the " token " for the " minion ". "
      "The action for the " token " is then taken by that " minion ". "]

     [:p
      "If the " token " permits the " minion " movement as well as attack, "
      "you may perform these in either order, "
      "but you cannot split your movement or attack into two parts. "
      "e.g. If a " token " grants up to 3 movement and 2 attack"
      " you may move the " minion " by 1 and then attack causing 2 damage, "
      " but you may not move 1 then attack and then move the remaining 2."]

     [:h4 "The " midden]
     [:p
      "If you have a " minion " that you have not yet played this turn "
      "and it is located on one of the 7 " midden " hexes "
      "then you may perform the " midden " action."]

     [:h3 "List of " minion " actions"]

     [:ul

      (cons [:li "Midden - sacrifice one unplayed " token " from your hand."]
            (for [c characters]
              (let [desc (string/join " "
                           [(:desc c)
                            (condp = (:move c)
                              nil nil
                              1 (str "You may move this " minion " to an adjacent hex.")
                              (str "You may move this " minion " up to " (:move c) " hexes."))
                            (when (:coin c) (str "You have " (:coin c) " more to spend at turn's end."))
                            (when (:shield c) (str "You may take the next " shield " from the piece card."))
                            (when (:damage c)
                              (if (:range c)
                                (str "You may attack one other " minion " up to " (:range c) " hexes away "
                                     "with " (:damage c) " damage points.")
                                (str "You may attack one other " minion " on an adjacent hex "
                                     "with " (:damage c) " damage points.")))
                            ])]
                [:li (:label c) " - " desc])))

      ]

     [:h3 "Moving"]

     [:p
      minions " may not occupy the same hex, nor move through occupied hexes "
      "(even if they both belong to the same player). "
      "Hence, bridges may be blocked. "]

     [:h3 "Attacking"]

     [:p
      "If you attack a " minion " they are damaged by the damage points of the attack. "
      "If the defending " minion " is on a tree hex, the attack is reduced by 1 damage point. "
      "If the defending player chooses they may sacrifice any number of " shields " that they have "
      "to reduce the attack by 1 damage point per " shield " played. "
      "If any damage points remain then the " minion " is removed from the board and replaced on the piece card. "]

     [:h3 "Buying"]

     [:p
      "At the end of your turn, you may buy as many things as you can afford. "
      "You can afford to buy things up to the value of all the money " tokens " in your hand "
      "and all the money values of the " minion " " tokens " you have played. "
      "Do not count the money values on " minion " " tokens " that you have not played. "]

     [:p
      "Bought " tokens " are placed in your discard " bag ". "
      "Bought " minions " are placed on any unoccupied starting hex (marked 'Start'). "
      "Bought " shields " are placed in a pile by the player where all other players can see them. "
      "The price for " minions " and " shields " is the visible number printed in the adjacent spot. "]




     [:h3 "Sacrificing"]
     [:p
      "Sacrificed " tokens " return to the bank. "
      "Sacrificed " minions " and " shields " return to the piece card. "
      "When returning " minions " and " shields ", place them in the most expensive unoccupied spot."]

     ]]

   ])