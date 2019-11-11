(ns domination.data)



(def max-coins 4)
(defn make-money [label amount price]
  {:label  (or label (if (= 1 amount) "1 Coin" (str amount " Coins")))
   :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
   :coin   amount
   :money? true
   :price  price})

(defn character [label move damage coin shield range price colour desc]
  {:label  label
   :move   move
   :damage damage
   :shield shield
   :coin   coin
   :range  range
   :price  price
   :colour (or colour [150 200 255])
   :desc   desc})

(def characters
  [
   ;                            Move  Dmg.  Coin  Shield Range Price Color Description

   (make-money "Bronze"                     1                  0)
   (make-money "Silver"                     2                  4)
   (make-money "Gold"                       3                  7)

   (character "Farmer"          nil   nil   2     nil    nil   1     [150 255 100]   nil)
   (character "Horse Rider"     4     1     nil   nil    nil   2     [232 175 116]   nil)
   (character "Archer"          2     2     nil   nil    3     3     [0, 255, 255]   nil)
   (character "Chocolate Cake"  nil   nil   nil   nil    nil   5     [255 50 200] "Sacrifice this token and minion for a Victory Point. Your minion must be on a 'Cake' hex to perform this action.")
   (character "Blacksmith"      nil   nil   nil   2      nil   6     [100, 100, 255]   nil)
   (character "Knight"          4     3     nil   nil    nil   8     [255 50 50]   nil)

   ])