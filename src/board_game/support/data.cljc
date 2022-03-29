(ns board-game.support.data)


(def max-coins 4)
(defn make-money [label amount price color]
  {:label  label
   :colour color
   :coin   amount
   :money? true
   :price  price})

(defn make-role [label move damage coin shield range price colour]
  {:label  label
   :move   move
   :damage damage
   :shield shield
   :coin   coin
   :range  range
   :price  price
   :colour colour})

(defn make-cake [label move damage coin shield range price colour]
  {:label  label
   :move   move
   :damage damage
   :shield shield
   :coin   coin
   :range  range
   :price  price
   :colour colour
   :cake?  true})

 ;"Gold" Color/YELLOW
 ;   "Silver" Color/LIGHT_GRAY
 ;   "Bronze" (draw/rgb 205, 127, 0)
 ;   "Scout" (draw/rgb 255, 175, 0)

(def characters
  [
   ;                            Move  Dmg.  Coin  Shield Range Price Color Description

   (make-money "Bronze"                     1                  0 [205 127 0])
   (make-money "Silver"                     2                  4 [192 192 192])
   (make-money "Gold"                       3                  7 [255 255 0])

   (make-role "Plough"          1     nil   2     nil    nil   1 [150 255 100])
   (make-role "Dagger"          4     1     nil   nil    nil   2 [255 175 0])
   (make-role "Bow"             2     2     nil   nil    3     3 [0   255 255])
   (make-cake "Chocolate Cake"  nil   nil   nil   nil    nil   5 [255 50  200])
   (make-role "Anvil"           1     nil   nil   2      nil   6 [100 100 255])
   (make-role "Sword"           4     3     nil   nil    nil   8 [255 0   0])

   ])

(def terrain-map
  [[:cake :cake :cake :cake :cake :cake :cake :cake :cake :cake]
   [:wall :twrr :spot :spot :spot :tree :spot :twrl :wall]
   [:spot :spot :spot :spot :spot :spot :spot :spot :spot :spot]
   [:tree :spot :twrl :wall :wall :wall :twrr :spot :spot]
   [:spot :spot :spot :spot :spot :spot :spot :spot :tree :spot]
   [:tree :spot :spot :midd :trmd :midd :spot :tree :tree]
   [:tree :tree :spot :midd :midd :midd :midd :spot :tree :spot]
   [:rivr :tree :spot :midd :midd :midd :spot :spot :spot]
   [:spot :brga :spot :spot :spot :spot :spot :spot :spot :spot]
   [:spot :rivr :rivr :spot :tree :tree :spot :spot :spot]
   [:spot :spot :tree :rivr :brgb :rivr :rivr :spot :spot :tree]
   [:spot :tree :tree :spot :spot :tree :brga :spot :tree]
   [:spot :spot :tree :spot :spot :spot :spot :rivr :brgb :rivr]
   [:spot :spot :tree :spot :spot :spot :spot :spot :tree]
   [:strt :strt :strt :strt :strt :strt :strt :strt :strt :strt]])