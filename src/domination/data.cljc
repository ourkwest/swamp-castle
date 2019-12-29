(ns domination.data)



(def max-coins 4)
(defn make-money [label amount price]
  {:label  label
   :colour [255 255 (int (* 255 (- 1 (/ amount max-coins))))]
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

(def characters
  [
   ;                            Move  Dmg.  Coin  Shield Range Price Color Description

   (make-money "Bronze"                     1                  0)
   (make-money "Silver"                     2                  4)
   (make-money "Gold"                       3                  7)

   (make-role "Farmer"          1     nil   2     nil    nil   1 [150 255 100])
   (make-role "Scout"           4     1     nil   nil    nil   2 [232 175 116])
   (make-role "Archer"          2     2     nil   nil    3     3 [0, 255, 255])
   (make-cake "Chocolate Cake"  nil   nil   nil   nil    nil   5 [255 50 200])
   (make-role "Smithy"          1     nil   nil   2      nil   6 [100, 100, 255])
   (make-role "Knight"          4     3     nil   nil    nil   8 [255 50 50])

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