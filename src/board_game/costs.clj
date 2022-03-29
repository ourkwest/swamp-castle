(ns board-game.costs)


(defn thing
  ([name cost-per-game-for-1 cost-per-game-for-100]
   {:name    name
    :mutex   name
    :per-1   cost-per-game-for-1
    :per-100 cost-per-game-for-100})
  ([name sub-name cost-per-game-for-1 cost-per-game-for-100]
   {:name    name
    :mutex   name
    :per-1   cost-per-game-for-1
    :per-100 cost-per-game-for-100}))

(def things
  [(thing "box" 15.00 5.00)
   (thing "board" 11.00 6.00)
   (thing "piece-card" 11.00 6.00)
   (thing "instruction-booklet" 5.00 0.50)

   (thing "minions" "wooden" 4.00 3.00)
   ;(thing "minions" "printed plastic" 48.00 24.00)
   ;(thing "minions" "plastic with stickers" (+ 3.00 2.80) (+ 3.00 1.70))

   ;(thing "bags" 10.40 8.00)
   ;(thing "token cases" 22.50 15.00)
   #_(let [thin-sheets 0]
     (thing "thin tokens" (* thin-sheets 11.05) (* thin-sheets 4.20)))

   #_(let [thick-sheets 1]
     (thing "thick tokens" (* thick-sheets 14.45) (* thick-sheets 5.45)))

   #_(let [roles 6
         multiples 3]
     (thing "role" "small wooden disc" (* roles multiples 0.10) (* roles multiples 0.10))
     ;(thing "role" "printed small wooden disc" (* roles multiples 1.30) (* roles multiples 0.65))
     )
   ;(thing "30x 32mm round tiles for Shields & VPs" 14.45 5.45)
   ;(thing "deck of 144 cards" 25.00 11.60)

   (let [token-count 100
         sticker-1-sheet 2.80
         sticker-100-sheets 1.70
         stickers-per-sheet 54
         sticker-sheet-count (Math/ceil (/ token-count stickers-per-sheet))]
     (thing "tokens" "20mm x 5mm round plastic tokens"
            (+ (* token-count 0.10)
               (* sticker-sheet-count sticker-1-sheet))
            (+ (* token-count 0.10)
               (* sticker-sheet-count sticker-100-sheets))))

   ])


(println (transduce (map :per-1) + 0 things))
(println (transduce (map :per-100) + 0 things))

(defn flatten-component [component]
  (cons component (mapcat flatten-component (:components component))))

(defn describe [component]
  (println (:component-name component))
  (let [component-groups (vals (group-by :component-name (flatten-component component)))]
    (doseq [;group (sort-by count (filter (comp :url first) component-groups))
            group (sort-by count component-groups)
            :let [{:keys [cost-per-batch units-per-batch]} (first group)]
            :when cost-per-batch]
      (let [number-of-batches (Math/ceil (/ (count group) units-per-batch))]
        (println (format "%5.2f %3d x %s" (* number-of-batches (cost-per-batch 1)) (count group) (:component-name (first group))))))
    (println "Overall:")
    (doseq [n [1 10 100 1000]]
      (println "    " n (reduce + 0 (for [group component-groups
                                          :let [{:keys [cost-per-batch units-per-batch]} (first group)]
                                          :when cost-per-batch]
                                      (let [number-of-batches (Math/ceil (/ (count group) units-per-batch))]
                                        (* number-of-batches (cost-per-batch n))))))))
  component)

(defn component
  ([component-name components]
   ;(println component-name)
   #_(let [component-groups (vals (group-by :component-name (flatten-component component)))]
     (doseq [n [1 10 100 1000]
             group component-groups
             :let [{:keys [cost-per-batch units-per-batch]} (first group)]
             :when cost-per-batch]
       (let [number-of-batches (Math/ceil (/ (count group) units-per-batch))]
         (println n (* number-of-batches (cost-per-batch n))))))
   {:component-name component-name
    :components     components})
  ([component-name url units-per-batch cost-per-batch]
   ;(println component-name)
   ;(doseq [[batches price] cost-per-batch]
   ;  (println batches price))
   {:component-name  component-name
    :url             url
    :units-per-batch units-per-batch
    :cost-per-batch  cost-per-batch}))

(def token-sticker
  (component "Custom Stickers 20mm" "https://www.boardgamesmaker.com/print/custom-stickers-20mm.html" 54
             {1    2.80
              10   2.50
              100  1.70
              1000 1.45}))

(def plastic-token
  (component "Game Tokens 20mm X 5mm" "https://www.boardgamesmaker.com/print/game-tokens-18mm.html" 1
             {1    0.10
              10   0.10
              100  0.10
              1000 0.10}))

(def game-token (component "Token" [plastic-token token-sticker token-sticker]))

(let [[r g p y] (for [variant ["Red" "Green" "Purple" "Yellow"]]
                  (component (str "25X25X6mm Wooden Disc " variant) "https://www.boardgamesmaker.com/print/fi-8747.html" 1
                             {1    0.20
                              10   0.15
                              100  0.15
                              1000 0.10}))]
  (def minion-red r)
  (def minion-green g)
  (def minion-purple p)
  (def minion-yellow y))

(def set-of-minions
  (component "Set of minions" (concat (repeat 5 minion-red)
                                      (repeat 5 minion-green)
                                      (repeat 5 minion-purple)
                                      (repeat 5 minion-yellow))))

(def set-of-tokens
  (let [players 4
        bronzes (* 4 players)
        silvers (* 3 players)
        golds (* 2 players)
        farmers (* 3 players)
        scouts (* 3 players)
        archers (* 3 players)
        cakes (* 2 players)
        smithies (* 2 players)
        knights (* 2 players)
        token-count (+ bronzes silvers golds farmers scouts archers cakes smithies knights)]
    (println token-count " Tokens")
    (component "Set of tokens" (repeat token-count game-token))))

;(thing "box" 15.00 5.00)
;(thing "board" 11.00 6.00)
;(thing "piece-card" 11.00 6.00)
;(thing "instruction-booklet" 5.00 0.50)

(def other-stuff (component "Stuff" "..." 1 {1 (+ 5)
                                             10 (+ 5) ; a guess
                                             100 (+ 0.5)
                                             1000 (+ 0.5) ; a guess
                                             }))

(def board
  (component "Board: 267mm x 350mm / Bi-fold" "https://www.boardgamesmaker.com/print/custom-size-game-board.html" 1
             {1    12.55
              10   9.65
              100  6.65
              1000 2.65}))

(def piece-card
  (component "Piece card: 210mm x 320mm / Bi-fold" "https://www.boardgamesmaker.com/print/custom-size-game-board.html" 1
             {1    11.30
              10   8.70
              100  6.00
              1000 2.40}))

(def box
  (component "Custom Box 280mm x 190mm x 48mm inner dimensions"
             "https://www.boardgamesmaker.com/print/custom-size-game-box-oem.html#" 1
             {1    15.60
              10   15.60
              100  6.10
              1000 3.25}))

(def instructions
  (component "Bi-Fold Booklet For Large Size 3.5\"X5.75\""
             "https://www.boardgamesmaker.com/print/custom-bi-fold-instructions-booklet-large-size.html" 1
             {1    3.35
              10   2.55
              100  0.55
              1000 0.30}))

(def bag (component "Bag" "https://www.boardgamesmaker.com/print/black-game-bags.html" 1
                    {1    1.30
                     10   1.15
                     100  1.00
                     1000 0.80}))
(def set-of-bags (component "Set of Bags" (repeat 8 bag)))

(def card-token
  (component "Custom Circular Game Tiles 25mm" "https://www.boardgamesmaker.com/print/fi-9673.html" 48
             {1    14.45
              10   10.65
              100  5.45
              1000 2.10}))

(def set-of-shields (component "Set of Shields" (repeat 30 card-token)))
(def set-of-vps (component "Set of Victory Points" (repeat 10 card-token)))

(def cakewalk
  (component "Cakewalk"
             [box instructions board piece-card set-of-minions set-of-tokens set-of-bags set-of-shields set-of-vps]))

(describe cakewalk)
