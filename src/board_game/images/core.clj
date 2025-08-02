(ns board-game.images.core
  (:require [board-game.support.symbol :as symbol]
            [clojure.java.io :as io]
            [board-game.images.tokens :as tokens]
            [board-game.images.board :as board]
            [board-game.images.instructions :as instructions]
            [board-game.images.piece-card :as piece-card]))



(defn create-all-images []

  (let [dir (io/file "generated")]
    (run! #(io/delete-file % :silently!) (reverse (file-seq dir)))
    (.mkdir dir))

  (tokens/render-token-images)
  (board/draw-board-properly)
  (symbol/draw-cake-image)
  (instructions/render-instructions)
  ;(instructions/build-instruction-image)
  (piece-card/draw-piece-card)

  ; cannot draw the box for now as there's a missing image...

  )


(comment

  (create-all-images)

  )
