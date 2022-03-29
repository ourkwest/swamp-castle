(ns board-game.support.protocols
  (:import
    [java.awt Graphics2D]))

(defprotocol Style
  (prepare-for-draw [this ^Graphics2D g] "Returns a boolean")
  (prepare-for-fill [this ^Graphics2D g] "Returns a boolean"))
