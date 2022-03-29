(ns board-game.support.util
  (:require
    [clojure.java.io :as io])
  (:import (java.lang Math$RandomNumberGeneratorHolder)
           (java.util Random)))


(def TAU (* 2 Math/PI))

(def millis-per-inch 25.4)

(def minion-size-mm millis-per-inch)
(def shield-size-mm 20) ;TODO: unknown!!1

(def pixels-per-mm 4)
(defn mm->px [mm] (* mm pixels-per-mm))
(defn px->mm [px] (/ px pixels-per-mm))

(def minion-size (mm->px minion-size-mm))
(def shield-size (mm->px shield-size-mm))

(def a4-width-mm 297)
(def a4-height-mm 210)


(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))

(def dir-to-print (io/file "generated" "to-print"))
(def dir-for-instructions (io/file "generated" "for-instructions"))

(def copyright-text "Copyright © 2017-2022 Rachel K. Westmacott")
(def tagline "A deck-building game of strategic warfare and cake-based seduction for 2-4 players.")

(defn set-random-seed [seed]
  (let [field (.getDeclaredField Math$RandomNumberGeneratorHolder "randomNumberGenerator")]
    (.setAccessible field true)
    (.setSeed ^Random (.get field nil) seed)))
