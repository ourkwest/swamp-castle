(ns domination.support.util)


(def TAU (* 2 Math/PI))

(def millis-per-inch 25.4)

(def minion-size-mm millis-per-inch)
(def shield-size-mm 20) ;TODO: unknown!!1

(def pixels-per-mm 4)
(defn mm->px [mm] (* mm pixels-per-mm))

(def minion-size (mm->px minion-size-mm))
(def shield-size (mm->px shield-size-mm))

(def a4-width-mm 297)
(def a4-height-mm 210)


(defn clamp [lower x upper]
  (-> x
      (max lower)
      (min upper)))
