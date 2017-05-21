(ns domination.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.events :as events]
            [domination.simulator :as sim]
            [domination.board :as board]
            [domination.card :as card]
            [domination.tokens :as tokens]
            [domination.instructions :as rules]))

(enable-console-print!)

(when-let [element (. js/document (getElementById "app"))]
  (reagent/render-component [sim/render-simulator] element)
  ;(println (dom/getElement "mounteddiv"))
  (events/listen (dom/getElement "mounteddiv") (.-KEYDOWN events/EventType)
                 (fn [e]
                   ;(println (.-keyCode e))
                   (when (= 18 (.-keyCode e))
                     (swap! sim/app-state assoc :trash-mode true))))
  (events/listen (dom/getElement "mounteddiv") (.-KEYUP events/EventType)
                 (fn [e]
                   (when (= 18 (.-keyCode e))
                     (swap! sim/app-state assoc :trash-mode false))))
  (sim/refocus-soon))

(when-let [element (. js/document (getElementById "board"))]
  (reagent/render-component [board/render-board] element))

(when-let [element (. js/document (getElementById "card"))]
  (reagent/render-component [card/render-card] element))

(when-let [element (. js/document (getElementById "rules"))]
  (reagent/render-component [rules/render-card] element))

(when-let [element (. js/document (getElementById "tokens"))]
  (reagent/render-component [tokens/render-tokens] element))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


; TODO:
;  tidy up dead code

