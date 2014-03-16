(ns twentyfortyeight.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn randomize-start []
  "Returns two different integers from 0 to 8.  These are the two
  initial positions that start with 2."
  (let [x (rand-int 9)
        y (rand-int 9)]
    (if (= x y)
      (recur)
      [x y])))

(defn init-board []
  "Creates a 2048 game board.
  A board is a vector containing 9 elements representative
  of three rows.  7 of the elements will begin as nil.  2 will
  have the value of 2."
  (let [[x y] (randomize-start)
        coll (into [] (repeat 9 nil))]
    (assoc coll x 2 y 2)))

(def board-state (atom (init-board)))

(defn render-row [coll]
  (apply dom/div #js {:className "row"} coll))

(defn render-block [x]
  (dom/div #js {:className "block"} x))

(defn game-board [data owner]
  "An Om component representing the game board."
  (reify
    om/IInitState
    (init-state [_]
      {:rows data})
    om/IRenderState
    (render-state [this state]
      (let [rows (partition 3 (map render-block (:rows state)))]
        (apply dom/div
               nil
               (map #(render-row %) rows))))))

(om/root game-board
         board-state
         {:target (. js/document (getElementById "game"))})
