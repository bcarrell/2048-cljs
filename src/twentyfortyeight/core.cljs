(ns twentyfortyeight.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [dommy.core :as dommy]))

(enable-console-print!)

(defn randomize-start []
  "Returns two different integers from 0 to 15.  These are the two
  initial positions that start with 2."
  (let [x (rand-int 16)
        y (rand-int 16)]
    (if (= x y)
      (recur)
      [x y])))

(defn init-board []
  "Creates a 2048 game board.
  A board is a vector containing 16 elements representative
  of three rows.  14 of the elements will begin as 0.  2 will
  have the value of 2."
  (let [[x y] (randomize-start)
        coll (into [] (repeat 16 nil))]
    (assoc coll x 2 y 2)))

(def board-state (atom (init-board)))
(def score-state (atom 0))

(def board-edges {:left [0 4 8 12]
                  :right [3 7 11 15]
                  :top [0 1 2 3]
                  :bottom [12 13 14 15]})

(def keycodes {37 :left ;; keycodes with vi mode!
               38 :up
               39 :right
               40 :down
               72 :left
               75 :up
               76 :right
               74 :down})

(def key-ch (chan))

(defn evt->dir [e]
  (let [keycode (.-which e)]
    (get keycodes keycode)))

(defn key-listener [ch]
  (dommy/listen! js/window
                 :keyup
                 #(when-let [k (evt->dir %)]
                    (put! ch k)
                    (.preventDefault %))))

(key-listener key-ch)

(defn get-open [coll]
  "Returns the indices of all zero items in a collection."
  (map first (filter #(nil? (second %))
                     (map-indexed vector coll))))

(defn add-score [amount]
  "Updates the game scoreboard by the amount."
  (swap! score-state + amount))

(defn choose-random [coll]
  "Given a vector, chooses a random element."
  (get coll (rand-int (count coll))))

(defn add-new-block [coll]
  "Adds a new block to a random location on the board."
  (if-let [open ((comp choose-random vec get-open) coll)]
    (assoc coll open 2)
    coll))

(def not-nil? (complement nil?))

(defn movable? [direction coll]
  "Given a direction and collection representing the board's current state,
  determines if a move in that direction is possible."
  (let [edge (direction board-edges)
        xs (map #(nth coll %) edge)]
    (boolean (some nil? xs))))

(defn pad-with-nils [coll]
  "A helper function to ensure coll has 4 elements.  Pads with
  nils if less than 4."
  (take 4 (concat coll (repeat nil))))

(defn combine [xs ys]
  "Given two collections of non-nil elements,
  recursively combines them according to the game rules.

  Example:
     [] [2 2 4 8]

  ;; => [4 4 8]
  "
  (let [x (first ys)
        y (second ys)]
    (if (seq ys)
      (if (= x y)
        (do
          (add-score (+ x y))
          (combine (conj xs (+ x y)) (drop 2 ys)))
        (combine (conj xs x) (drop 1 ys)))
      xs)))

(defn combine-row [coll]
  "A helper function to take a collection representing a
  game row, filter out nil elements, combine the tiles where
  appropriate, and pad it back up with nils."
  (let [filtered (filter not-nil? coll)
        combined (combine [] filtered)]
    (pad-with-nils combined)))

(defn combine-rows [coll-of-colls]
  (map combine-row coll-of-colls))

(defn rearrange-rows [coll-of-colls]
  "Creates a new collection with all nil elements at the end of each coll"
  (let [not-nils (map #(filter not-nil? %) coll-of-colls)]
    (map pad-with-nils not-nils)))

(defn position-rows [coll vertical]
  "Partitions the rows in the correct orientation depending upon
  the direction."
  (if vertical
    (->> coll
         (partition 4)
         (apply interleave)
         (partition 4))
    (partition 4 coll)))

(defn vertical-rows? [direction]
  "Pred for determining if we need to partition into columns."
  (or (= direction :up) (= direction :down)))

(defn slide? [direction]
  "Pred for determining if we need to put the nils before the
  non-nils."
  (or (= direction :right) (= direction :down)))

(defn slide [coll]
  (let [not-nils (filter not-nil? coll)
        nils (repeat (- 4 (count not-nils)) nil)]
    (concat nils not-nils)))

(defn arrange-and-combine [coll direction]
  (let [should-slide (slide? direction)
        colls (-> coll
                  (position-rows (vertical-rows? direction))
                  rearrange-rows
                  combine-rows)]
    (if should-slide
      (map slide colls)
      colls)))

(defn process-move [direction]
  "Given a direction, reorients and combines the tiles according
  to game rules.  If applicable, we add a new block."
  (fn [xs]
    (let [colls (arrange-and-combine xs direction)
          coll (if (vertical-rows? direction)
                 (into [] (flatten (apply interleave colls)))
                 (into [] (flatten colls)))]
      (if (not= coll xs) ;; we moved and it was different, add a block!
        (add-new-block coll)
        coll))))

(defn move [direction cursor]
  "Transacts the application state when a move has been issued."
  (om/transact! cursor (process-move direction)))

(defn render-row [coll]
  "Creates an Om element representing a row.  A row contains 4 blocks."
  (apply dom/div #js {:className "row"} coll))

(defn render-block [x]
  "Creates an Om element representing a block within a row.  There are 16
  total blocks in the game."
  (dom/div #js {:className "block"} x))

(defn score-board [app owner]
  "An Om component representing the score board."
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (str "Score: " app)))))

(defn game-board [app owner]
  "An Om component representing the game board."
  (reify
    om/IWillMount
    (will-mount [_]
      (let [keypresses key-ch]
        (go (loop []
              (let [direction (<! keypresses)]
                (move direction app)
                (recur))))))
    om/IRender
    (render [this]
      (let [rows (partition 4 (map render-block app))]
        (apply dom/div
               nil
               (map #(render-row %) rows))))))

;; attach to DOM
(om/root score-board
         score-state
         {:target (. js/document (getElementById "score"))})

(om/root game-board
         board-state
         {:target (. js/document (getElementById "game"))})
