(ns cljs-go.core
  (:require [reagent.core :as r]
            [clojure.set :refer [union intersection difference select]]))

(enable-console-print!)

(defonce app-state
  (r/atom {:board {:size 19
                   :turn :black
                   :stones {:black #{} :white #{}}}
           :history '()}))

(defn stone-as-string [stone]
  (case stone
    :black "black"
    :white "white"
    "open"))

(defn whose-stone [{{black-stones :black, white-stones :white} :stones} pos]
  (cond
    (black-stones pos) :black
    (white-stones pos) :white
    :else nil))

(defn is-open-intersection? [board pos]
  (nil? (whose-stone board pos)))

(defn other-player [player]
  (if (= player :black)
    :white
    :black))

(defn in-bounds? [{size :size} [x y]]
  (and (<= 0 x size) (<= 0 y size)))

(defn get-neighbors
  ([board [x y]]
    (filter (partial in-bounds? board)
      [[x (dec y)]
       [x (inc y)]
       [(dec x) y]
       [(inc x) y]]))
  ([board pos filterfn]
    (filter filterfn (get-neighbors board pos))))

(defn get-neighbors-for-stones [board stones]
  (set (mapcat (partial get-neighbors board) stones)))

(defn find-group-stones-at [{size :size, stones :stones, :as board} pos]
  (if-let [player (whose-stone board pos)]
    (let [player-stones (stones player)]
      (loop [group-stones #{}
             unchecked-stones #{pos}]
        (let [neighbors (get-neighbors-for-stones board unchecked-stones)
              siblings (intersection neighbors player-stones)
              group-stones (union group-stones unchecked-stones)
              unchecked-siblings (difference siblings group-stones)]
          (if (seq unchecked-siblings)
            (recur group-stones unchecked-siblings)
            group-stones))))))

(defn find-group-liberties [board group]
  (let [neighbors (get-neighbors-for-stones board group)]
    (select (partial is-open-intersection? board) neighbors)))

(defn find-adjacent-dead-stones [{stones :stones, :as board} player pos]
  (->> (stones player)
       (get-neighbors board pos)
       (map (partial find-group-stones-at board))
       (filter #(empty? (find-group-liberties board %)))
       (apply concat)
       (set)))

(defn remove-stones [board player stones]
  (update-in board [:stones player] difference stones))

(defn place-stone [{turn :turn, :as board} history stone]
  "Returns a new board after stone has been placed, or nil if invalid move"
  (if (is-open-intersection? board stone)
    (let [opponent (other-player turn)
          place-stone-step (update-in board [:stones turn] conj stone)
          dead-stones (find-adjacent-dead-stones place-stone-step opponent stone)
          remove-dead-stones-step (remove-stones place-stone-step opponent dead-stones)
          new-board (assoc remove-dead-stones-step :turn opponent)
          group (find-group-stones-at new-board stone)
          liberties (find-group-liberties new-board group)]
      (if (and
            (seq liberties)
            (not (= new-board (first history)))) ; check history
        new-board))))

(defn valid-move? [board history stone]
  (not (nil? (place-stone board history stone))))

(defn update-board-state [app-state old-board new-board]
  (-> app-state
    (assoc :board new-board)
    (update :history conj old-board)))

(defn play-stone [pos]
  (let [{:keys [board history]} @app-state]
    (if-let [new-board (place-stone board history pos)]
      (swap! app-state update-board-state board new-board))))

(defn valid-move-from-state? [pos]
  (let [{:keys [board history]} @app-state]
    (valid-move? board history pos)))

(defn board-intersection [pos player]
  (let [valid-move (r/atom nil)]
    (fn [pos player]
      [:div {:class (str "intersection " (stone-as-string player) (if @valid-move " valid-move"))
             :on-click #(play-stone pos)
             :on-mouse-enter #(reset! valid-move (valid-move-from-state? pos))
             :on-mouse-leave #(reset! valid-move nil)}
        (for [i ["top" "bottom"]
              j ["left" "right"]]
          ^{:key (str "subtile-" i "-" j)}
          [:div {:class (str "subtile " i " " j)}])
        [:div {:class "stone"}]])))

(defn board []
  (let [board (:board @app-state)
        turn (:turn board)
        size (inc (:size board))
        stones (:stones board)]
    [:div {:class (str "game " (stone-as-string turn) "-turn")}
      [:div {:class "board"}
        (for [y (range size)]
          ^{:key (str "row-" y)}
          [:div {:class "row"}
            (for [x (range size)]
              (let [pos [x y]]
                ^{:key (str "intersection-" x "-" y)}
                [board-intersection pos (whose-stone board pos)]))])]]))

(defn game []
  [board])

(r/render-component [game] (. js/document (getElementById "app")))

(defn on-js-reload []
  ;(swap! board update-in [:turn] #(if (= % "black") "white" "black"))
)
