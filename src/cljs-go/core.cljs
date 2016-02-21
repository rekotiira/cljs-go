(ns cljs-go.core
  (:require [reagent.core :as r]
            [clojure.set :refer [union intersection difference select]]))

(enable-console-print!)

(defonce board-state (r/atom {:size 19
                              :turn :black
                              :stones {:black #{} :white #{}}}))

(defonce board-history (r/atom '()))

(add-watch board-state :board-history-watcher
  (fn [key atom old-state new-state]
    (swap! board-history conj new-state)))

(defonce color (r/atom "empty"))
(defonce liberties (r/atom 0))

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

(defn find-adjacent-dead-stones [{turn :turn, stones :stones, :as board} pos]
  (->> (stones (other-player turn))
       (get-neighbors board pos)
       (map (partial find-group-stones-at board))
       (filter #(empty? (find-group-liberties board %)))
       (apply concat)
       (set)))

(defn remove-stones [board who stones]
  (update-in board [:stones who] difference stones))

(defn valid-move? [{turn :turn, :as board} pos]
  (and
    (is-open-intersection? board pos)
    (let [opponent (other-player turn)
          place-stone-step (update-in board [:stones turn] conj pos)
          dead-stones (find-adjacent-dead-stones place-stone-step pos)
          remove-dead-stones-step (remove-stones place-stone-step opponent dead-stones)
          group (find-group-stones-at remove-dead-stones-step pos)
          liberties (find-group-liberties remove-dead-stones-step group)
          change-turn-step (assoc remove-dead-stones-step :turn opponent)]
      (and
        (seq liberties)
        (not (= (second @board-history) change-turn-step))))))

(defn place-stone-and-change-turn [{turn :turn, :as board} stone]
  (let [opponent (other-player turn)
        place-stone-step (update-in board [:stones turn] conj stone)
        dead-stones (find-adjacent-dead-stones place-stone-step stone)
        remove-dead-stones-step (remove-stones place-stone-step opponent dead-stones)
        change-turn-step (assoc remove-dead-stones-step :turn opponent)]
    change-turn-step))

(defn play-stone [{turn :turn, stones :stones, :as board} pos]
  (if (valid-move? board pos)
    (swap! board-state place-stone-and-change-turn pos)))

(defn update-debug-info [{stones :stones, :as board} valid-move-state pos]
  (reset! color (stone-as-string (whose-stone board pos)))
  (reset! liberties (count (find-group-liberties board (find-group-stones-at board pos)))))

(defn intersection-component [board pos]
  (let [valid-move-state (r/atom false)]
    (fn [board [x y :as pos]]
      (let [stone (whose-stone board pos)]
        [:div {:class (str "intersection " (stone-as-string stone) (if @valid-move-state " valid-move"))
               :on-click #(play-stone board pos)
               :on-mouse-enter #(reset! valid-move-state (valid-move? board pos))
               :on-mouse-leave #(reset! valid-move-state nil)}
          (for [i ["top" "bottom"]
                j ["left" "right"]]
            ^{:key (str "subtile-" i "-" j)}
            [:div {:class (str "subtile " i " " j)}])
          [:div {:class "stone"}]]))))

(defn board-component [board-state]
  (let [board @board-state
        size (inc (:size board))
        turn (:turn board)
        stones (:stones board)]
    [:div {:class (str "game " (stone-as-string turn) "-turn")}
      [:div {:class "board"}
        (for [y (range size)]
          ^{:key (str "row-" y)}
          [:div {:class "row"}
            (for [x (range size)]
              ^{:key (str "intersection-" x "-" y)}
              [intersection-component board [x y]])])]]))

(defn game-component []
  [:div
    [:div (str @color (if (> @liberties 0) (str ", liberties: " @liberties)))]
    [board-component board-state]])

(r/render-component [game-component] (. js/document (getElementById "app")))

(defn on-js-reload []
  ;(swap! board update-in [:turn] #(if (= % "black") "white" "black"))
)
