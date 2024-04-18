(ns banzai.puzzle.crossword.electric
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [banzai.puzzle.crossword.core :as cw]
            [banzai.puzzle.crossword.ui :as cw-ui :refer [coord-id focus-coord]]
            #?(:cljs [banzai.puzzle.crossword.ui.browser :as cw-ui-browser])
            [clojure.string :as string]))

(e/defn CrosswordPuzzleCluePane
  [{:keys [cells crossword/clues] :as cw-puzzle}
   cw-state
   ProcessCmd
   {:keys [words-key] :or {words-key :words}}]
  (e/client
    (let [grouped-words (group-by cw/word->axis (get cw-puzzle words-key))]
      (dom/ol (dom/props {:class ['ml-4]})
        (e/for-by first [[axis words] grouped-words]
          (dom/h3 (dom/props {:class "capitalize text-xl"})
            (dom/text (name axis)))
          (dom/ol (dom/props {:class ['mb-4]})
            (e/for-by first [[coord word cell]
                             (->> (e/for [word words]
                                    (let [coord (cw/word->coord word)]
                                      [coord word (get cells coord)]))
                               (sort-by (fn number [[_ _ {:keys [cell-num]}]] cell-num)))]
              (dom/li
                (when (some (partial = (:selected-coord cw-state)) (cw/word->coords word))
                  (dom/props {:class [(if (= (:axis cw-state) (cw/word->axis word)) 'bg-yellow-100 'bg-slate-100)
                                      'font-bold]}))
               (dom/label (dom/props {:for (coord-id coord)
                                      :class ['hover:bg-yellow-200 'cursor-pointer]})
                 (dom/on "click" (e/fn [_](new ProcessCmd ['select-coord {:coord coord, :axis axis}])
                                          (focus-coord coord)))
                (dom/text (:cell-num cell) ". " (get clues (cw/word->answer word))))))))))))

(e/defn all-cells
  [cw-puzzle cw-state]
  (e/client
    (cw/all-cells cw-puzzle cw-state)))

(e/defn CrosswordPuzzleGridPane
  [{:keys [col-count row-count] :as cw-puzzle}
   {:keys [selected-coord selected-word axis] :as cw-state}
   ProcessCmd]
  (e/client
    (dom/div (dom/props {:class ['relative]})
      (dom/div (dom/props {:class '["-z-1" absolute w-full h-full bg-cover bg-center bg-no-repeat]})
        (dom/style {:background-image "url(https://static-app-misc.teachbanzai.com/img/income-and-expenses-thumb.jpg)"}))
      (dom/div (dom/props {:class '[grid m-6]
                           :style {:grid-template-columns (string/join " " (repeat col-count "3rem"))
                                   :grid-template-rows    (string/join " " (repeat row-count "3rem"))}})
       (let [selected-word-coords (set (some-> selected-word cw/word->coords))]
        (e/for-by first [[[r-idx c-idx :as cell] {:keys [cell-num current-value correct-letter locked]}]
                         (cw/all-cells cw-puzzle cw-state)]
          (let [coord [r-idx c-idx]
                active-word? (contains? selected-word-coords coord)
                cell-format (if locked
                              `[bg-black border-black]
                              `[border-slate-500 border-1])
                text-format (if current-value "text-black" "text-slate-500")
                focused-cell? (= selected-coord coord)]
            (when-not locked
              (dom/div
                (dom/props (cond-> {:id (coord-id coord)
                                    :contenteditable true
                                    :class [text-format cell-format
                                            (when-not locked "border")
                                            (cond
                                              focused-cell? 'bg-yellow-200
                                              active-word? 'bg-blue-200
                                              :else 'bg-white)
                                            'justify-center 'items-center
                                            "text-xl bold grid items-center focus:outline-none height-[3rem] m-[-1px_-1px_0_0] caret-transparent"]}
                             (not locked) (assoc :tabindex 0)))
                (dom/style {:grid-row-start    (inc r-idx) ; Tailwind doesn't always find generated classes
                            :grid-column-start (inc c-idx)
                            :z-index           1})
                (dom/on "click"   (e/fn [e] (when focused-cell?
                                              (new ProcessCmd ['select-coord {:coord coord, :axis ({:across :down} axis :across)}]))
                                            (.focus (.-target e))))
                (dom/on "focus"   (e/fn [_] (new ProcessCmd ['select-coord {:coord coord}])
                                            (focus-coord coord)))
                (dom/on "blur"    (e/fn [_] (new ProcessCmd ['deselect-coord {:coord coord}])))
                (dom/on "input"   (e/fn [e] (let [value (some-> (.-data e) string/upper-case)
                                                  next-coord (cw/find-coord cw-puzzle cw-state coord
                                                               :find-coord/next-in-word :find-coord/next-empty-start-in-axis :find-coord/next-empty-in-axis :find-coord/next-empty)]
                                              (new ProcessCmd ['set-coord {:coord coord, :value value, :move :empty}])
                                              (focus-coord next-coord)
                                              (set! (.-innerText (.-target e)) value)
                                              (.preventDefault e))))
                (dom/on "keydown" (e/fn [e] (when-let [[_ args :as cmd] (cw-ui/keydown-command (cw-ui-browser/keydown-event-data e) {:coord coord})]
                                              (new ProcessCmd cmd)
                                              (when-let [next-coord (->> args :find-coord/directives (apply cw/find-coord cw-puzzle cw-state coord))]
                                                (focus-coord next-coord)
                                                (.preventDefault e))
                                              (when-let [[_ value] (find args :value)]
                                                (set! (.-innerText (.-target e)) value)
                                                (.preventDefault e)))))
                (dom/text current-value))

              (when cell-num
               (dom/div (dom/props {:class ['text-slate-800 'relative '-z-1 'left-0 'top-1]})
                (dom/style {:grid-row-start    (inc r-idx) ; Tailwind doesn't always find generated classes
                            :grid-column-start (inc c-idx)})
                (dom/sup (dom/props {:class ['text-slate-800 'relative 'm-1]})
                  (dom/style {:z-index 2})
                  (dom/text (str cell-num)))))))))))))

(e/defn CrosswordPuzzle [cw-puzzle cw-state ProcessCmd]
  (e/client
    (dom/div
      (dom/div (dom/props {:class "grid lg:grid-cols-2 m-4"})
        ;; (CrosswordPuzzleCluePane. cw-puzzle cw-state {:words-key :words-in-rows})
        (CrosswordPuzzleGridPane. cw-puzzle cw-state ProcessCmd)
        (CrosswordPuzzleCluePane. cw-puzzle cw-state ProcessCmd {:words-key :words}))
      #_(dom/pre (dom/code (dom/text (e/server (cw-ui/describe-cw-state cw-state)))))
      #_(dom/pre (dom/code (dom/text (e/server (cw-ui/describe-cw-puzzle cw-puzzle))))))))

(e/defn PuzzlePage [_ puzzle state]
  (e/server
    (let [!cw (atom {:puzzle (cw/comprehend-puzzle (or puzzle cw/example-cw-puzzle))
                     :state  (or state cw/example-cw-state)})
          cw (e/watch !cw)]
      (e/client
        (let [ProcessCmd (e/fn process-c-cmd [cmd]
                            (e/server
                              (let [{:keys         [update-state result]}
                                    (cw/mutate @!cw cmd)]
                                (when update-state
                                  (assert (fn? update-state))
                                  (swap! !cw update :state update-state))
                                result)))]
            (dom/h1 (dom/props {:class "text-xl"}) (dom/text "Crossword in ")
               (dom/a (dom/props {:href "https://electric.hyperfiddle.net/"
                                  :target "_blank"
                                  :class "cursor-pointer text-blue-500"})
                 (dom/text "Electric Clojure")))
            (let [{:keys [puzzle state]} (e/server cw)]
              (CrosswordPuzzle. puzzle state ProcessCmd)))))))
