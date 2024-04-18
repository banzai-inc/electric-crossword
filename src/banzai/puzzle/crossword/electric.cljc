(ns banzai.puzzle.crossword.electric
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [banzai.puzzle.crossword.core :as cw]
            [clojure.string :as string]))

;; #?(:cljs (set! hashp.core/print-opts (assoc hashp.core/print-opts :print-color false)))

#?(:clj (defn describe-cw-puzzle [cw-puzzle]
          (with-out-str
            (pprint cw-puzzle))))
#?(:clj (defn describe-cw-state [cw-state]
          (with-out-str
            (pprint cw-state))))

(e/defn CrosswordPuzzleCluePane
  [{:keys [cells crossword/clues] :as cw-puzzle}
   cw-state
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
                (dom/text (:cell-num cell) ". " (get clues (cw/word->answer word)))))))))))

(e/defn all-cells
  [cw-puzzle cw-state]
  (e/client
    (cw/all-cells cw-puzzle cw-state)))

(def coord-id
  "Returns the cell element id for a coordinate."
  pr-str)

(defn keydown-event-data
  [e]
  {:key       (.-key e)
   :shift-key (.-shiftKey e)
   :ctrl-key  (.-ctrlKey e)
   :alt-key   (.-altKey e)
   :meta-key  (.-metaKey e)})

(defn- keydown-command
  [{:keys [key shift-key ctrl-key alt-key meta-key]} data]
  (prn 'keydown-command key data)
  (when-not (or ctrl-key alt-key meta-key)
    (case key
      "Tab"        ['select-coord (assoc data :find-coord/directives (if-not shift-key
                                                                       [:find-coord/next-word-in-axis :find-coord/next-word]
                                                                       [:find-coord/prev-word-in-axis :find-coord/prev-word]))]
      "Backspace"  ['set-coord (assoc data :value nil :find-coord/directives [:find-coord/prev-in-word])]
      "Delete"     ['set-coord (assoc data :value nil)]
      "ArrowUp"    ['select-coord (assoc data :find-coord/directives [:find-coord/up :find-coord/prev-in-word :find-coord/prev])]
      "ArrowLeft"  ['select-coord (assoc data :find-coord/directives [:find-coord/left :find-coord/prev-in-word :find-coord/prev])]
      "ArrowDown"  ['select-coord (assoc data :find-coord/directives [:find-coord/down :find-coord/next-in-word :find-coord/next])]
      "ArrowRight" ['select-coord (assoc data :find-coord/directives [:find-coord/right :find-coord/next-in-word :find-coord/next])]
      nil)))

(defn- focus-coord
  [coord]
  (and coord
       #?(:cljs (some-> js/document (.getElementById (coord-id coord)) (.focus)))))

(e/defn CrosswordPuzzleGridPane
  [{:keys [col-count row-count] :as cw-puzzle}
   {:keys [selected-coord] :as cw-state}
   ProcessCmd]
  (e/client
    (dom/div (dom/props {:class ['relative]})
    ;;   (dom/div (dom/props {:class "bg-slate-500" :tabindex 1 :style {:height "50px" :width "50px"}})
    ;;            (dom/on "focus" (e/fn asdfljkasdfh [e](ProcessCmd ['asdfljkasdfh]))))
      (dom/div (dom/props {:class '["-z-1" absolute w-full h-full bg-cover bg-center bg-no-repeat]})
        (dom/style {:background-image "url(https://static-app-misc.teachbanzai.com/img/income-and-expenses-thumb.jpg)"}))
      (dom/div (dom/props {:class '[grid m-6]
                           :style {:grid-template-columns (string/join " " (repeat col-count "3rem"))
                                   :grid-template-rows    (string/join " " (repeat row-count "3rem"))}})
        ;; (prn 'cw-state cw-state)
        (e/for-by first [[[r-idx c-idx :as coord] {:keys [cell-num current-value correct-letter locked]}]
                         (cw/all-cells cw-puzzle cw-state)]
          (let [cell-format (if locked
                              "bg-black border-black"
                              "border-slate-500 border-2")
                text-format (if current-value "text-black" "text-slate-500")
                focused-cell? (= selected-coord coord)]
            (when-not locked
              (dom/div
                (dom/props (cond-> {:id (coord-id coord)
                                    :contenteditable true
                                    :class [text-format cell-format
                                            (when-not locked "border")
                                            'justify-center 'items-center
                                            "text-xl bold grid items-center focus:outline-none focus:ring focus:ring-violet-300 height-[3rem] m-[-1px_-1px_0_0] caret-transparent"]}
                             (not locked) (assoc :tabindex 0)))
                (dom/style {:grid-row-start    (inc r-idx) ; Tailwind doesn't always find generated classes
                            :grid-column-start (inc c-idx)
                            :background        "white"
                            :z-index           1})
                (dom/on "click"   (e/fn [e] #_(js/console.log "click " (.-target e) e) (.focus (.-target e))))
                (dom/on "focus"   (e/fn [_] (new ProcessCmd ['select-coord   {:coord coord}])))
                (dom/on "blur"    (e/fn [_] (new ProcessCmd ['deselect-coord {:coord coord}])))
                (dom/on "input"   (e/fn [e] (let [value (some-> (.-data e) string/upper-case)
                                                  next-coord (cw/find-coord cw-puzzle cw-state coord
                                                               :find-coord/next-in-word :find-coord/next-empty-start-in-axis :find-coord/next-empty-in-axis :find-coord/next-empty)]
                                              (new ProcessCmd ['set-coord {:coord coord, :value value, :move :empty}])
                                              (focus-coord next-coord)
                                              (set! (.-innerText (.-target e)) value)
                                              (.preventDefault e))))
                (dom/on "keydown" (e/fn [e] (when-let [[_ args :as cmd] (keydown-command (keydown-event-data e) {:coord coord})]
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
                  (dom/text (str cell-num))))))))))))

(e/defn CrosswordPuzzle [cw-puzzle cw-state ProcessCmd]
  (e/client
    (dom/div
      (dom/div (dom/props {:class "grid lg:grid-cols-2 m-4"})
        ;; e/watch
        ;; (CrosswordPuzzleCluePane. cw-puzzle cw-state {:words-key :words-in-rows})
        (CrosswordPuzzleGridPane. cw-puzzle cw-state ProcessCmd)
        (CrosswordPuzzleCluePane. cw-puzzle cw-state {:words-key :words}))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-state cw-state)))))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-puzzle cw-puzzle))))))))

(e/defn PuzzlePage [_]
  (e/server
    (let [!cw (atom {:puzzle (cw/comprehend-puzzle cw/example-cw-puzzle)
                     :state  cw/example-cw-state})
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
            (dom/h1 (dom/props {:class "text-xl"}) (dom/text "Crossword in Electric Clojure"))
            (let [{:keys [puzzle state]} (e/server cw)]
              (CrosswordPuzzle. puzzle state ProcessCmd)))))))
