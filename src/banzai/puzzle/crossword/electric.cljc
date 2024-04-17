(ns banzai.puzzle.crossword.electric
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [banzai.puzzle.crossword.core :as cw]
            [clojure.string :as string]))

#?(:clj (defonce cw-atom (atom {:def (cw/comprehend-puzzle-def cw/example-cw-def)
                                :state cw/example-cw-state})))

#?(:clj (defn describe-cw-def [cw-def]
          (with-out-str
            (pprint cw-def))))
#?(:clj (defn describe-cw-state [cw-state]
          (with-out-str
            (pprint cw-state))))

(e/defn CrosswordPuzzleCluePane
  [{:keys [cells crossword/clues] :as cw-def}
   cw-state
   {:keys [words-key] :or {words-key :words}}]
  (e/client
    (let [grouped-words (group-by cw/word->axis (get cw-def words-key))]
      (dom/ol
        (e/for-by first [[axis words] grouped-words]
          (dom/h3 (dom/props {:class "capitalize"})
            (dom/text (name axis)))
          (dom/ol
            (e/for-by first [[coord word cell]
                             (->> (e/for [word words]
                                    (let [coord (cw/word->coord word)]
                                      [coord word (get cells coord)]))
                               (sort-by (fn number [[_ _ {:keys [cell-num]}]] cell-num)))]
              (dom/li (dom/text (:cell-num cell) ". " (get clues (cw/word->answer word)))))))))))

(e/defn CrosswordPuzzleGridPane
  [{:keys [col-count row-count] :as cw-def}
   cw-state]
  (e/client
    (dom/div (dom/props {:class ['relative]})
      (dom/div (dom/props {:class '["-z-1" absolute w-full h-full bg-cover bg-center bg-no-repeat]})
        (dom/style {:background-image "url(https://static-app-misc.teachbanzai.com/img/income-and-expenses-thumb.jpg)"}))
      (dom/div (dom/props {:class ["grid"]
                           :style {:grid-template-columns (string/join " " (repeat col-count "3rem"))
                                   :grid-template-rows    (string/join " " (repeat row-count "3rem"))}})
        (e/for-by first [[[r-idx c-idx :as coord] {:keys [cell-num current-value correct-letter locked]}]
                         (cw/all-cells cw-def cw-state)]
          (let [cell-format (if locked
                              "bg-black border-black"
                              "border-slate-500 border-2")
                text-format (if current-value "text-black" "text-slate-500")]
            (when-not locked
              (dom/div
                (dom/props (cond-> {:class [text-format cell-format
                                            (str "row-start-" (inc r-idx))
                                            (str "col-start-" (inc c-idx))
                                            (when-not locked "border")
                                            "text-xl bold grid items-center focus:outline-none focus:ring focus:ring-violet-300 height-[3rem] m-[-1px_-1px_0_0]"]}
                             (not locked) (assoc :tabindex 0)))
                (dom/on! "blur" (fn [e] #?(:cljs (js/alert (pr-str e)))))
                (dom/style {:background "white", :z-index 1})
                (when cell-num
                  (dom/sup (dom/props {:class ['row-1 'col-1 'text-slate-800 'relative 'left-0 'top-1]})

                    (dom/text (str cell-num))))
                (dom/div (dom/props {:class ['grid 'row-1 'col-1 'w-full 'h-full 'relative 'justify-center 'items-center]})
                  (dom/span (dom/props {:class [text-format]})
                    (dom/text
                      (or current-value correct-letter))))))))))))

(e/defn CrosswordPuzzle [cw-def cw-state]
  (e/client
    (dom/div
      (dom/div (dom/props {:class "grid lg:grid-cols-2 m-4"})
        ;; (CrosswordPuzzleCluePane. cw-def cw-state {:words-key :words-in-rows})
        (CrosswordPuzzleGridPane. cw-def cw-state)
        (CrosswordPuzzleCluePane. cw-def cw-state {:words-key :words}))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-state cw-state)))))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-def cw-def))))))))

(e/defn PuzzlePage [_]
  (e/client
    (dom/h1 (dom/props {:class "text-xl"}) (dom/text "Crossword in Electric Clojure"))
    (let [{cw-def :def, cw-state :state} (e/server @cw-atom)]
      (CrosswordPuzzle. cw-def cw-state))))

(e/defn PuzzlePage2 [_]
  (e/client (dom/p (dom/text "Make sure you check the "))))
