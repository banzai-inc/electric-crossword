(ns electric-starter-app.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clojure.string :as string]))

;; Saving this file will automatically recompile and update in your browser

(def example-cw-def
  {:crossword/puzzle-string
   "#  TEACH
    #    N
    #    STUDY
    #    W  O
    # G  E  N
    # LEARN E
    # U
    #TEST WORD
    #   A O
    #   P R
    #  DESK"
   :crossword/clues
   {"TEACH"  "Instruct a student"
    "ANSWER" "Response to a question"
    "STUDY"  "Learn from a resource (e.g. book)"
    "GLUE"   "Used to stick one piece of paper to another"
    "LEARN"  "Acquire knowledge"
    "TAPE"   "Sticky strips often coming in rolls"
    "WORK"   "Getting stuff done"
    "WORD"   "Things in sentences."
    "DESK"   "A place to hold your school books and paper while you work."
    "DONE"   "Complete"}})

(def example-cw-state
  {:current {[0 5] "a"}})

(defn- find-words-in-rows
  [{:keys [row-count rows]}]
  ; to-do: consider using recursion instead of volatile!
  (let [words (volatile! [])] ; seq of [r-idx c-idx answer axis]
    (doseq [r-idx (range 0 row-count)]
      (loop [c-idx 0
             ss (re-seq #"\S+|\s+" (nth rows r-idx))]
        (when-let [word-or-spaces (first ss)]
          (when (re-seq #"^\S\S" word-or-spaces)
            (vswap! words conj [r-idx c-idx word-or-spaces :across]))
          (recur
            (+ c-idx (count word-or-spaces))
            (next ss)))))
    @words))

(defn- invert-puzzle-def-light
  "Given a normalized puzzle (all row strings of the appropiate length),
   inverts the rows and columns.
   Only inverts `:row-count, :col-count, :rows`
   other dependent fields may need to be inverted as well."
  [{:keys [row-count col-count rows] :as cw-def}]
  (-> cw-def
    (assoc :row-count col-count
           :col-count row-count
           :rows (mapv (fn restring [c-idx]
                         (apply str (map nth rows (repeat c-idx))))
                       (range col-count)))))


(defn- find-words-in-cols
  [cw-def]
  (let [inverted (invert-puzzle-def-light cw-def)
        inverted-words (find-words-in-rows inverted)]
    (for [[c-idx r-idx word] inverted-words]
      [r-idx c-idx word :down])))

(defn- cell-idx
  [{:keys [col-count] :as _cw-def}
   [r-idx col-idx]]
  (+ (* r-idx col-count) col-idx))

(defn- word->coord
  [[r-idx cell-idx]]
  [r-idx cell-idx])

(defn word->row-index [[r-idx _c-idx _answer _axis]] r-idx)
(defn word->col-index [[_r-idx c-idx _answer _axis]] c-idx)
(defn word->answer    [[_r-idx _c-idx answer _axis]] answer)
(defn word->axis      [[_r-idx _c-idx _answer axis]] axis)

(defn- assoc-cells
  [{:keys [rows] :as cw-def}]
  (assoc cw-def
    :cells (let [coords (sequence (comp
                                   (map word->coord)
                                   (distinct))
                                  (:words cw-def))
                 sorted-coords (sort-by (partial cell-idx cw-def) coords)
                 words-at-coord (group-by word->coord (:words cw-def))
                 first-letter-cells (into (sorted-map)
                                          (zipmap
                                            sorted-coords
                                            (map-indexed (fn [idx coord]
                                                           {:cell-num (inc idx)
                                                            :words    (into #{} (words-at-coord coord []))})
                                                         sorted-coords)))]
            (reduce-kv
              (fn r-words [cells [r-idx c-idx] {:keys [words]}]
                (reduce
                  (fn r-word [cells word]
                    (let [[r* c*] (case (word->axis word) :across [0 1] :down [1 0])]
                      (reduce
                        (fn r-letter [cells [l-idx letter]]
                          (let [coord [(+ r-idx (* r* l-idx))
                                       (+ c-idx (* c* l-idx))]]
                            (-> cells
                             (assoc-in [coord :correct-letter] (str letter))
                             (update-in [coord :words] conj word))))
                        cells
                        (map-indexed vector (word->answer word)))))
                  cells
                  (seq words)))
              first-letter-cells
              first-letter-cells))))

(defn- assoc-words
  [cw-def]
  (as-> cw-def cw-def
   (assoc cw-def
    :words-in-rows (find-words-in-rows cw-def)
    :words-in-cols (find-words-in-cols cw-def))
   (assoc cw-def :words (concat (:words-in-rows cw-def) (:words-in-cols cw-def)))
   (assoc-cells cw-def)))

(defn- comprehend-puzzle-def
  [{:crossword/keys [puzzle-string] :as cw-def}]
  (assert (string? puzzle-string))
  (let [short-rows (re-seq #"(?<=#).*" puzzle-string)
        width (apply max 0 (map count short-rows))
        rows (mapv (fn fill-row-w-extra-spaces [s spaces]
                    (subs (str s spaces) 0 width))
                   short-rows
                   (repeat (apply str (repeat width " "))))
        comprehended (->
                       {:rows      rows
                        :col-count width
                        :row-count (count rows)}
                       (merge cw-def)
                       assoc-words)]
    (-> comprehended)))
        ;(assoc :inverted (invert-puzzle-def-light comprehended)))))

(defn all-cells
  [{:keys [col-count row-count cells rows] :as cw-def}
   cw-state]
  (for [r-idx (range row-count)
        c-idx (range col-count)]
   (let [coord [r-idx c-idx]
         cell (get cells coord {:locked true})]
     [coord
      (assoc cell
        :current-value (get-in cw-state [:current coord]))])))

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
    ;; (dom/h1 (dom/text (name words-key)))))
    (let [grouped-words (group-by word->axis (get cw-def words-key))]
      (dom/ol
        (e/for-by first [[axis words] grouped-words]
           (dom/h3 (dom/props {:class "capitalize"})
             (dom/text (name axis)))
           (dom/ol
             (e/for-by first [[coord word cell]
                              (->> (e/for [word words]
                                     (let [coord (word->coord word)]
                                       [coord word (get cells coord)]))
                                   (sort-by (fn number [[_ _ {:keys [cell-num]}]]cell-num)))]
               (dom/li (dom/text (:cell-num cell) ". " (get clues (word->answer word)))))))))))

(e/defn CrosswordPuzzleGridPane
  [{:keys [col-count row-count] :as cw-def}
   cw-state]
  (e/client
    (dom/div (dom/props {:class ["grid"]
                         :style {:grid-template-columns (string/join " " (repeat col-count "2rem"))
                                 :grid-template-rows    (string/join " " (repeat row-count "2rem"))}})

      (e/for-by first [[coord {:keys [cell-num current-value correct-letter locked]}] (all-cells cw-def cw-state)]
        (let [cell-format (if locked
                            "bg-black border-black"
                            "border-blue-1 bg-gray-1")
              text-format (if current-value "text-black" "text-slate-300")]
          (dom/pre
            (dom/props {:class (str cell-format " border flex items-center justify-center " text-format)})
            (when cell-num
             (dom/sup
              (dom/text (str cell-num))))
            (dom/text (or current-value correct-letter))))))))

(e/defn CrosswordPuzzle [cw-def cw-state]
  (e/client
    (dom/div
      (dom/div (dom/props {:class "grid lg:grid-cols-2 m-4"})
        ;; (CrosswordPuzzleCluePane. cw-def cw-state {:words-key :words-in-rows})
        (CrosswordPuzzleGridPane. cw-def cw-state)
        (CrosswordPuzzleCluePane. cw-def cw-state {:words-key :words}))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-state cw-state)))))
      (dom/pre (dom/code (dom/text (e/server (describe-cw-def cw-def))))))))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (dom/h1 (dom/props {:class "text-xl"}) (dom/text "Crossword in Electric Clojure"))
      (CrosswordPuzzle. (comprehend-puzzle-def example-cw-def) example-cw-state)
      (dom/p (dom/text "Make sure you check the ")
        (dom/a (dom/props {:href "https://electric.hyperfiddle.net/" :target "_blank"})
          (dom/text "Electric Tutorial"))))))
