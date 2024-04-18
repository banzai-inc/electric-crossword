(ns banzai.puzzle.crossword.core
  (:require [clojure.string :as string]
            [hashp.core]))

(declare find-coord)

(def example-cw-puzzle
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
    "TEST"   "A graded set of questions for students"
    "TAPE"   "Sticky strips often coming in rolls"
    "WORK"   "Getting stuff done"
    "WORD"   "A part of a sentence."
    "DESK"   "A place to hold your school books and paper while you work."
    "DONE"   "Complete"}})

(def nyt-cw-puzzle-2024-04-18
  {:crossword/puzzle-string
   "#BANJO
    #ALOUD
    #GENIE
    #EXECS
    #LATE"
   :crossword/clues
   {"BANJO"  "Bluegrass instrument"
    "ALOUD"  "Audible way to think"
    "GENIE"  "Aladdin character whose first line is 'Oy! 10,000 years will give you such a crick in the neck`'"
    "EXECS"  "Members of a company's C-suite"
    "LATE"   "Behind schedule"
    "BAGEL"   "The schmear goes here"
    "ALEXA"   "Amazon's assistant"
    "NONET"   "Group of nine"
    "JUICE"   "Batter life, informally"
    "ODES"   "Poems like 'to Autumn' and 'To a Skylark'"}})

(def example-cw-state
  {:current {#_#_[0 5] "A"}
   :axis :across
   :selected-cell [0 0]})

(defn- word->coord
  "Gets the starting coordinate from a word."
  [[r-idx c-idx]]
  [r-idx c-idx])

(defn word->coords
  "Gets the coordinates of a word."
  [[r-idx c-idx answer axis]]
  (let [[r* c*] (case axis :across [0 1] :down [1 0])]
    (map-indexed
      (fn r-letter [l-idx _letter]
        [(+ r-idx (* r* l-idx))
         (+ c-idx (* c* l-idx))])
      answer)))

(defn word->row-index [[r-idx _c-idx _answer _axis]] r-idx)
(defn word->col-index [[_r-idx c-idx _answer _axis]] c-idx)
(defn word->answer    [[_r-idx _c-idx answer _axis]] answer)
(defn word->axis      [[_r-idx _c-idx _answer axis]] axis)


(defmulti mutate
  "Given `{:puzzle {...}, :state {...}} [op arg1...]`
   dispatches on op,
   optionally returning a state-mutation with `:update-state` (an arity-1 fn of old-state to new-state),
   optionally returning a result with `:result`

   ```clojure
   {;:server/effect (fn [](prn 'hi))
    ;:client/effect (fn [](js/console.log 42))
    :update-state (fn [st](assoc st :x 42))
    :result 42}
   ```"
  (fn mutate_dispatch [{:keys [puzzle state]} [op :as _cmd]]op))

(defmethod mutate :default
  [_puz-st [op :as _cmd]]
  (let [msg-parts ["Warning:" "operation isn't supported:" op]]
    #?(:cljs (apply js/console.warn msg-parts)
       :default (binding [*out* *err*](apply println msg-parts)))))

(defn update-axis
  [{:keys [selected-coord axis] :as st} {:keys [cells] :as _puzzle}]
  (let [axises-at-coord (into #{} (map word->axis) (get-in cells [selected-coord :words]))]
    (cond-> st
        (not (contains? axises-at-coord axis)) (assoc :axis (first (seq axises-at-coord))))))

(defn update-selected-word
  [{:keys [selected-coord axis] :as st} {:keys [cells] :as _puzzle}]
  (let [word (some #(and (= axis (word->axis %)) %)
                   (get-in cells [selected-coord :words]))]
    (assoc st :selected-word word)))

(defmethod mutate 'deselect-coord deselect-coord
  [_puz-st [op {:keys [coord]}]]
  {:update-state (fn update-st [{:keys [selected-coord] :as st}]
                   (cond-> st
                     (= coord selected-coord) (dissoc :selected-coord)))})

(defmethod mutate 'select-coord select-coord
  [{:keys [puzzle]} [_op {:keys [coord axis]}]]
  {:update-state (fn update-st [st]
                   (-> st
                       (assoc :selected-coord coord)
                       (update :axis #(or axis %))
                       (update-axis puzzle)
                       (update-selected-word puzzle)))
   :result (pr-str "selecting" coord)})

(defmethod mutate 'set-coord set-coord
  [{:keys [puzzle]} [_op {:keys [coord value] :as data}]]
  (-> (select-keys data [:client/effect])
      (assoc :update-state (fn update-st [st]
                             (-> st
                                 (assoc-in [:current coord] value)))
                                ;;  (update-axis puzzle)))
             :result (pr-str "set" coord "to" value))))

(defmethod mutate 'set-axis set-axis
  [_puz-st [op {:keys [axis] :as data}]]
  (-> (select-keys data [:client/effect])
    (assoc :update-state (fn update-st [st]
                           (update st :current-axis #(or axis ({:across :down, :down :across} % :across)))))))

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

(defn- invert-puzzle-puzzle-light
  "Given a normalized puzzle (all row strings of the appropiate length),
   inverts the rows and columns.
   Only inverts `:row-count, :col-count, :rows`
   other dependent fields may need to be inverted as well."
  [{:keys [row-count col-count rows] :as cw-puzzle}]
  (-> cw-puzzle
    (assoc :row-count col-count
      :col-count row-count
      :rows (mapv (fn restring [c-idx]
                    (apply str (map nth rows (repeat c-idx))))
              (range col-count)))))

(defn- find-words-in-cols
  [cw-puzzle]
  (let [inverted (invert-puzzle-puzzle-light cw-puzzle)
        inverted-words (find-words-in-rows inverted)]
    (for [[c-idx r-idx word] inverted-words]
      [r-idx c-idx word :down])))

(defn- cell-idx
  [{:keys [col-count] :as _cw-puzzle}
   [r-idx col-idx]]
  (+ (* r-idx col-count) col-idx))

(defn- assoc-cells
  [cw-puzzle]
  (assoc cw-puzzle
    :cells (let [coords (sequence (comp
                                    (map word->coord)
                                    (distinct))
                                  (:words cw-puzzle))
                 sorted-coords (sort-by (partial cell-idx cw-puzzle) coords)
                 words-at-coord (group-by word->coord (:words cw-puzzle))
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
  [cw-puzzle]
  (as-> cw-puzzle cw-puzzle
    (assoc cw-puzzle
      :words-in-rows (find-words-in-rows cw-puzzle)
      :words-in-cols (find-words-in-cols cw-puzzle))
    (assoc cw-puzzle :words (concat (:words-in-rows cw-puzzle) (:words-in-cols cw-puzzle)))
    (assoc-cells cw-puzzle)))

(defn comprehend-puzzle
  [{:crossword/keys [puzzle-string] :as cw-puzzle}]
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
                       (merge cw-puzzle)
                       assoc-words)]
    comprehended))

(defn all-cells
  [{:keys [col-count row-count cells] :as _cw-puzzle}
   cw-state]
  (for [r-idx (range row-count)
        c-idx (range col-count)]
    (let [coord [r-idx c-idx]
          cell (get cells coord {:locked true})]
      [coord
       (assoc cell
         :current-value (get-in cw-state [:current coord]))])))

(defn find-coord
  [cw-puzzle
   {:keys [axis] :as cw-state}
   coord
   & directives]
  (let [[before [[_ curr-entry] & after]]
        (->> (all-cells cw-puzzle cw-state)
             (split-with (fn sw-not-equal [[_coord]](not= _coord coord)))
             (remove (fn r-locked [[_coord {:keys [locked]}]] locked)))
        curr-word (delay (->> curr-entry :words (some #(and (= axis (word->axis %)) %))))
        editable-coord (fn editable-coord [coord](some-> cw-puzzle :cells (get coord) :words (and coord)))]
    (loop [[directive & directives] (seq directives)]
      (if-not directive
        coord
        (or
         (condp = directive
           :find-coord/current coord
           :find-coord/prev (first (last before))
           :find-coord/next (first (first after))
           :find-coord/left (editable-coord (update coord 1 dec))
           :find-coord/right (editable-coord (update coord 1 inc))
           :find-coord/up (editable-coord (update coord 0 dec))
           :find-coord/down (editable-coord (update coord 0 inc))
           :find-coord/prev-in-word
           (->> (some (fn prev-in-word [n-coord]
                        (and (neg? (compare n-coord coord)) n-coord))
                      (reverse (word->coords @curr-word))))
           :find-coord/next-in-word
           (->> (some (fn next-in-word [n-coord]
                        (and (pos? (compare n-coord coord)) n-coord))
                      (word->coords @curr-word)))
           :find-coord/prev-word-in-axis
           (->> (concat (reverse before))
                (some (fn f-nxt-empty [[n-coord {:keys [words cell-num]}]]
                        (and cell-num
                             (some #(= axis (word->axis %)) words)
                             n-coord))))
           :find-coord/next-word-in-axis
           (->> (concat after before)
                (some (fn f-nxt-empty [[n-coord {:keys [words cell-num]}]]
                        (and cell-num
                             (some #(= axis (word->axis %)) words)
                             n-coord))))
           :find-coord/prev-word
           (->> (concat (reverse before))
                (some (fn f-nxt-empty [[n-coord {:keys [cell-num]}]]
                        (and cell-num n-coord))))
           :find-coord/next-word
           (->> (concat after before)
                (some (fn f-nxt-empty [[n-coord {:keys [cell-num]}]]
                        (and cell-num n-coord))))
           :find-coord/next-empty-start-in-axis
           (->> (concat after before)
                (some (fn f-nxt-empty [[n-coord {:keys [words current-value cell-num]}]]
                        (and cell-num
                             (string/blank? current-value)
                             (some #(= axis (word->axis %)) words)
                             n-coord))))
           :find-coord/next-empty-in-axis
           (->> (concat after before)
                (some (fn f-nxt-empty [[n-coord {:keys [words current-value]}]]
                        (and (string/blank? current-value)
                             (some #(= axis (word->axis %)) words)
                             n-coord))))
           :find-coord/next-empty
           (->> (concat after before)
                (some (fn f-nxt-empty [[n-coord {:keys [words current-value]}]]
                        (and (string/blank? current-value) words n-coord)))))
         (recur directives))))))
