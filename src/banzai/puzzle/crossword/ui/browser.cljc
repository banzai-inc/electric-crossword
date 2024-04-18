(ns banzai.puzzle.crossword.ui.browser)

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

(defn focus-coord
  [coord]
  (and coord
       #?(:cljs (some-> js/document (.getElementById (coord-id coord)) (doto (.focus)) boolean)
          :default false)))
