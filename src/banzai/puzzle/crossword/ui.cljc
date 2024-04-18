(ns banzai.puzzle.crossword.ui
  (:require #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:cljs [banzai.puzzle.crossword.ui.browser :as ui-browser])))

#?(:clj (defn describe-cw-puzzle [cw-puzzle]
          (with-out-str
            (pprint cw-puzzle))))
#?(:clj (defn describe-cw-state [cw-state]
          (with-out-str
            (pprint cw-state))))

(def coord-id
  "Returns the cell element id for a coordinate."
  #?(:cljs ui-browser/coord-id
     :default pr-str))

(defn keydown-command
  [{:keys [key shift-key ctrl-key alt-key meta-key]} data]
  (when-not (or ctrl-key alt-key meta-key)
    (case key
      "Tab"        ['select-coord (assoc data :find-coord/directives (if-not shift-key
                                                                       [:find-coord/next-word-in-axis :find-coord/next-word]
                                                                       [:find-coord/prev-word-in-axis :find-coord/prev-word]))]
      "Backspace"  ['set-coord (assoc data :value nil :find-coord/directives [:find-coord/prev-in-word :find-coord/prev])]
      "Delete"     ['set-coord (assoc data :value nil)]
      "ArrowUp"    ['select-coord (assoc data :find-coord/directives [:find-coord/up :find-coord/prev-in-word :find-coord/prev])]
      "ArrowLeft"  ['select-coord (assoc data :find-coord/directives [:find-coord/left :find-coord/prev-in-word :find-coord/prev])]
      "ArrowDown"  ['select-coord (assoc data :find-coord/directives [:find-coord/down :find-coord/next-in-word :find-coord/next])]
      "ArrowRight" ['select-coord (assoc data :find-coord/directives [:find-coord/right :find-coord/next-in-word :find-coord/next])]
      nil)))

(defn focus-coord
  [coord]
  (and coord
       #?(:cljs (ui-browser/focus-coord coord)
          :default (prn 'focus-coord/not-implemented))))
