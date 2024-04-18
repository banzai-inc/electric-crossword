(ns electric-starter-app.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [banzai.puzzle.crossword.core :as cw]
            [banzai.puzzle.crossword.electric :as ecw]))

(e/defn Main [ring-request]
  (e/client
    (binding [#?@(:cljs [dom/node js/document.body])]
     (dom/div
      (ecw/PuzzlePage. ring-request
        cw/nyt-cw-puzzle-2024-04-18
        nil)
      (ecw/PuzzlePage. ring-request
        nil
        nil)))))
