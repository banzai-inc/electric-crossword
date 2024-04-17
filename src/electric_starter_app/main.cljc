(ns electric-starter-app.main
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [banzai.puzzle.crossword.electric :as ecw]))

(e/defn Main [ring-request]
  (e/client
    (binding [#?@(:cljs [dom/node js/document.body])]
     (dom/div
      (ecw/PuzzlePage. ring-request)
      (dom/p (dom/text "Make sure you check the ")
        (dom/a (dom/props {:href "https://electric.hyperfiddle.net/" :target "_blank"})
          (dom/text "Electric Tutorial")))))))
