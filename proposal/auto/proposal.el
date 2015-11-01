(TeX-add-style-hook
 "proposal"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "oneside")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "latexrc"
    "hyperref")
   (LaTeX-add-labels
    "fig:initial_position")))

