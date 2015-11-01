(TeX-add-style-hook
 "latexrc"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "amsfonts"
    "amsmath"
    "amsthm"
    "amssymb"
    "amstext"
    "calc"
    "cancel"
    "fullpage"
    "tikz"
    "soul"
    "tikz-cd"
    "graphicx"
    "multicol"
    "color"
    "enumerate"
    "bm"
    "inputenc"
    "bbm")
   (TeX-add-symbols
    '("whyeq" 1)
    '("eqblU" 1)
    '("MyBl" 1)
    '("dbyd" 2)
    '("sqrtfrac" 2)
    '("avg" 1)
    '("abs" 1)
    '("Abs" 1)
    '("norm" 1)
    '("set" 1)
    '("SES" 5)
    '("mrm" 1)
    '("mc" 1)
    '("bb" 1)
    '("textcirc" 1)
    "poof"
    "npoof"
    "pro"
    "npro"
    "defn"
    "ndefn"
    "nmark"
    "thm"
    "nthm"
    "lemon"
    "nlemon"
    "enum"
    "nenum"
    "cor"
    "ncor"
    "exam"
    "nexam"
    "E"
    "A"
    "tooo"
    "Thus"
    "thus"
    "into"
    "mto"
    "onto"
    "g"
    "ep"
    "ph"
    "w"
    "im"
    "eval"
    "ins"
    "by"
    "oby"
    "what"
    "inv")
   (LaTeX-add-environments
    "theorem"
    "proposition"
    "corollary"
    "lemma"
    "conjecture"
    "truefact"
    "definition"
    "remark"
    "example"
    "examples")))

