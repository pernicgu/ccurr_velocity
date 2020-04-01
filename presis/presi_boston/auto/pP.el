(TeX-add-style-hook
 "pP"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("algorithm2e" "linesnumbered" "ruled" "lined") ("subcaption" "skip=6pt") ("hypcap" "all")))
   (TeX-run-style-hooks
    "inputenc"
    "rotating"
    "fontenc"
    "tabularx"
    "lscape"
    "longtable"
    "makecell"
    "csquotes"
    "relsize"
    "url"
    "xspace"
    "todonotes"
    "amsmath"
    "amssymb"
    "amstext"
    "multirow"
    "eurosym"
    "siunitx"
    "pifont"
    "tikz"
    "pgfplots"
    "graphicx"
    "algorithm2e"
    "subcaption"
    "natbib"
    "hypcap"
    "mathtools"
    "dcolumn"
    "array"
    "expl3"
    "acro"
    "booktabs")
   (TeX-add-symbols
    '("mycommfont" 1)
    '("ttext" 1)))
 :latex)

