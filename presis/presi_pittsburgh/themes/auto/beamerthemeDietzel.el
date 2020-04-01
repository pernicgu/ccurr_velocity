(TeX-add-style-hook
 "beamerthemeDietzel"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("sourcesanspro" "semibold" "lining" "default" "type1") ("fontenc" "T1") ("inputenc" "utf8")))
   (TeX-run-style-hooks
    "etoolbox"
    "tikz"
    "pgfplots"
    "sourcesanspro"
    "ccicons"
    "ifxetex"
    "ifluatex"
    "fontenc"
    "inputenc"
    "biblatex")
   (TeX-add-symbols
    "frame")
   (LaTeX-add-environments
    "wider"))
 :latex)

