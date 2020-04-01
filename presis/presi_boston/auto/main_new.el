(TeX-add-style-hook
 "main_new"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "Sections/pP"
    "Sections/pCmd"
    "Sections/pAc"
    "beamer"
    "beamer10")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

