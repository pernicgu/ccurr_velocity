(TeX-add-style-hook
 "sourcesanspro"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "OT1" "LY1" "T1")))
   (TeX-run-style-hooks
    "ifxetex"
    "ifluatex"
    "xkeyval"
    "fontspec"
    "fontenc"
    "mweights")
   (TeX-add-symbols
    "SourceSansPro"
    "nativeoldstylenums"))
 :latex)

