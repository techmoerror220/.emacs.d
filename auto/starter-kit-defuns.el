(TeX-add-style-hook
 "starter-kit-defuns"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("org-preamble-xelatex" "minted")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "org-preamble-xelatex"
    "graphicx"
    "longtable"
    "float")
   (LaTeX-add-labels
    "sec:org4638948"
    "sec:org508797f"
    "sec:orgdd264dd"
    "sec:org0172c37"))
 :latex)

