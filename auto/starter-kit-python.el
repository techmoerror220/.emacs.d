(TeX-add-style-hook
 "starter-kit-python"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("org-preamble-pdflatex" "minted")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "org-preamble-pdflatex")
   (LaTeX-add-labels
    "sec:orgc3bd256"
    "sec:org0d49514"
    "python"
    "cython"
    "sec:orgf7106ea"
    "sec:org4a46ae0"
    "sec:org5e9bb07"
    "sec:orgd36451d"
    "sec:org7be1fcb"
    "sec:org0b59dc3"
    "sec:orged87da6"))
 :latex)

