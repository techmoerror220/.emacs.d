(TeX-add-style-hook
 "starter-kit-completion"
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
    "sec:org52c8155"
    "sec:org11fd7f2"
    "sec:orgb94f0b2"
    "sec:orgc6e804c"
    "sec:orgafb9581"
    "sec:orgbf8bc77"
    "sec:orgfd3f39f"
    "sec:org4185fc1"
    "sec:orgf48acc1"
    "sec:org8d5bf3b"
    "sec:org3442943"
    "sec:org8920fd7"
    "sec:org834b6ce"
    "sec:org5a94adf"))
 :latex)

