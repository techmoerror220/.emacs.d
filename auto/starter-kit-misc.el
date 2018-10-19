(TeX-add-style-hook
 "starter-kit-misc"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:org3d6bac6"
    "sec:orge1593d5"
    "sec:orge3ffc4d"
    "sec:org653c9d5"
    "sec:org43c3662"
    "sec:org6bdf174"
    "sec:orgcbc8e43"
    "sec:org7b9f453"
    "sec:orgd0cb353"
    "sec:org9d787db"
    "sec:org3135231"
    "sec:orgc42b271"
    "sec:org66f73a0"
    "sec:org4a6c173"
    "sec:org7813fc8"
    "sec:org70d0512"
    "sec:orgd7d71d3"
    "sec:org3f67c8e"))
 :latex)

