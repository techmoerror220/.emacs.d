(TeX-add-style-hook
 "dgm"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
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
    "sec:org8cd8ed8"
    "sec:org863ff6e"
    "sec:orgda2d580"
    "sec:orgc7512c6"
    "sec:org84c78f2"
    "sec:org3b5fc47"
    "sec:org020f908"
    "sec:orgf534a41"
    "sec:orgd55fa23"
    "sec:orga5febaa"
    "sec:orga43a0f1"
    "sec:org2458283"
    "sec:orgc524f34"
    "sec:orgb60c197"
    "sec:org7017f16"
    "sec:org2a6baea"
    "sec:orgc1087dc"
    "sec:org25254fc"
    "sec:org2aa1aca"
    "sec:org5e0f780"
    "sec:orgf9a906b"
    "sec:orgb8feb98"
    "sec:org3b5f8bd"
    "sec:orgd2a9d95"
    "sec:orgbeff46f"
    "sec:org030bffa"
    "sec:org79ec3a5"
    "sec:org1bf3b36"
    "sec:org41e3656"
    "sec:org8b8ee1b"
    "sec:orgbc04fda"
    "sec:org7f91115"
    "sec:org75df10e"
    "sec:org70baa35"
    "sec:org126bb3d"
    "sec:org06660cd"
    "sec:orgb632ef4"
    "sec:org701c182"
    "sec:org8fdc6f6"
    "sec:org7064cf3"
    "sec:org3fb031c"
    "sec:org5879590"
    "sec:orgf911b0a"
    "sec:orgdcd0de6"
    "sec:org574fbc2"
    "sec:org7723383"
    "sec:org7635243"
    "sec:org57e4e2d"
    "sec:orgf23e06b"
    "sec:org1f268c9"
    "sec:orgb26c842"
    "sec:org37fd51f"
    "sec:orgedddb27"
    "sec:org31abf39"
    "sec:org0396f37"
    "sec:org335044f"
    "sec:orgf86061d"
    "sec:orgb0b89c1"
    "sec:orgd6a4c23"
    "sec:org92ffc27"
    "sec:org5a6869a"
    "sec:orgdf43e67"
    "sec:org904129f"
    "sec:org7c7d50a"
    "sec:org7b5bee1"))
 :latex)

