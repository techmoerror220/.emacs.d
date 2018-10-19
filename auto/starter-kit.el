(TeX-add-style-hook
 "starter-kit"
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
    "sec:org6879f4a"
    "sec:org7bc87bd"
    "sec:org598af80"
    "sec:org92fb562"
    "sec:orga151a74"
    "sec:org2ba3245"
    "sec:org41f1594"
    "sec:org66864f6"
    "sec:org560358c"
    "sec:org7c802b7"
    "sec:org3c34824"
    "sec:org8487038"
    "sec:org7351f9f"
    "sec:org4846c72"
    "sec:orgdb2d957"
    "sec:org683e855"
    "sec:org8311a94"
    "sec:org73d3c1d"
    "sec:org7c24209"
    "sec:org2cce457"
    "sec:org205b56e"
    "sec:orgc515f34"
    "sec:orge40cc45"
    "sec:orgc8e91d6"
    "sec:org8847a74"
    "sec:orgf0717f4"
    "sec:org7d955fe"
    "sec:org9bfb60c"
    "sec:orgbcb1e2b"
    "sec:org97d903d"
    "sec:org03d72da"
    "sec:orgc8f16ae"
    "sec:org49b7242"
    "sec:org997b87c"
    "sec:org00c0034"
    "sec:orgf0b5742"
    "sec:orga42183e"
    "sec:orgd73ad37"
    "org40a5b61"
    "sec:org4f341fc"
    "sec:orgff22f48"
    "sec:org3b2da92"))
 :latex)

