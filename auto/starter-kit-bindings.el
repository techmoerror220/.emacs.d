(TeX-add-style-hook
 "starter-kit-bindings"
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
    "sec:org7c6f50c"
    "sec:org2a93c09"
    "sec:org1350f6b"
    "sec:org5f136cd"
    "sec:orga91db37"
    "sec:orgb3fbbcc"
    "sec:orgc04408f"
    "sec:org459c894"
    "sec:orgd59c08a"
    "sec:orgcd84af8"
    "sec:org1af4211"
    "sec:orgb2d9353"
    "sec:org9882165"
    "sec:org1877a0e"
    "sec:orgbaca892"
    "sec:org7f33d3f"
    "sec:org34e8632"
    "sec:org135413e"
    "sec:org8631d62"
    "sec:org4dda76b"
    "sec:org6aceb21"
    "sec:org2cacc7f"
    "sec:orgfa46983"
    "sec:org6812fda"
    "sec:orgfdf3c56"
    "sec:org1ed1745"
    "sec:org6ca7cc6"
    "sec:orga8229d5"
    "sec:org1d5b1fe"
    "sec:orga3f4dac"
    "sec:orgccc81cb"
    "sec:org050aea1"
    "sec:org7769256"
    "sec:orgf536882"
    "sec:orge9e5056"
    "sec:org534a624"))
 :latex)

