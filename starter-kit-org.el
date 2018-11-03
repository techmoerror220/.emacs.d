(setq org-archive-location "/home/dgm/Dropbox/gtd/archive.org::From %s")

(setq org-image-actual-width 550)
(setq org-highlight-latex-and-related '(latex script entities))

(setq org-tags-column 45)

(require 'org-ref)
  (setq reftex-default-bibliography '("/media/dgm/blue/documents/bibs/socbib.bib"))
  (setq org-ref-default-bibliography '("/media/dgm/blue/documents/bibs/socbib.bib"))
  (setq bibtex-completion-bibliography "/media/dgm/blue/documents/bibs/socbib.bib")
  (setq org-latex-pdf-process '("latexmk -pdflatex='xelatex -synctex=1 --shell-escape' -pdf %f"))
;; LaTeX compilation command. For orgmode docs we just always use xelatex for convenience.
;; You can change it to pdflatex if you like, just remember to make the adjustments to the packages-alist below.
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
;; the alternative, if you want a regular pdflatex would be, I think
;; (setq org-latex-pdf-process '("latexmk -pdf %f"))
;; (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex --shell-escape -bibtex -f'  -pdf %f"))

(message "Starter Kit Org loaded.")
