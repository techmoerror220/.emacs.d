    (eval-after-load "tex"
      '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk"))
      )
    (eval-after-load "tex"
      '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
      )

  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq-default TeX-engine (quote default))
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(LaTeX-XeTeX-command "latex -synctex=1 -shell-escape")
;; '(TeX-engine (quote default))
 ;; '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
 ;; '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
;; '(blink-cursor-mode nil)
;; '(text-mode-hook (quote (text-mode-hook-identify)))
;; )

(defun org-latex-no-toc (depth)  
  (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
(setq org-latex-format-toc-function 'org-latex-no-toc)

(setq org-latex-default-packages-alist nil)     
;; (setq org-latex-packages-alist
;;        '(("minted" "org-preamble-xelatex" t)
;;          ("" "graphicx" t)
;;          ("" "longtable" nil)
;;          ("" "float" )))

    (require 'ox-latex)

;; (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -synctex=1 --shell-escape -bibtex -f' -pdf %f")) ;; with this it doesn't work
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))  ;; this is Kitchin's way
(setq org-latex-pdf-process (list "latexmk -synctex=1 -shell-escape -bibtex -f -pdf %f"))  ;; this is Kitchin's way customized

;; the alternative, if you want a regular pdflatex would be, I think
;; (setq org-latex-pdf-process '("latexmk -pdf %f"))

(require 'ox-beamer)

(with-eval-after-load 'ox-beamer 
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(("breaklines" "true"))))

(defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

;; fix color handling in org-preview-latex-fragment
(let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
  (plist-put dvipng--plist :use-xcolor t)
  (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))

;; bigger latex fragment
(setq org-format-latex-options (plist-put org-format-latex-options :scale 4))
(setq org-format-latex-options (plist-put org-format-latex-options :foreground "grey"))
;;(setq org-format-latex-options (plist-put org-format-latex-options :html-foreground "grey"))
;;(setq org-format-latex-options (plist-put org-format-latex-options :background "grey"))

(customize-set-value 'org-latex-with-hyperref nil)

(add-to-list 'org-latex-default-packages-alist "\\PassOptionsToPackage{hyphens}{url}")

(provide 'starter-kit-latex-org)

  (message "Starter Kit LaTeX-Org loaded.")
