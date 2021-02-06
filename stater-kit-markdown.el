(use-package markdown-mode
  :defer t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("\\.Rmd\\'" . markdown-mode)
   ("\\.rmd\\'" . markdown-mode)
   ("\\.Rmw\\'" . markdown-mode))
  :init 
  (defvar markdown-electric-pairs '(
                                    (?* . ?*)
                                    (?` . ?`)
                                    (?$ . ?$)
                                    (?' . ?')
                                    ) "Electric pairs for markdown-mode.")

  (defun markdown-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook 'markdown-add-electric-pairs)
  :config
  (setq markdown-command "markdown")
  ;; DGM trying to make line-move-visual work with visual lines rather than logical lines (nil) (manual, p.19)
  (setq line-move-visual t))

;; Add yaml to markdown an .yml files
(use-package yaml-mode
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)))

;;;Insert new chunk for Rmarkdown
(defun kjh-insert-r-chunk (header) 
  "Insert an r-chunk in markdown mode." 
  (interactive "sLabel: ") 
  (insert (concat "```{r " header "}\n\n```")) 
  (forward-line -1))

;; (global-set-key (kbd "C-c i") 'kjh-insert-r-chunk)

(autoload 'markdown-mode "markdown-mode"
      "Major mode for editing Markdown files" t)
      (setq auto-mode-alist
      (cons '("\\.Markdown" . markdown-mode) auto-mode-alist)
      )
      (setq auto-mode-alist
      (cons '("\\.MarkDown" . markdown-mode) auto-mode-alist)
      )
      (setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist)
      )
      (setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist)
      )
      (setq auto-mode-alist
      (cons '("README\\.md" . gfm-mode) auto-mode-alist)
      )

   ;; This function will open Marked.app and monitor the current markdown document
   ;; for anything changes.  In other words, it will live reload and convert the
   ;; markdown documment
   ;; DGM comments this out as this Marked.app is for Mac Os
;;   (defun markdown-preview-file ()
;;     "run Marked on the current file and revert the buffer"
;;     (interactive)
;;     (shell-command
;;      (format "open -a /Applications/Marked\\ 2.app %s"
;;              (shell-quote-argument (buffer-file-name))))
;;     )  
;;   (global-set-key "\C-co" 'markdown-preview-file) 


;; Disabled on 12 jan 21 as the function <latex-unicode-simplified> is not known 
;; (add-hook 'markdown-mode-hook 'latex-unicode-simplified)

(defvar markdown-cite-format)
(setq markdown-cite-format
      '(
        (?\C-m . "[@%l]")
        (?p . "[@%l]")
        (?t . "@%l")
        )
      )

(defun markdown-reftex-citation ()
  (interactive)
  (let ((reftex-cite-format markdown-cite-format)
        (reftex-cite-key-separator "; @"))
    (reftex-citation)))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c [" 'markdown-reftex-citation)))

(use-package pandoc-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'TeX-mode-hook 'pandoc-mode)  
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
;; (global-set-key (kbd "C-c C-p") 'pandoc-main-hydra/body) ;; not sure it is taken

(provide 'starter-kit-polymode)

(message "Starter Kit Polymode file loaded.")
