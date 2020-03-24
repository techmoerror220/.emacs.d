(use-package markdown-mode
    :ensure t
    :mode
    (("README\\.md\\'" . gfm-mode)
     ("\\.md\\'" . markdown-mode)
     ("\\.markdown\\'" . markdown-mode))
    :init
    (setq markdown-command "markdown")
    )

  (use-package polymode
    :ensure markdown-mode
    :ensure poly-R
    :ensure poly-noweb
    :config
    ;; R/tex polymodes
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
    ;; org-mode poly (not working at the moment)
    ;; (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))
    ;; Make sure r-mode is loaded
    ;; (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
    ;; Add a chunk for rmarkdown
    ;; Need to add a keyboard shortcut
    ;; https://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
    ;; (defun insert-r-chunk (header) 
    ;;   "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
    ;;   (interactive "sHeader: ") 
    ;;   (insert (concat "```{r " header "}\n\n\n```")) 
    ;;   (forward-line -2))
    ;; (define-key poly-markdown+r-mode-map (kbd "M-c") #'insert-r-chunk)
    )


  (use-package poly-markdown
    :ensure polymode
    :defer t
    )

;;  Originally, the above had: 
;;  :config
;;  ;; Wrap lines at column limit, but don't put hard returns in
;;  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode -1)))
;;  ;; Flyspell on
;;  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode -1))) 
;;  ;; Add highligh-symbol 
;;  (add-hook 'markdown-mode-hook (lambda () (highlight-symbol-mode 1)))  ;; now in =dgm.org=

  ;; poly-R
  (use-package poly-R
    :ensure polymode
    :ensure poly-markdown
    :ensure poly-noweb
    :defer t
  )

;; Originally the above had:
    ;; :config
    ;; Add a chunk for rmarkdown
    ;; Need to add a keyboard shortcut
    ;; https://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
    ;; (defun insert-r-chunk (header) 
    ;;   "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
    ;;   (interactive "sHeader: ") 
    ;;   (insert (concat "```{r " header "}\n\n\n```")) 
    ;;   (forward-line -2))
    ;; (define-key poly-markdown+r-mode-map (kbd "M-c") #'insert-r-chunk)


  ;; Add yaml to markdown an .yml files
  (use-package yaml-mode
    :ensure t
    :mode (("\\.yml\\'" . yaml-mode)))

(defun my_pipe_operator ()
  "R/ESS %>% operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(define-key ess-mode-map (kbd "M-_") 'my_pipe_operator)
(define-key inferior-ess-mode-map (kbd "M-_") 'my_pipe_operator)

;;;Insert new chunk for Rmarkdown
(defun kjh-insert-r-chunk (header) 
  "Insert an r-chunk in markdown mode." 
  (interactive "sLabel: ") 
  (insert (concat "```{r " header "}\n\n```")) 
  (forward-line -1))

(global-set-key (kbd "C-c i") 'kjh-insert-r-chunk)

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

 (add-hook 'markdown-mode-hook 'latex-unicode-simplified)

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
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'TeX-mode-hook 'pandoc-mode)  
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
;; (global-set-key (kbd "C-c C-p") 'pandoc-main-hydra/body) ;; not sure it is taken

(provide 'starter-kit-polymode)

(message "Starter Kit Polymode file loaded.")
