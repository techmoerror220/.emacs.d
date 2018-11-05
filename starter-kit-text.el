(when (fboundp 'adaptive-wrap-prefix-mode)
    (defun my-activate-adaptive-wrap-prefix-mode ()
      "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
      (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
    (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))
    (global-visual-line-mode t)

;;; original code by Kieran Healy
    ;;; prefer auto-fill to visual line wrap in ESS mode
        ;;    (add-hook 'ess-mode-hook 'turn-on-auto-fill)
        ;;    (add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill) 

;; but for me the auto-fill mode is causing disruption in interactive stata and r sessions so I turn it off.
        ;;    (add-hook 'ess-mode-hook 'turn-off-auto-fill)
        ;;    (add-hook 'inferior-ess-mode-hook 'turn-off-auto-fill) 

    ;;; but turn off auto-fill in tex and markdown
    (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
    (add-hook 'latex-mode-hook 'turn-off-auto-fill)

    ;;; unfill paragraph
    (defun unfill-paragraph ()
    (interactive)
    (let ((fill-column (point-max)))
    (fill-paragraph nil)))
    (global-set-key (kbd "<f6>") 'unfill-paragraph)

    ;; smooth-scrolling 
    ;; (require 'smooth-scrolling) ;; moved to dgm.org

    ;; more smooth efforts.
    (setq-default 
    scroll-conservatively 0
    scroll-up-aggressively 0.01
    scroll-down-aggressively 0.01)

    ;; centered-cursor package in src/
    ;; (and
    ;;  (require 'centered-cursor-mode)
    ;;  (global-centered-cursor-mode +1))

(smartparens-global-mode 1)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)

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
 (defun markdown-preview-file ()
   "run Marked on the current file and revert the buffer"
   (interactive)
   (shell-command
    (format "open -a /Applications/Marked\\ 2.app %s"
            (shell-quote-argument (buffer-file-name))))
   )  
 (global-set-key "\C-co" 'markdown-preview-file) 

(add-hook 'markdown-mode-hook 'latex-unicode-simplified)

(message "Starter Kit Text loaded.")

(require 'deft)
(setq deft-extensions '("org" "txt" "tex" "Rnw" "md" "markdown"))
(add-to-list 'auto-mode-alist '("/deft/.*\\.txt\\'" . org))
(setq deft-directory "/media/dgm/blue/documents/dropbox/notes")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-markdown-mode-title-level 2)
(setq deft-org-mode-title-prefix t)
(global-set-key (kbd "C-x C-g") 'deft-find-file)
(global-set-key  (kbd "C-c d") 'deft)
