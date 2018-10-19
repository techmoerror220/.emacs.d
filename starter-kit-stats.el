(require 'ess-site)

(add-hook 'ess-mode-hook 'run-starter-kit-coding-hook)
  (add-hook 'ess-R-post-run-hook 'smartparens-mode)

(setq ess-swv-processor "'knitr")

(require 'poly-R)
(require 'poly-markdown)
;;; polymode + markdown
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; polymode + R
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1 nil t))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
	(call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key [C-up] 'comint-previous-input)
	       (local-set-key [C-down] 'comint-next-input)))
 (add-hook 'Rnw-mode-hook 
          '(lambda() 
             (local-set-key [(shift return)] 'my-ess-eval))) 
  (require 'ess-site)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;  (add-hook 'after-init-hook #'global-flycheck-mode)
;;    (add-hook 'ess-mode-hook
;;              (lambda () (flycheck-mode t)))

(setq ess-smart-S-assign-key ";")
(ess-toggle-S-assign nil)
(ess-toggle-S-assign nil)
(ess-toggle-underscore nil) ; leave underscore key alone!

(message "Starter Kit main file loaded.")
