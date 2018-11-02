(add-to-list 'load-path
               (expand-file-name  "yasnippet"
                                  (expand-file-name "elpa"
                                                    dotfiles-dir)))
  ;;    (require 'yasnippet)
  ;; above line replaced with the following use-package from uncle dave
  ;; https://github.com/daedreth/UncleDavesEmacs
  (use-package yasnippet
    :ensure t)

;;  (use-package yasnippet-snippets
;;      :ensure t)

  ;; Setting yas-indent-line to =’fixed= fixes Python indentation behavior when typing a templated snippet. 
  ;; Tip from https://github.com/danielmai/.emacs.d/blob/master/config.org
  (setq yas-indent-line 'fixed)

  ;;  (yas-set-ac-modes)
  ;;  (yas-enable-emacs-lisp-paren-hack)
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"
                           "~/.emacs.d/snippets"
                           "~/.emacs.d/elpa/yasnippet-classic-snippets-1.0.2/snippets"
                           "~/.emacs.d/elpa/yasnippet-snippets-20180909.1015/snippets"
                           "~/.emacs.d/elpa/"))

  ;;   (yas-load-directory (expand-file-name "snippets" dotfiles-dir))  ;; original line from kieran healy

  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  (global-set-key [tab] 'tab-indent-or-complete)

;;Use C-TAB to complete. We put this in eval-after-load 
;; because otherwise some modes will try to override our settings.
;;;;;;; (require 'company) ;; commented out by dgm in favor of uncle dave's use-package from https://github.com/daedreth/UncleDavesEmacs

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

;; set to 0 to prevent completion starting automatically 
;; (setq company-idle-delay 0)
;; cancel if input doesn't match
(setq company-require-match nil)
;; complete using C-TAB
(global-set-key (kbd "<C-tab>") 'company-complete)
;; use C-n and C-p to cycle through completions
;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;; enable math completions
(require 'company-math)
;; company-mode completions for ess
;; (require 'company-ess)
(add-to-list 'company-backends 'company-math-symbols-unicode)
;;(add-to-list 'company-backends 'company-math-symbols-latex)
;; put company-capf at the beginning of the list
(require 'company-capf)
(setq company-backends
      (delete-dups (cons 'company-capf company-backends)))

;; ;; disable dabbrev
;; (delete 'company-dabbrev company-backends)
;; (delete 'company-dabbrev-code company-backends)


(add-hook 'after-init-hook 'global-company-mode)

;; completion for kill ring history
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(with-eval-after-load 'company
    (add-hook 'python-mode-hook 'company-mode))

(use-package company-jedi
  :ensure t
  :config
    (require 'company)
    (add-to-list 'company-backends 'company-jedi))

(defun python-mode-company-init ()
  (setq-local company-backends '((company-jedi
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-jedi
  :ensure t
  :config
    (require 'company)
    (add-hook 'python-mode-hook 'python-mode-company-init))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; (use-package slime
;;  :ensure t
;;  :config
;;  (setq inferior-lisp-program "/usr/bin/sbcl")
;;  (setq slime-contribs '(slime-fancy)))

;; (use-package slime-company
;;  :ensure t
;;  :init
;;    (require 'company)
;;    (slime-setup '(slime-fancy slime-company)))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :ensure t
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))

(message "Starter Kit Completion loaded.")
