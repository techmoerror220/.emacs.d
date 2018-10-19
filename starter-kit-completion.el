(add-to-list 'load-path
                   (expand-file-name  "yasnippet"
                                      (expand-file-name "elpa"
                                                        dotfiles-dir)))
    (require 'yasnippet)
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
(require 'company)
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

(message "Starter Kit Completion loaded.")
