(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (yas-global-mode 1)
  (advice-add 'yas--auto-fill-wrapper :override #'ignore))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet-classic-snippets
  :ensure t)

(use-package ivy-yasnippet
  :ensure t
  :bind ("s-y" . ivy-yasnippet))

;;Use C-TAB to complete. We put this in eval-after-load 
;; because otherwise some modes will try to override our settings.
;;;;;;; (require 'company) ;; commented out by dgm in favor of uncle dave's use-package from https://github.com/daedreth/UncleDavesEmacs

(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
;;  (setq company-idle-delay 0.01)
  (setq company-ech-delay 0)
  (setq company-minimum-prefix-length 3))

;; set to 0 to prevent completion starting automatically 
(setq company-idle-delay 0)
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
;;(require 'company-math)
;; company-mode completions for ess
;; (require 'company-ess)
;;  (add-to-list 'company-backends 'company-math-symbols-unicode)
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
(use-package browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; From https://stackoverflow.com/questions/34652692/how-to-turn-off-company-mode-in-org-mode
(setq company-global-modes '(not python-mode))

;; (semantic-mode 1)
;;(setq helm-semantic-fuzzy-match t
;;      helm-imenu-fuzzy-match t)

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

;; company-c-headers
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux") ;; set style to "linux"

;; dgm comments out as c-mode-map is reported to be a void variable.
(use-package cc-mode)
;;  :init
;;  (define-key c-mode-map  [(tab)] 'company-complete)
;;  (define-key c++-mode-map  [(tab)] 'company-complete))

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
;;;;;; Disabled by DGM so that I don't use company with Python
;;(with-eval-after-load 'company
;;    (add-hook 'python-mode-hook 'company-mode))

;;(use-package company-jedi
;;  :ensure t
;;  :config
;;    (require 'company)
;;    (add-to-list 'company-backends 'company-jedi)
;;    ;(add-hook 'python-mode-hook 'python-mode-company-init)
;;    (add-hook 'python-mode-hook 'company-jedi-setup))

;;(defun python-mode-company-init ()
;; company-jedi and company-etags taken out
;;  (setq-local company-backends '((company-dabbrev-code))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;;(use-package slime
;;  :ensure t
;;  :config
;;  (setq inferior-lisp-program "/usr/bin/sbcl")
;;  (setq slime-contribs '(slime-fancy)))

;; (use-package slime-company
;;  :ensure t
;;  :init
;;    (require 'company)
;;    (slime-setup '(slime-fancy slime-company)))

(use-package company-shell
  :ensure t
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

;; company-etags
(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-dabbrev-code))))

(provide 'starter-kit-completion)

  (message "Starter Kit Completion loaded.")
