;; dgm added customizations intended for modeline, following https://github.com/bbatsov/solarized-emacs

;; make the modeline high contrast
 (setq solarized-high-contrast-mode-line t)

;; powerline by milkman
(require 'powerline)
(display-time-mode 1)
(powerline-default-theme)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
  (setq custom-safe-themes t)
;;  (load-theme 'zenburn t)
(load-theme 'solarized-dark t)
;;  (load-theme 'solarized-light t)
;;  (load-theme 'darktooth t)
;;  (load-theme 'soothe t)
;;  (load-theme 'clues t)

;; from https://github.com/kuanyui/moe-theme.el
;;    (require 'moe-theme)
;;    (powerline-moe-theme)

    ;; Show highlighted buffer-id as decoration. (Default: nil)
;;    (setq moe-theme-highlight-buffer-id t)

    ;; Resize titles (optional).
;;    (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
;;    (setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;;    (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))

    ;; Choose a color for mode-line.(Default: blue)
;;    (moe-theme-set-color 'orange)

    ;; Finally, apply moe-theme now.
    ;; Choose what you like, (moe-light) or (moe-dark)
;;    (moe-dark)    

;; If you use Emacs build-in show-paren-mode, I recommend set the value of show-paren-style to expression for optimized visual experience:

;;    (show-paren-mode t)
;;    (setq show-paren-style 'expression)

;; trying to improve the looks of dired+ with solarized dark: http://unix.stackexchange.com/questions/20519/dired-on-dark-color-themes
;; (add-to-list 'default-frame-alist '(background-mode . dark))
;;  (load-theme 'spacemacs-dark t)
;;  (load-theme 'misterioso t)

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'switch-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(when window-system
;;      (setq frame-title-format '(buffer-file-name "%f" ("%b")))
      (setq frame-title-format (concat "%b" (unless (daemonp) " [serverless]"))) ;; from ambrevar's main.el
      (tooltip-mode -1)
      (tool-bar-mode -1)
      (blink-cursor-mode -1))

    (mouse-wheel-mode t)

  ;; dgm comments this out as it is in init.el already
  ;;  (set-language-environment 'utf-8)
  ;;  (set-terminal-coding-system 'utf-8)
  ;;  (set-keyboard-coding-system 'utf-8)
  ;;
  ;;  (setq locale-coding-system 'utf-8)
  ;;  (set-default-coding-systems 'utf-8)
  ;;  (set-selection-coding-system 'utf-8)
  ;;  (prefer-coding-system 'utf-8)

    (setq visible-bell t
          echo-keystrokes 0.1
          font-lock-maximum-decoration t
          font-lock-verbose nil
          inhibit-startup-message t
          transient-mark-mode t
        ;;  color-theme-is-global t
          delete-by-moving-to-trash t
          shift-select-mode nil
          truncate-partial-width-windows nil
          uniquify-buffer-name-style 'forward
          whitespace-style '(trailing lines space-before-tab
                                      indentation space-after-tab)
          whitespace-line-column 100
          ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally
          oddmuse-directory (concat dotfiles-dir "oddmuse")
          xterm-mouse-mode t
          save-place-file (concat dotfiles-dir "places"))

(minimal-mode)

;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(auto-compression-mode t)

;;(global-font-lock-mode t)

;;  (if window-system
;      (menu-bar-mode t)
;;      (menu-bar-mode -1)
;;      )

;; Code by Kieran Healy:
;; saveplace remembers your location in a file when saving files
;;  (require 'saveplace)
;;  (setq-default save-place t) for Emacs below 24.4
;;  (toggle-save-place-globally 1) ;; in Emacs above 24.4

;; My code
;; (require 'saveplace)
;; I comment saveplace out because in the documentation it says: "For GNU Emacs 25.1 and newer versions
;; Note that saveplace is auto-loaded by save-place-mode. So you do not need to explicitly require it.

  (save-place-mode 1)

;;  (when (> emacs-major-version 21)
;;    (require 'flx-ido) 
;;    (ido-mode t)
;;    (ido-everywhere 1)
;;    (setq ido-enable-prefix nil
;;          ido-enable-flex-matching t
;;          ido-create-new-buffer 'always
;;          ido-use-filename-at-point nil
;;          ido-use-faces nil
;;          ido-max-prospects 10))

(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil 
        ido-use-faces nil             
        ido-max-prospects 10
        ido-everywhere t
        ido-mode t)
  (use-package flx-ido
    :ensure t) 
  (use-package ido-vertical-mode
    :ensure t
    :defer t
    :init (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(set-default 'indent-tabs-mode nil)
  (set-default 'indicate-empty-lines t)
  (set-default 'imenu-auto-rescan t)
  
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; dgm, 1 july 2017: turn flyspell off
;;  (add-hook 'text-mode-hook 'turn-on-flyspell)
;;  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
;;  (add-hook 'markdown-mode-hook 'turn-on-flyspell)
;;  (add-hook 'org-mode-hook 'turn-on-flyspell)
  
  (defvar starter-kit-coding-hook nil
    "Hook that gets run on activation of any programming mode.")
  
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Seed the random-number generator
  (random t)

;; Istan Zahn uses this instea: (https://github.com/izahn/dotemacs)
;; Use y/n instead of yes/no
;; (fset 'yes-or-no-p 'y-or-n-p)

(defun starter-kit-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;;  (require 'powerline)
;;  (powerline-default-theme)

(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(setq diff-switches "-u")

(message "Starter Kit Misc loaded.")
