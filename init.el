;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))

;; Common Lisp compatability
(require 'cl-lib)

;; Temporary workaround for eshell bug in 24.3.1
;; http://zpcat.blogspot.com/2013/08/configure-eshell-mode-after-upgrade.html
(require 'esh-mode)

;; Package Locations
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      ;; (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

; start emacs in server mode
(server-start)


;; Font-face setup. Check the availability of a some default fonts, in
;; order of preference. The first of these alternatives to be found is
;; set as the default font, together with base size and fg/bg
;; colors. If none of the preferred fonts is found, nothing happens
;; and Emacs carries on with the default setup. We do this here to
;; prevent some of the irritating flickering and resizing that
;; otherwise goes on during startup. You can reorder or replace the
;; options here with the names of your preferred choices.

;; by dgm, when trying to solve weird rendering by Pragmata Pro.
;; (load "pragmatapro-font-lock-symbols.el")
;; (load "pretty-pragmata.el")

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; on Why Pragmata Pro doesn't work, read here: https://github.com/hiavi/pragmatapro/issues/9
;; Set default font. First one found is selected.
 (cond
  ((eq window-system nil) nil)
 ((font-existsp "Pragmata Pro Mono")
  (set-face-attribute 'default nil :height 156 :font "Pragmata Pro Mono"))
 ;;  ((font-existsp "FiraCode")
 ;;   (set-face-attribute 'default nil :height 121 :font "FiraCode"))
 ;;  ((font-existsp "Monoid")
 ;;   (set-face-attribute 'default nil :height 121 :font "Monoid"))
 ;;  ((font-existsp "Inconsolata")
 ;;  (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
 ;; ((font-existsp "Input Mono Compressed")
 ;;  (set-face-attribute 'default nil :height 131 :font "Input Mono Compressed"))
 ;; ((font-existsp "Menlo")
 ;;  (set-face-attribute 'default nil :height 121 :font "Menlo"))
 ;;  ((font-existsp "Consolas")
 ;;  (set-face-attribute 'default nil :height 121 :font "Consolas"))
 ;; ((font-existsp "Monaco")
 ;;  (set-face-attribute 'default nil :height 121 :font "Monaco"))
 ;; ((font-existsp "Envy Code R")
 ;;   (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
 ;; ((font-existsp "Source Code Pro")
 ;;  (set-face-attribute 'default nil :height 121 :font "Source Code Pro"))
   )

;; Line-spacing tweak
;; Set this to a different number depending on taste and the fonr
;; selected. The value can be a integer or decimal number.
;; if integer: it means pixels, added below each line.
;; if float (e.g 0.02): a scaling factor relative to current window's default line height.
;; if nil: add no extra spacing.

(setq-default line-spacing 0.06) ;; tuned for Pragmata Pro


;; Load up Org Mode and Babel
;; load up the main file
;; org-mode windmove compatibility
(require 'org)
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

;; This tells Emacs to open all .org files in org-mode (http://sachachua.com/blog/2007/12/emacs-getting-things-done-with-org-basic/)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;; Higher garbage collection threshold
(setq gc-cons-threshold 20000000)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; coding system
;; from: lopez-ibanez.eu/dotemacs.html
(prefer-coding-system 'utf-8)
;; Enable UTF-8 by default


  ;;; other stuff for setting up unicode by: https://github.com/izahn/emacs-starter-kit
  ;;; which is a fork of kieran healy's starter kit
  ;;; he says that setting this coding system prevents emacs from choking  on melpa file listings
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Instead, lopez-ibañez says that Emacs < 23 sometimes require setting these directly
;; but now they cause more problems than they solve... I leave them on as Kieran and Istha use them but I am not sure...
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
