;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dotfiles-dir)
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))

;; Common Lisp compatability
(require 'cl-lib)

;; Package Locations
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

 (server-start); start emacs in server mode


;; Font-face setup. Check the availability of a some default fonts, in
;; order of preference. The first of these alternatives to be found is
;; set as the default font, together with base size and fg/bg
;; colors. If none of the preferred fonts is found, nothing happens
;; and Emacs carries on with the default setup. We do this here to
;; prevent some of the irritating flickering and resizing that
;; otherwise goes on during startup. You can reorder or replace the
;; options here with the names of your preferred choices.

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font. First one found is selected.
(cond
 ((eq window-system nil) nil)
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 121 :font "PragmataPro"))
  ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 121 :font "Source Code Pro"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 121 :font "Menlo"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 121 :font "Consolas"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
   ((font-existsp "Envy Code R")
  (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
 )

;; Load up Org Mode and Babel
;; load up the main file
;; org-mode windmove compatibility
(setq org-replace-disputed-keys t)
(require 'org)
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

;;; init.el ends here
