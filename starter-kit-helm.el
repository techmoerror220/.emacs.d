(use-package helm
  :ensure t
  :diminish helm-mode
  :init 
  (helm-mode 1)
  (require 'helm-config)
  (require 'helm-grep))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action but this gives rise to problems. See https://github.com/jkitchin/org-ref/issues/527
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-google-suggest-use-curl-p t
      helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
      ;; helm-quick-update t ; do not display invisible candidates
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

      ;; you can customize helm-do-grep to execute ack-grep
      ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
      ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
      helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

      helm-echo-input-in-header-line t

      ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
      helm-ff-file-name-history-use-recentf t
      helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
      helm-buffer-skip-remote-checking t

      helm-mode-fuzzy-match t

      helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
      helm-org-headings-fontify t
      ;; helm-find-files-sort-directories t
      ;; ido-use-virtual-buffers t
      helm-semantic-fuzzy-match t
      ;; helm-M-x-fuzzy-match t
      ;; helm-imenu-fuzzy-match t
      ;; helm-lisp-fuzzy-completion t
      ;; helm-apropos-fuzzy-match t
      ;; helm-locate-fuzzy-match t
      helm-display-header-line nil)

;;    (global-set-key (kbd "C-x b") 'helm-buffers-list)
;;    (global-set-key (kbd "C-c r") 'helm-recentf)
;;    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;;    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; Command: helm-mini-buffer-history
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(setq helm-candidate-number-limit 100)

    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)

(use-package helm-swoop
  :ensure t
  :after helm-mode
  :bind (("C-c h o" . helm-swoop)
         ("C-c s" . helm-multi-swoop-all))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

    ;; Make M-x be equal to M-x helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

    ;; Command: helm-show-kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

    ;; Command: helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-recentf-fuzzy-match    t)

;; Command: helm-find-files
;; helm-find-files is file navigation on steroids:
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "<s-escape>") 'helm-recentf)

;; Command: helm-semantic-or-imenu
;; recall I have ==(semantic-mode 1)= in =starter-kit-completion.org=
(setq helm-imenu-fuzzy-match    t)

(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)

;; Command: helm-locate
(setq helm-locate-fuzzy-match t)

;; From ambrevar: Fallback on 'find' if 'locate' is not available.
(unless (executable-find "locate")
  (setq helm-locate-recursive-dirs-command "find %s -type d -regex .*%s.*$"))

;; See https://github.com/emacs-helm/helm/issues/1962.
;; DGM comments it out on 4 sept 2019 as I don't use it
;; (defun ambrevar/helm-locate-meta (&optional update)
;;   "Like `helm-locate' but also use the databases found in /media and /run/media.
;; With prefix argument, UPDATE the databases with custom uptions thanks to the
;; 'updatedb-local' script."
;;   (interactive "P")
;;   (let ((user-db (expand-file-name "~/.cache/locate.db"))
;;         (media-dbs (apply 'append
;;                           (mapcar
;;                            (lambda (root) (ignore-errors (file-expand-wildcards (concat root "/*/locate.db"))))
;;                            (list (concat "/run/media/" (user-login-name))
;;                                  (concat "/media/" (user-login-name))
;;                                  "/media")))))
;;     (when update
;;       (with-temp-buffer
;;         (if (= (shell-command "updatedb-local" (current-buffer)) 0)
;;             (message "%s" (buffer-string))
;;           (error "%s" (current-buffer)))))
;;     (helm-locate-with-db
;;      (mapconcat 'identity
;;                 (cons user-db media-dbs)
;;                 ":")
;;      nil (thing-at-point 'filename))))

;; Command: helm-occur
;; search for patterns in current buffer
;; (global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "s-o") 'helm-occur)

;; helm-resume: taken to dgm.org or else it didn't replace <exwm-reset>
;; (global-set-key (kbd "s-r") 'helm-resume)

;; Command: helm-lisp-completion-at-point
;; To enable fuzzy matching, add this setting:
(setq helm-lisp-fuzzy-completion t)

;; Command: helm-all-mark-rings
;; (global-set-key (kbd "<s-return>") 'helm-all-mark-rings)
;; <> compulsory for return but not for s
(global-set-key (kbd "s-<return>") 'helm-all-mark-rings)

;; Command: helm-register
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "s-x") 'helm-register)

;;(global-set-key [remap query-replace-regexp] 'helm-regexp)
(global-set-key (kbd "s-\'") 'helm-regexp)
;;(unless (boundp 'completion-in-region-function)
;;  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; (when (< emacs-major-version 26)
;;   (when (require 'linum-relative nil t)
;;     (helm-linum-relative-mode 1)))

;; (when (require 'helm-descbinds nil t)
;;    (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-c h w")))

;; From Ambrevar: wgrep-face is not so pretty. Commented out as not working
;; (set-face-attribute 'wgrep-face nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil)

;; (when (require 'helm-ls-git nil t)
;;   ;; `helm-source-ls-git' must be defined manually.
;;   ;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
;;   (setq helm-source-ls-git
;;         (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
;;              (helm-make-source "Git files" 'helm-ls-git-source
;;                :fuzzy-match helm-ls-git-fuzzy-match))))

(setq
 helm-follow-mode-persistent t
 helm-reuse-last-window-split-state t
 helm-findutils-search-full-path t
 helm-show-completion-display-function nil
 helm-completion-mode-string ""
 helm-dwim-target 'completion
 ;; helm-echo-input-in-header-line t
 helm-use-frame-when-more-than-two-windows nil
 ;; helm-apropos-fuzzy-match t
 ;; helm-buffers-fuzzy-matching t
 ;; helm-eshell-fuzzy-match t
 ;; helm-imenu-fuzzy-match t
 ;; helm-M-x-fuzzy-match t
 ;; helm-recentf-fuzzy-match t
 ;; Use woman instead of man.
 helm-man-or-woman-function nil
 ;; https://github.com/emacs-helm/helm/issues/1910
 helm-buffers-end-truncated-string "…"
 helm-buffer-max-length 22
 helm-window-show-buffers-function 'helm-window-mosaic-fn
 helm-window-prefer-horizontal-split t)

;; Command: helm-apropos
;; To enable fuzzy matching, add this setting:
(setq helm-apropos-fuzzy-match t)
;; (global-set-key [remap apropos-command] 'helm-apropos) ;; dgm comments out on sept 2019

;;; Add bindings to `helm-apropos`. TODO: Does not work most of the times.
;;; https://github.com/emacs-helm/helm/issues/1140
;;; Commented out by DGM on 4 sept 2019
(defun ambrevar/helm-def-source--emacs-commands (&optional default)
  (helm-build-in-buffer-source "Commands"
    :init `(lambda ()
             (helm-apropos-init 'commandp ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :candidate-transformer 'helm-M-x-transformer-1
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(global-set-key (kbd "s-a") 'helm-apropos)

(helm-top-poll-mode)
(global-set-key (kbd "s-t") 'helm-top)

;; helm-google-suggest
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)

;; helm-eval-expression-with-eldoc
;; (global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "s-\(") 'helm-eval-expression-with-eldoc)

;; Command: helm-eshell-history
(require 'helm-eshell)

;; (add-hook 'eshell-mode-hook
;;          '(lambda ()
;;             (define-key eshell-mode-map (kbd "C-c h C-c h")  'helm-eshell-history))) 
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Eshell
(defun ambrevar/helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  ;; dgm comments out on 4 sept 2019
  ;;(define-key eshell-mode-map (kbd "M-s") nil) ; Useless when we have 'helm-eshell-history.
  ;;(define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)) ;; this one doesn't work... I don't know what it'd do.
(add-hook 'eshell-mode-hook 'ambrevar/helm/eshell-set-keys)

(use-package helm-descbinds
	:ensure t)
(helm-descbinds-mode)

(define-key global-map [remap find-tag] 'helm-etags-select)

(use-package ggtags
  :ensure t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; this variable must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

;; (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation)
(use-package helm-bibtex
  :ensure t)
;; (global-set-key (kbd "<s-backspace>") 'helm-bibtex) ;; not needed. Already in =C-c ]=. <s-backspace> relocated to helm-swoop. Aunque ojo que en Olivetti mode =C-c ]= esta' bound to another thing.

(setq bibtex-completion-bibliography "/media/dgm/blue/documents/bibs/socbib.bib")

(setq bibtex-completion-library-path '("/media/dgm/blue/documents/elibrary/org/references/pdfs"))

(setq bibtex-completion-notes-path "/media/dgm/blue/documents/elibrary/org/references")

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")

(setq bibtex-completion-pdf-open-function 'org-open-file)

 (setq helm-bibtex-bibliography "/media/dgm/blue/documents/bibs/socbib.bib" 
       helm-bibtex-library-path "/media/dgm/blue/documents/elibrary/org/references/pdfs/"
       helm-bibtex-notes-path "/media/dgm/blue/documents/elibrary/org/references/readings.org")

(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

;; Projectile
(use-package projectile
  :ensure t
  :config 
  (projectile-global-mode t)
  (setq projectile-project-search-path '("~/.emacs.d/"
                                         "~/texmf/"
                                         "~/Dropbox/gtd/"
                                         "/media/dgm/blue/documents/proyectos/mtj/"
                                         "/media/dgm/blue/documents/dropbox/"
                                         "/media/dgm/blue/documents/UNED/"
                                         "/media/dgm/blue/documents/data/eurostat" 
                                         "/media/dgm/blue/documents/programming"
                                         "/media/dgm/blue/documents/My-Academic-Stuff"
                                         "/media/dgm/blue/documents/personal"
                                         "/home/dgm/Dropbox/gtd"
                                         "/media/dgm/blue/documents/bibs"
                                         "/media/dgm/blue/documents/templates"
                                         "/media/dgm/blue/documents/proyectos/alianza"
                                         "/media/dgm/blue/documents/cv"
                                         "/media/dgm/blue/documents/backups"
                                         "/media/dgm/blue/documents/UNED/teaching/mis-cursos"
                                         "/media/dgm/blue/documents/elibrary/women-labor-market"))

  (projectile-add-known-project "~/.emacs.d/")
  (projectile-add-known-project  "~/texmf/")
  (projectile-add-known-project "~/Dropbox/gtd/")
  (projectile-add-known-project "/media/dgm/blue/documents/proyectos/mtj/")
  (projectile-add-known-project "/media/dgm/blue/documents/dropbox/")
  (projectile-add-known-project "/media/dgm/blue/documents/UNED/")
  (projectile-add-known-project "/media/dgm/blue/documents/data/eurostat")
  (projectile-add-known-project "/media/dgm/blue/documents/programming")
  (projectile-add-known-project "/media/dgm/blue/documents/My-Academic-Stuff")
  (projectile-add-known-project "/media/dgm/blue/documents/personal")  
  (projectile-add-known-project "/home/dgm/Dropbox/gtd")  
  (projectile-add-known-project "/media/dgm/blue/documents/bibs")  
  (projectile-add-known-project "/media/dgm/blue/documents/templates")
  (projectile-add-known-project "/media/dgm/blue/documents/proyectos/alianza/")
  (projectile-add-known-project "/media/dgm/blue/documents/cv/")
  (projectile-add-known-project "/media/dgm/blue/documents/backups")
  (projectile-add-known-project "/media/dgm/blue/documents/UNED/teaching/mis-cursos/")
  (projectile-add-known-project "/media/dgm/blue/documents/elibrary/women-labor-market/")
  
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

;; from: https://github.com/bbatsov/projectile#usage
(projectile-mode +1) ;; You now need to explicitly enable projectile and set a prefix. See      https://stackoverflow.com/questions/31421106/why-emacs-project-c-c-p-is-undefined, I guess it's already done with (projectile-global-mode t) in the use-package settings... but just in case.
;;(define-key projectile-mode-map (kbd "s--") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-enable-caching nil) ;; update 22 nov 2018. In C-h v projectile-indexing-method they recommend to have it set to alien to have this other variable set to true. If it does not work, revert to instructions in emacs's cheatsheet.
;; (setq projectile-enable-caching nil) ; see https://emacs.stackexchange.com/questions/2164/projectile-does-not-show-all-files-in-project
;; https://github.com/bbatsov/projectile/issues/1183
;; trying to fix slow behaviour of emacs
(setq projectile-mode-line
      '(:eval (format " Projectile[%s]"
                      (projectile-project-name))))

(use-package helm-projectile
  :ensure t
  :after helm-mode 
  :init     
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  :commands helm-projectile
  ;;   :bind ("C-c p h" . helm-projectile)
  )

;; (define-key projectile-mode-map [?\s-u] 'helm-projectile-find-file-in-known-projects) 
(setq projectile-switch-project-action 'helm-projectile)
(global-set-key (kbd "s-h") 'helm-projectile)
;; from https://projectile.readthedocs.io/en/latest/usage/
;; You can go one step further and set a list of folders which Projectile is automatically going to check for projects. But in reality, if I re-start the computer, Projectile does not recall this list.

(setq projectile-other-file-alist '(("cpp" "h" "hpp" "ipp")
                                    ("ipp" "h" "hpp" "cpp")
                                    ("hpp" "h" "ipp" "cpp")
                                    ("cxx" "hxx" "ixx")
                                    ("ixx" "cxx" "hxx")
                                    ("hxx" "ixx" "cxx")
                                    ("c" "h")
                                    ("m" "h")
                                    ("mm" "h")
                                    ("h" "c" "cpp" "ipp" "hpp" "m" "mm")
                                    ("cc" "hh")
                                    ("hh" "cc")
                                    ("vert" "frag")
                                    ("frag" "vert")
                                    (nil "lock" "gpg")
                                    ("lock" "")
                                    ("gpg" "")))

(add-to-list 'projectile-other-file-alist '("org" "el")) ;; switch from org -> el 
(add-to-list 'projectile-other-file-alist '("el" "org")) ;; switch from el -> org 
(add-to-list 'projectile-other-file-alist '("Rnw" "R"))
(add-to-list 'projectile-other-file-alist '("R" "Rnw"))
(add-to-list 'projectile-other-file-alist '("Rnw" "tex"))
(add-to-list 'projectile-other-file-alist '("tex" "Rnw"))
(add-to-list 'projectile-other-file-alist '("org" "tex"))
(add-to-list 'projectile-other-file-alist '("tex" "org"))
(add-to-list 'projectile-other-file-alist '("tex" "log"))
(add-to-list 'projectile-other-file-alist '("log" "tex"))
(add-to-list 'projectile-other-file-alist '("org" "html"))
(add-to-list 'projectile-other-file-alist '("html" "org"))

(add-to-list 'projectile-globally-ignored-files "*.png")
(setq projectile-globally-ignored-file-suffixes '(".cache"))

;; (define-key projectile-mode-map [?\s-d] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-\|] 'helm-projectile-find-file-dwim)
(define-key projectile-mode-map [?\s-\`] 'helm-projectile-find-other-file)
(define-key projectile-mode-map [?\s-\+] 'helm-projectile-find-dir)
(define-key projectile-mode-map [?\s-j] 'helm-projectile-find-file)
(define-key projectile-mode-map [?\s-l] 'helm-projectile-switch-to-buffer)
;;(define-key projectile-mode-map [?\s-y] 'projectile-ag) ;; this is not working.

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "s-m") 'helm-man-woman)

 (global-set-key (kbd "s-g") 'helm-projectile-grep)
 ;; (global-set-key (kbd "s-f") 'helm-projectile-ag)

(setq helm-grep-default-command
      "grep --color=always -d skip %e -n%cH -e %p %f"
      helm-grep-default-recurse-command
      "grep --color=always -d recurse %e -n%cH -e %p %f")

;; Originally in starter-kit-bindings.org like this
;;  (require 'ag)
;;  (define-key global-map "\C-x\C-a" 'ag) 
;;  (define-key global-map "\C-x\C-r" 'ag-regexp)

;; new bindings by DGM to try and use 'helm-ag
;;  (define-key global-map "\C-x\C-a" 'helm-ag) 
;;  (define-key global-map "\C-x\C-r" 'helm-ag-regexp)

(use-package ag 
  :ensure t)

(use-package helm-ag
  :ensure t
  :after (helm-mode ag)
  :bind ("s-f" . helm-ag)
  :init (setq helm-ag-base-command "/usr/bin/ag"
              helm-ag-insert-at-point t
              helm-ag-fuzzy-match     t
              helm-ag-command-option " --hidden" 
              helm-ag-use-agignore t))

(setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'"))

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  ;;  (ambrevar/define-keys helm-map "s-\\" 'helm-toggle-resplit-and-swap-windows) ;; already used in starter-kit-exwm.org for ambrevar/toggle-window-split
  ;; (exwm-input-set-key (kbd "s-c") #'helm-resume)  ;; get the latest helm thing you did!, i.e., reopen the last helm search. Hey: if I enable this line, instead of helm-resume I get helm-occur. Why?
  ;; (exwm-input-set-key (kbd "s-b") #'helm-mini) ;; not needed as already in =C-x b=
  ;; (exwm-input-set-key (kbd "s-f") #'helm-find-files) ;; already in C-x C-f
  ;; (exwm-input-set-key (kbd "s-:") #'helm-for-files) ;; tuhdo doesn't use it, so it goes
  (exwm-input-set-key (kbd "s-D") #'helm-locate)
  (exwm-input-set-key (kbd "s-d") #'helm-find))
  ;;(when (fboundp 'ambrevar/helm-locate-meta)
  ;;  (exwm-input-set-key (kbd "!!here") #'ambrevar/helm-locate-meta))
  ;; (exwm-input-set-key (kbd "s-F") #'helm-ag)
  ;; (exwm-input-set-key (kbd "s-f") #'helm-do-grep-ag))
  ;;(exwm-input-set-key (kbd "s-g") 'ambrevar/helm-grep-git-or-ag)
  ;;(exwm-input-set-key (kbd "s-G") 'ambrevar/helm-grep-git-all-or-ag))

(require 'helm-bookmark)

(use-package helm-exwm)
;; (when (require 'helm-exwm nil t)
(add-to-list 'helm-source-names-using-follow "EXWM buffers")
(setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
(setq helm-exwm-source (helm-exwm-build-source))
(setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                  helm-exwm-source
                                  helm-source-buffers-list
                                  helm-source-recentf
                                  ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;; Not sure how this works
;;(ambrevar/define-keys helm-exwm-map
;;   "s-!" 'helm-buffer-run-kill-persistent
;;   "s-#" 'helm-buffer-switch-other-window)
;; The above does not work and I don't know what is meant to do

;; next two lines work in the context of a helm menu like the one triggered with =C-x b=
(global-set-key (kbd "C-c h w") 'helm-buffer-switch-other-window)
(global-set-key (kbd "C-c h k") 'helm-buffer-run-kill-persistent)

;; Launcher
(exwm-input-set-key (kbd "s-e") 'helm-run-external-command)

;; Web browser. Turned off by DGM on 22 august 2019
;; (exwm-input-set-key (kbd "M-s-j") #'helm-exwm-switch-browser)               ;; I don't use these two and I don't see the user case.
;; (exwm-input-set-key (kbd "s-j")   #'helm-exwm-switch-browser-other-window)  ;; not using it

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x f") 'ido-recentf-open)

(add-to-list 'helm-completing-read-handlers-alist '(ido-recentf-open  . ido))

(use-package password-store)
(use-package helm-pass)

(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-buffer-map (kbd "C-g") 'helm-keyboard-quit)
            (define-key helm-map (kbd "C-g") 'helm-keyboard-quit)))

(use-package helm-org-rifle
  :ensure t
  :bind ("s-u" . helm-org-rifle))

(defun ap/helm-info-emacs-elisp-cl ()
  "Helm for Emacs, Elisp, and CL-library info pages."
  (interactive)
  (helm :sources '(helm-source-info-emacs
                   helm-source-info-elisp
                   helm-source-info-cl)))

(global-set-key (kbd "s-\)") 'ap/helm-info-emacs-elisp-cl)

(global-set-key (kbd "s-p") 'helm-org-in-buffer-headings)

(global-set-key (kbd "s-c") 'helm-colors)

(provide 'starter-kit-helm)

(message "Starter Kit Helm File loaded.")
