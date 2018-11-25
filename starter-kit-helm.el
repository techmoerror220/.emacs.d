(use-package helm
  :ensure t
  :diminish helm-mode
  :init 
  (helm-mode 1)
  (require 'helm-config))

(use-package helm-swoop
  :ensure t
  :after helm-mode
  ;; :bind ("H-w" . helm-swoop)
  )

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

    ;; Make M-x be equal to M-x helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

    ;; Command: helm-show-kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

    ;; Command: helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

    ;; Command: helm-find-files
    ;; helm-find-files is file navigation on steroids:
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "<s-escape>") 'helm-recentf)

;; =C-x C-f= you start a =helm-find-files= session. There you can do =C-s= to recursively grep a selected directory.  Every time you type a character, helm updates grep result immediately. You can use ack-grep to replace grep with this configuration:

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; Command: helm-semantic-or-imenu
(semantic-mode 1)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)

;; Command: helm-locate
(setq helm-locate-fuzzy-match t)

;; From ambrevar: Fallback on 'find' if 'locate' is not available.
(unless (executable-find "locate")
  (setq helm-locate-recursive-dirs-command "find %s -type d -regex .*%s.*$"))

;; See https://github.com/emacs-helm/helm/issues/1962.
(defun ambrevar/helm-locate-meta (&optional update)
  "Like `helm-locate' but also use the databases found in /media and /run/media.
With prefix argument, UPDATE the databases with custom uptions thanks to the
'updatedb-local' script."
  (interactive "P")
  (let ((user-db (expand-file-name "~/.cache/locate.db"))
        (media-dbs (apply 'append
                          (mapcar
                           (lambda (root) (ignore-errors (file-expand-wildcards (concat root "/*/locate.db"))))
                           (list (concat "/run/media/" (user-login-name))
                                 (concat "/media/" (user-login-name))
                                 "/media")))))
    (when update
      (with-temp-buffer
        (if (= (shell-command "updatedb-local" (current-buffer)) 0)
            (message "%s" (buffer-string))
          (error "%s" (current-buffer)))))
    (helm-locate-with-db
     (mapconcat 'identity
                (cons user-db media-dbs)
                ":")
     nil (thing-at-point 'filename))))

;; Command: helm-occur
;; search for patterns in current buffer
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "s-;") 'helm-occur)

;; Command: helm-lisp-completion-at-point
;; To enable fuzzy matching, add this setting:
(setq helm-lisp-fuzzy-completion t)

;; Command: helm-all-mark-rings
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; Command: helm-register
(global-set-key (kbd "C-c h x") 'helm-register)

(setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f")

(defun ambrevar/helm-grep-git-or-ag (arg)
  "Run `helm-grep-do-git-grep' if possible; fallback to `helm-do-grep-ag' otherwise.
Requires `call-process-to-string' from `functions'."
  (interactive "P")
  (require 'vc)
  (require 'functions)
  (if (and (vc-find-root default-directory ".git")
           (or arg (split-string (ambrevar/call-process-to-string "git" "ls-files" "-z") "\0" t)))
      (helm-grep-do-git-grep arg)
    (helm-do-grep-ag nil)))

(defun ambrevar/helm-grep-git-all-or-ag ()
  "Run `helm-grep-do-git-grep' over all git files."
  (interactive)
  (helm-grep-do-git-grep t))

(global-set-key [remap query-replace-regexp] 'helm-regexp)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; helm-google-suggest
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; helm-eval-expression-with-eldoc
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

;; Command: helm-eshell-history
(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "C-c h C-c h")  'helm-eshell-history)))

;;; Eshell
(defun ambrevar/helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
  (define-key eshell-mode-map (kbd "M-s") nil) ; Useless when we have 'helm-eshell-history.
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
(add-hook 'eshell-mode-hook 'ambrevar/helm/eshell-set-keys)

;; Command: helm-mini-buffer-history
(define-key minibuffer-local-map (kbd "C-c h C-c h") 'helm-minibuffer-history)

(require 'helm-descbinds)
(helm-descbinds-mode)

    ;; Tuhdo says to put this but if I do emacs spits error mesage on start up.
    ;;(require 'setup-helm)
    ;;(require 'setup-helm-gtags)

    (require 'helm-gtags)

    ;; Enable helm-gtags-mode
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "C-c g"
     helm-gtags-suggested-key-mapping t)

;; (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation)
(use-package helm-bibtex
  :ensure t)
(global-set-key (kbd "<s-backspace>") 'helm-bibtex)

(setq bibtex-completion-bibliography "/media/dgm/blue/documents/bibs/socbib.bib")

(setq bibtex-completion-library-path '("/media/dgm/blue/documents/elibrary/org/references/pdfs"))

(setq bibtex-completion-notes-path "/media/dgm/blue/documents/elibrary/org/references")

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")

(setq bibtex-completion-pdf-open-function 'org-open-file)

 (setq helm-bibtex-bibliography "/media/dgm/blue/documents/bibs/socbib.bib" 
       helm-bibtex-library-path "/media/dgm/blue/documents/elibrary/org/references/pdfs/"
       helm-bibtex-notes-path "/media/dgm/blue/documents/elibrary/org/references/readings.org")

(when (< emacs-major-version 26)
  (when (require 'linum-relative nil t)
    (helm-linum-relative-mode 1)))

;; (when (require 'helm-descbinds nil t)
;;    (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-c h w")))

(when (require 'helm-ls-git nil t)
  ;; `helm-source-ls-git' must be defined manually.
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34.
  (setq helm-source-ls-git
        (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
             (helm-make-source "Git files" 'helm-ls-git-source
               :fuzzy-match helm-ls-git-fuzzy-match))))

(setq
 helm-follow-mode-persistent t
 helm-reuse-last-window-split-state t
 helm-findutils-search-full-path t
 helm-show-completion-display-function nil
 helm-completion-mode-string ""
 helm-dwim-target 'completion
 helm-echo-input-in-header-line t
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
(global-set-key [remap apropos-command] 'helm-apropos)

;;; Add bindings to `helm-apropos`. TODO: Does not work most of the times.
;;; https://github.com/emacs-helm/helm/issues/1140
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

(define-key prog-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)
;;; The text-mode-map binding targets structured text modes like Markdown.
(define-key text-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)
(with-eval-after-load 'org
  (require 'helm-org-contacts nil t)
  (define-key org-mode-map (kbd "M-s f") 'helm-org-in-buffer-headings))
(with-eval-after-load 'woman
  (define-key woman-mode-map (kbd "M-s f") 'helm-imenu))
(with-eval-after-load 'man
  (define-key Man-mode-map (kbd "M-s f") 'helm-imenu))

(setq helm-source-names-using-follow '("Occur" "Git-Grep" "AG" "mark-ring" "Org Headings" "Imenu"))

;;; From https://www.reddit.com/r/emacs/comments/5q922h/removing_dot_files_in_helmfindfiles_menu/.
(defun ambrevar/helm-skip-dots (old-func &rest args)
  "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
        (helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
        (helm-previous-line 1))))

(advice-add #'helm-preselect :around #'ambrevar/helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate :around #'ambrevar/helm-skip-dots)

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'kmacro-ring)
  (add-to-list 'desktop-globals-to-save 'last-kbd-macro)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter)
  (add-to-list 'desktop-globals-to-save 'kmacro-counter-format)
  (add-to-list 'desktop-globals-to-save 'helm-ff-history)
  (add-to-list 'desktop-globals-to-save 'comint-input-ring))

(helm-top-poll-mode)
;;; Column indices might need some customizing. See `helm-top-command' and
;;; https://github.com/emacs-helm/helm/issues/1586 and
;;; https://github.com/emacs-helm/helm/issues/1909.

;;; Convenience.
(defun ambrevar/helm-toggle-visible-mark-backwards (arg)
  (interactive "p")
  (helm-toggle-visible-mark (- arg)))
;; (define-key helm-map (kbd "S-SPC") 'ambrevar/helm-toggle-visible-mark-backwards)

;; (global-set-key  (kbd "C-<f4>") 'helm-execute-kmacro)

(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action but this gives rise to problems. See https://github.com/jkitchin/org-ref/issues/527
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z... direct from Tuhdo!

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
                                         "/media/dgm/blue/documents/templates"))
  (projectile-add-known-project "~/.emacs.d/")
  (projectile-add-known-project  "~/texmf/")
  (projectile-add-known-project "~/Dropbox/gtd/")
  (projectile-add-known-project "/media/dgm/blue/documents/proyectos/mtj/")
  (projectile-add-known-project "/media/dgm/blue/documents/dropbox/")
  (projectile-add-known-project "/media/dgm/blue/documents/UNED/")
  (projectile-add-known-project "/media/dgm/blue/documents/data/eurostat")
  (projectile-add-known-project "/media/dgm/blue/documents/templates")
  
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

;; from) https://github.com/bbatsov/projectile#usage
(projectile-mode +1) ;; You now need to explicitly enable projectile and set a prefix. See      https://stackoverflow.com/questions/31421106/why-emacs-project-c-c-p-is-undefined, I guess it's already done with (projectile-global-mode t) in the use-package settings... but just in case.
(define-key projectile-mode-map (kbd "s--") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-enable-caching t) ;; update 22 nov 2018. In C-h v projectile-indexing-method they recommend to have it set to aliena to have this other variable set to true. If it does not work, revert to instructions in emacs's cheatsheet.
;; (setq projectile-enable-caching nil) ; see https://emacs.stackexchange.com/questions/2164/projectile-does-not-show-all-files-in-project
;; https://github.com/bbatsov/projectile/issues/1183
;; trying to fix slow behaviour of emacs
(setq projectile-mode-line
      '(:eval (format " Projectile[%s]"
                      (projectile-project-name))))

(define-key projectile-mode-map [?\s-d] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-D] 'projectile-find-dir-dwim)
(define-key projectile-mode-map [?\s-y] 'projectile-ag)

(use-package helm-projectile
  :ensure t
  :after helm-mode 
  :init     
  (setq projectile-completion-system 'helm)
  :commands helm-projectile
  ;;   :bind ("C-c p h" . helm-projectile)
  )

(helm-projectile-on)   ;;; creo que no hace falta tras decir =ensure t= in use-package.
(define-key projectile-mode-map [?\s-u] 'helm-projectile-find-file-in-known-projects) 
(setq projectile-switch-project-action 'helm-projectile)
(global-set-key (kbd "<s-return>") 'helm-projectile)
;; from https://projectile.readthedocs.io/en/latest/usage/
;; You can go one step further and set a list of folders which Projectile is automatically going to check for projects. But in reality, if I re-start the computer, Projectile does not recall this list.

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
  :bind ("M-p" . helm-projectile-ag)
  :init (setq helm-ag-base-command "/usr/bin/ag"
              helm-ag-insert-at-point t
              helm-ag-fuzzy-match     t
              helm-ag-command-option " --hidden" 
              helm-ag-use-agignore t))

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  ;;  (ambrevar/define-keys helm-map "s-\\" 'helm-toggle-resplit-and-swap-windows) ;; already used in starter-kit-exwm.org for ambrevar/toggle-window-split
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (when (fboundp 'ambrevar/helm-locate-meta)
    (exwm-input-set-key (kbd "s-F") #'ambrevar/helm-locate-meta))
  (exwm-input-set-key (kbd "s-a") #'helm-ag)
  (exwm-input-set-key (kbd "s-A") #'helm-do-grep-ag)
  (exwm-input-set-key (kbd "s-g") 'ambrevar/helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") 'ambrevar/helm-grep-git-all-or-ag))

(require 'helm-bookmark)

(when (require 'helm-exwm nil t)
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
                                    helm-source-buffer-not-found)))
;; Not sure how this works
;; (ambrevar/define-keys
;; helm-exwm-map
;;   "M-d" 'helm-buffer-run-kill-persistent
;;   "S-<return>" 'helm-buffer-switch-other-window)
  ;; Launcher
  (exwm-input-set-key (kbd "s-r") 'helm-run-external-command)
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x f") 'ido-recentf-open)

(add-to-list 'helm-completing-read-handlers-alist '(ido-recentf-open  . ido))

(provide 'starter-kit-helm)

(message "Starter Kit Helm File loaded.")
