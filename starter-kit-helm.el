(use-package helm
  :ensure t
  :diminish helm-mode
  :init 
  (helm-mode 1)
  (require 'helm-config))

(use-package helm-ag
  :ensure t
  :after helm-mode)

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

;; =C-x C-f= you start a =helm-find-files= session. There you can do =C-s= to recursively grep a selected directory.  Every time you type a character, helm updates grep result immediately. You can use ack-grep to replace grep with this configuration:

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; Command: helm-semantic-or-imenu
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

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

(ambrevar/global-set-keys
 "C-x M-g" 'ambrevar/helm-grep-git-or-ag
 "C-x M-G" 'helm-do-grep-ag)

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

(setq  helm-display-header-line nil)
;; Helm window is too big? That's why you have helm-autoresize-mode:

(helm-autoresize-mode -1)

;; The resizing is too annoying and you only want the window to be less varied or even at a different fixed size rather than the default size? It can be done with:

(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

;; Now, you have a Helm window that always takes 30% of your frame height.

;; One of the thing that annoyed me with Ido is that it is always at the bottom in the minibuffer and raise the mode line. If you are like me, you may want to open Helm window in the current window where point is in, so you don't have to move eyes far away from the upper-half of the window to the minibuffer. This is problematic if you have large monitor (i.e. 24 inches or above).

;; By setting this:

;; (setq helm-split-window-in-side-p t)
(setq helm-split-window-inside-p t)

;; Now, Helm always opens a small window right inside and at the lower half of current window. No more random Helm window!
;; You may want to remove the header line for Helm command with only one source. For Helm command with multiple sources, the header line appears as a very thin line. This is fine, but if you want Helm to be a bit smart, that is, keep the full source header line when multiple sources and hidden when there's a single source, you can add the following function to helm-before-initialize-hook:
;; (Now, Helm appears as usual when there's multiple sources.)

(defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
(defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
(defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))

(when (< emacs-major-version 26)
  (when (require 'linum-relative nil t)
    (helm-linum-relative-mode 1)))

;; (when (require 'helm-descbinds nil t)
;;    (helm-descbinds-mode))

(when (require 'wgrep-helm nil t)
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key (kbd "C-x C-q")))

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

(require 'helm-bookmark)
(setq helm-mini-default-sources `(helm-source-buffers-list
                                  helm-source-recentf
                                  ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;;; Use the M-s prefix just like `occur'.
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

;; comment out by dgm
; (set-face-attribute 'helm-source-header nil :inherit 'header-line :height 'unspecified :background 'unspecified :foreground 'unspecified)
(set-face-background 'helm-selection "#4f4f4f")
(set-face-background 'helm-visible-mark "#2f2f2f")
(set-face-foreground 'helm-visible-mark nil)
(set-face-foreground 'helm-match "tomato")
(set-face-attribute 'helm-buffer-file nil :background 'unspecified :foreground "white" :weight 'normal)
(set-face-attribute 'helm-buffer-directory nil :background 'unspecified :foreground "#1e90ff" :weight 'bold)
(set-face-attribute 'helm-ff-directory nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-directory)
(set-face-attribute 'helm-ff-file nil :background 'unspecified :foreground 'unspecified :weight 'unspecified :inherit 'helm-buffer-file)
(set-face-foreground 'helm-grep-finish "green4")

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
(define-key helm-map (kbd "S-SPC") 'ambrevar/helm-toggle-visible-mark-backwards)

(global-set-key  (kbd "C-<f4>") 'helm-execute-kmacro)

(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB work in terminal

(provide 'starter-kit-helm)

(message "Starter Kit Helm File loaded.")
