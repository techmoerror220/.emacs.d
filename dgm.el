(setq insert-directory-program (executable-find "ls"))

  (setq sentence-end-double-space nil)

    (defun fullscreen (&optional f)
      (interactive)
      (set-frame-parameter f 'fullscreen
                           (if (frame-parameter f 'fullscreen) nil 'fullboth)))
    (global-set-key (kbd "C-c f") 'fullscreen)        
    (add-hook 'after-make-frame-functions 'fullscreen)

;;  (load "dired-x")
     (add-hook 'dired-load-hook
               (lambda ()
                 (load "dired-x")
                 ;; Set dired-x global variables here.  For example:
                 ;; (setq dired-guess-shell-gnutar "gtar")
                 ;; (setq dired-x-hands-off-my-keys nil)
                 ))
     (add-hook 'dired-mode-hook
               (lambda ()
                 ;; Set dired-x buffer-local variables here.  For example:
                 ;; (dired-omit-mode 1)
                 ))

(if (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alDh --group-directories-first --time-style \"+%d-%m-%Y %H:%M:%S\"")) 

(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))

(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)

(add-hook 'dired-mode-hook
          (lambda()
             (setq truncate-lines 1)))

(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode")
           (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)))
         myFileList))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath)
           (shell-command (format "open \"%s\"" fPath)))
         myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" fPath))) myFileList))))))
;; use zip/unzip to compress/uncompress zip archives
(with-eval-after-load "dired-aux"
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" "" "unzip"))
  ;; open files from dired with "E"
  (define-key dired-mode-map (kbd "E") 'xah-open-in-external-app))

  (eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(use-package hl-line)
;; (global-hl-line-mode t)
;;(set-face-background 'hl-line "#bebebe")

(use-package stripe-buffer
  :config (progn
            (add-hook 'dired-mode-hook #'turn-on-stripe-buffer-mode)))

(add-hook 'org-mode-hook 'org-table-stripes-enable)
;; (add-hook 'dired-mode-hook 'stripe-listify-buffer)

(use-package avy
  :ensure t
  :bind
    ("s-z" . avy-goto-char))  ;; goes literally to any char

(define-key global-map (kbd "C-o") 'avy-goto-word-1) ;; goes to word that starts with a given char

 (add-hook 'prog-mode-hook 'linum-mode)

;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(use-package speed-type)

(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))

;; (use-package iedit)

;; activate this function by Mickey Petersen if you wish to use iedit only in current function and not all across the buffer.
;; (defun iedit-dwim (arg)
;;   "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;;   (interactive "P")
;;   (if arg
;;       (iedit-mode)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         ;; this function determines the scope of `iedit-start'.
;;         (if iedit-mode
;;             (iedit-done)
;;           ;; `current-word' can of course be replaced by other
;;           ;; functions.
;;           (narrow-to-defun)
;;           (iedit-start (current-word) (point-min) (point-max)))))))

;; (global-set-key (kbd "C-;") 'iedit-dwim)

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

(setq global-mark-ring-max 5000     ; increase mark ring to contain 5000 entries
      mark-ring-max 5000            ; increase kill ring to contain 5000 entries
      mode-require-final-newline t) ; add a newline to end of file

(setq
 kill-ring-max 5000 ; increase kill-ring capacity
;; kill-whole-line t  ; if NIL, killwhole line and move the next line up / commented out by dgm as it might interfere with kill-whole-line-or-region mode
)

(setq-default tab-width 4)

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

  ;; (require 'workgroups2)

  ;; Change workgroups session file
  ;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  ;; (wg-find-session-file "~/.emacs.d/.emacs_workgroups") ;; for emacs to load this file on startup... but it doesn't work... don't know why...

  ;; Set your own keyboard shortcuts to reload/save/switch WGs:
  ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
  ;; (global-set-key (kbd "<pause>")     'wg-reload-session)
  ;; (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
  ;; (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
  ;; (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

  ;; What to do on Emacs exit / workgroups-mode exit?
  ;; (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
  ;; (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

  ;; (workgroups-mode 1)   ; put this one at the bottom of .emacs

  (add-hook 'diff-mode-hook (lambda ()
                              (setq-local whitespace-style
                                          '(face
                                            tabs
                                            tab-mark
                                            spaces
                                            space-mark
                                            trailing
                                            indentation::space
                                            indentation::tab
                                            newline
                                            newline-mark))
                              (whitespace-mode 1)))

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(use-package volatile-highlights)
(volatile-highlights-mode t)

(use-package clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

 (defun my-pkg-init()
   (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
   (clean-aindent-mode t)
   (setq clean-aindent-is-simple-indent t)
   (define-key global-map (kbd "RET") 'newline-and-indent))
 (add-hook 'after-init-hook 'my-pkg-init)

(use-package dtrt-indent
  :init
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode 1)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

(setq-default fill-column 72)

;; always display ibuffer in another window
(setq ibuffer-use-other-window t)

(use-package duplicate-thing)
(global-set-key (kbd "M-D") 'duplicate-thing)

;; (require 'dired+)

;; copied  by dgm from: http://emacs-leuven.readthedocs.io/en/latest/?badge=latest

    ;; Don't hide details in Dired.
;;    (setq diredp-hide-details-initially-flag nil)

    ;; Don't display the next Dired buffer the same way as the last.
;;    (setq diredp-hide-details-propagate-flag nil)

    ;; Don't wrap "next" command around to buffer beginning.
    ;; (setq diredp-wrap-around-flag nil)

    ;; Dired `find-file' commands reuse directories.
    ;; (diredp-toggle-find-file-reuse-dir 1)

    ;; Up, reusing Dired buffers.
    ;; (define-key dired-mode-map (kbd "C-x C-j")
    ;;  #'diredp-up-directory-reuse-dir-buffer)

;; tips from Ista Zahn. Not sure if they require dired+
;; https://github.com/izahn/dotemacs

;;; Dired and Dired+ configuration
;; this is commented as it is reapplied somewhere else in this file
;; (add-hook 'dired-mode-hook
;;          (lambda()
;;            (diff-hl-dired-mode)
;;            (diff-hl-margin-mode)))

;;  (require 'flycheck-tip)
;;  (define-key global-map (kbd "\C-c \C-n") 'flycheck-tip-cycle)
;;  (setq flycheck-display-errors-function 'ignore)

(use-package highlight-numbers)
(use-package highlight-symbol)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)

(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)))

;; keybinds conflict so...
;;(global-set-key (kbd "M-n") 'highlight-symbol-next)
;;(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;; (use-package info+) ;;no longer available in MELPA?

;; A quick major mode help with discover-my-major
(global-unset-key (kbd "C-h h"))        ; original "\C-h h" displays "hello world" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: rainbow-mode              ;;
;;                                    ;;
;; GROUP: Help -> Rainbow             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook         #'rainbow-mode)
  (add-hook 'html-mode-hook         #'rainbow-mode)
  (add-hook 'css-mode-hook          #'rainbow-mode)
  (add-hook 'org-mode-hook          #'rainbow-mode)
  (add-hook 'latex-mode-hook        #'rainbow-mode)
  (add-hook 'R-mode-hook            #'rainbow-mode)
  (add-hook 'inferior-ess-mode-hook #'rainbow-mode)
  (add-hook 'python-mode-hook       #'rainbow-mode)
 )

;;R-mode-hook runs when you open a new source buffer, so anything you put in that will only affect your source buffers.inferior-ess-mode-hook runs when you start an R console, so anything in there should only apply to the console buffer and not the source.

(use-package kurecolor
   :ensure t)

;; (require 'spaceline-config)
;; (spaceline-emacs-theme)
;; (spaceline-helm-mode)

;; (require 'smart-mode-line)
;;       (require 'smart-mode-line-powerline-theme)
;;       (sml/apply-theme 'powerline)


;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-default-separator-dir '(right . left))
;; (setq sml/theme 'powerline)
;; (setq sml/mode-width 0)
;; (setq sml/name-width 20)
;; (rich-minority-mode 1)
;; (setf rm-blacklist "")
;; (sml/setup)


;; (if (require 'smart-mode-line nil 'noerror)
;;     (progn
;;       (setq sml/name-width 20)
;;       (setq sml/mode-width 'full)
;;       (setq sml/shorten-directory t)
;;       (setq sml/shorten-modes t)
;;
;;       (rich-minority-mode 1)
;; ;;      (setq rm-blacklist '(" GitGutter" " MRev" " company" " mate" " Projectile"))
;;
;;       (if after-init-time
;;         (sml/setup)
;;         (add-hook 'after-init-hook 'sml/setup))))
;;
      ;; Alternatives:
      ;; (sml/apply-theme 'powerline)
      ;; (sml/apply-theme 'dark)
      ;; (sml/apply-theme 'light)
      ;; (sml/apply-theme 'respectful)
      ;; (sml/apply-theme 'automatic)

;;      (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;;      (add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":CODE:"))
;;      (add-to-list 'sml/replacer-regexp-list '("^:CODE:investor-bridge" ":IB:"))
;;      (add-to-list 'sml/replacer-regexp-list '("^~/.*/lib/ruby/gems" ":GEMS" ))))

(use-package which-key
:ensure t
:config
(which-key-mode))

;; require the main file containing common functions
(use-package eval-in-repl
  :ensure t
  :config 
  (setq comint-process-echoes t)
  ;; truncate lines in comint buffers
  (add-hook 'comint-mode-hook
            (lambda()
              (setq truncate-lines 1)))
  ;; Scroll down for input and output
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t))

(setq comint-get-old-input (lambda () (end-of-buffer) (comint-get-old-input-default)))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  ;;;  ESS (Emacs Speaks Statistics)

;; ;; Start R in the working directory by default
;; (setq ess-ask-for-ess-directory nil)
;;
;; ;; Make sure ESS is loaded before we configure it
;; (autoload 'julia "ess-julia" "Start a Julia REPL." t)
;; (with-eval-after-load "ess-site"
;;   ;; disable ehoing input
;;   (setq ess-eval-visibly nil)
;;   ;; Start R in the working directory by default
;;   (setq ess-ask-for-ess-directory nil)
;;   ;; Use tab completion
;;   (setq ess-tab-complete-in-script t)
;;   ;; extra ESS stuff inspired by https://github.com/gaborcsardi/dot-emacs/blob/master/.emacs
;;   (ess-toggle-underscore nil)
;;   (defun my-ess-execute-screen-options (foo)
;;     "cycle through windows whose major mode is inferior-ess-mode and fix width"
;;     (interactive)
;;     (setq my-windows-list (window-list))
;;     (while my-windows-list
;;       (when (with-selected-window (car my-windows-list) (string= "inferior-ess-mode" ;; major-mode))
;;         (with-selected-window (car my-windows-list) (ess-execute-screen-options t)))
;;       (setq my-windows-list (cdr my-windows-list))))
;;   (add-to-list 'window-size-change-functions 'my-ess-execute-screen-options)
;;   (define-key ess-mode-map (kbd "<C-return>") ;; 'ess-eval-region-or-function-or-paragraph-and-step)
;;   ;; truncate long lines in R source files
;;   (add-hook 'ess-mode-hook
;;             (lambda()
;;               ;; don't wrap long lines
;;               (toggle-truncate-lines t)
;;               (outline-minor-mode t))))

(with-eval-after-load "elisp-mode"
  (require 'company-elisp)
  ;; ielm
  (require 'eval-in-repl-ielm)
  ;; For .el files
  (define-key emacs-lisp-mode-map "C-c C-c" 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; For *scratch*
  (define-key lisp-interaction-mode-map "C-c C-c" 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; For M-x info
  (define-key Info-mode-map "C-c C-c" 'eir-eval-in-ielm)
  ;; Set up completions
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              ;; make sure completion calls company-elisp first
              (require 'company-elisp)
              (setq-local company-backends
                          (delete-dups (cons 'company-elisp (cons 'company-files company-backends)))))))

;;(setq command-log-mode-auto-show t)
;;(global-set-key (kbd "\C-x c l") 'global-command-log-mode)

;; (require 'auto-complete-config)
;; (ac-config-default)

;; if you really like the menu
;;(setq ac-show-menu-immediately-on-auto-complete t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized functions                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "\C-a") 'prelude-move-beginning-of-line)

(use-package recentf
  :ensure t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq recentf-auto-cleanup 'never)

(use-package recentf-ext
  :ensure t)

(use-package ztree)
;;(use-package ztree-diff)
;;(use-package ztree-dir)

(use-package rebox2)
(global-set-key [(meta q)] 'rebox-dwin-fill)
(global-set-key [(shift meta q)] 'rebox-dwin-no-fill)

(use-package helpful)

(global-set-key (kbd "\C-h f") #'helpful-callable)
(global-set-key (kbd "\C-h v") #'helpful-variable)
(global-set-key (kbd "\C-h k") #'helpful-key)

(defun rtags-peek-definition ()
  "Peek at definition at point using rtags."
  (interactive)
  (let ((func (lambda ()
                (rtags-find-symbol-at-point)
                (rtags-location-stack-forward))))
    (rtags-start-process-unless-running)
    (make-peek-frame func)))

(defun make-peek-frame (find-definition-function &rest args)
  "Make a new frame for peeking definition"
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let (summary
          doc-frame
          x y
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
          ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (abs-pixel-pos (save-excursion
                           (beginning-of-thing 'symbol)
                           (window-absolute-pixel-position))))
      (setq x (car abs-pixel-pos))
      ;; (setq y (cdr abs-pixel-pos))
      (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq doc-frame (make-frame '((minibuffer . nil)
                                    (name . "*RTags Peek*")
                                    (width . 80)
                                    (visibility . nil)
                                    (height . 15))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (set-frame-position doc-frame x y)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (with-selected-frame doc-frame
        (apply find-definition-function args)
        (read-only-mode)
        (when semantic-stickyfunc-mode (semantic-stickyfunc-mode -1))
        (recenter-top-bottom 0))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (make-frame-visible doc-frame))))

(global-set-key (kbd "M-s-p") 'rtags-peek-definition)

(use-package ghub
  :ensure t)

(use-package magit
  :ensure t
  :after (ghub)
  :defer t
  :bind (("C-x g" . magit-status) 
         ("C-x M-l" . magit-log-buffer-file)
         ("C-x M-b" . magit-blame)))

(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

 (use-package atomic-chrome
   :ensure t
   :custom
   (atomic-chrome-url-major-mode-alist
    '(("reddit\\.com" . markdown-mode)
      ;;("github\\.com" . gfm-mode)
      ("uned\\.es" . text-mode))
    "Major modes for URLs.")
   :config
   (atomic-chrome-start-server))

;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English.

;;    (define-key global-map [?\s-u] 'undo)
;;    (define-key global-map [?\s-j] 'save-buffer)
;;    (define-key global-map [?\s-q] 'move-beginning-of-line)
;;    (define-key global-map [?\s-e] 'move-end-of-line)
;;    (define-key global-map [?\s-k] 'kill-buffer)

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
             (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (interactive)
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

;; (setq org-agenda-window-setup 'only-window)
;; (setq org-export-dispatch 'only-window)

(defun my-window-displaying-agenda-p (window)
    (equal (with-current-buffer (window-buffer window) major-mode)
        'org-agenda-mode)) 

(defun my-position-calendar-buffer (buffer alist)
  (let ((agenda-window (car (remove-if-not #'my-window-displaying-agenda-p (window-list)))))
    (when agenda-window
      (let ((desired-window (split-window agenda-window nil 'below)))
        (set-window-buffer desired-window  buffer)
        desired-window))))

(add-to-list 'display-buffer-alist (cons "\\*Calendar\\*" (cons #'my-position-calendar-buffer nil)))

(add-to-list 'display-buffer-alist
             `(,(rx string-start "*Calendar*" string-end)
               (display-buffer-below-selected)))

;;; Save M-: history.
(savehist-mode)

;;; Enforce horizontal splitting. 140 means that the window is large enough to
;;; hold 2 other windows of 70 columns.
(setq split-height-threshold nil
      split-width-threshold 140)

;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Initial scratch buffer message.
;; commented out as it was too distracting
;;(require 'functions) ; For `ambrevar/fortune-scratch-message'.
;;(let ((fortune (ambrevar/fortune-scratch-message)))
;;  (when fortune
;;    (setq initial-scratch-message fortune)))

;;; Save all visited URLs.
(setq url-history-track t
      url-history-file (expand-file-name "url/history" user-emacs-directory))

 (setq dired-dwim-target t)

(setq default-frame-alist '((font . "Pragmata Pro Mono-16")))
(add-to-list 'default-frame-alist '(line-spacing . 0.06))

(use-package fortune)
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")

(require 'uniquify)
(setq ;;(setq uniquify-buffer-name-style 'reverse)
      ;; (setq uniquify-buffer-name-style 'forward) ;; technomancy's default
      ;;(setq uniquify-separator "|")
      uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets)  ;; default in kieran healy's config
   ;;(setq uniquify-ignore-buffers-re "^*")

(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq apropos-do-all t
      mouse-yank-at-point t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)

;; winner-mode is activated in starter-kit-bindings.org
;; (winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; write merge buffer.  If the optional argument save-and-continue is non-nil,
;; then don't kill the merge buffer
(defun caolan/ediff-write-merge-buffer-and-maybe-kill (buf file
                                                           &optional
                                                           show-file save-and-continue)
  (if (not (eq (find-buffer-visiting file) buf))
      (let ((warn-message
             (format "Another buffer is visiting file %s. Too dangerous to save the merge buffer"
                     file)))
        (beep)
        (message "%s" warn-message)
        (with-output-to-temp-buffer ediff-msg-buffer
          (princ "\n\n")
          (princ warn-message)
          (princ "\n\n")
          )
        (sit-for 2))
    (ediff-with-current-buffer buf
      (if (or (not (file-exists-p file))
              (y-or-n-p (format "File %s exists, overwrite? " file)))
          (progn
            ;;(write-region nil nil file)
            (ediff-with-current-buffer buf
              (set-visited-file-name file)
              (save-buffer))
            (if show-file
                (progn
                  (message "Merge buffer saved in: %s" file)
                  (set-buffer-modified-p nil)))
            (if (and (not save-and-continue))
                (ediff-kill-buffer-carefully buf)))))
    ))

(defun caolan/ediff-maybe-save-and-delete-merge (&optional save-and-continue)
  "Default hook to run on quitting a merge job.
This can also be used to save merge buffer in the middle of an Ediff session.

If the optional SAVE-AND-CONTINUE argument is non-nil, save merge buffer and
continue.  Otherwise:
If `ediff-autostore-merges' is nil, this does nothing.
If it is t, it saves the merge buffer in the file `ediff-merge-store-file'
or asks the user, if the latter is nil.  It then asks the user whether to
delete the merge buffer.
If `ediff-autostore-merges' is neither nil nor t, the merge buffer is saved
only if this merge job is part of a group, i.e., was invoked from within
`ediff-merge-directories', `ediff-merge-directory-revisions', and such."
  (let ((merge-store-file ediff-merge-store-file)
        (ediff-autostore-merges ; fake ediff-autostore-merges, if necessary
         (if save-and-continue t ediff-autostore-merges)))
    (if ediff-autostore-merges
        (cond ((stringp merge-store-file)
               ;; store, ask to delete
               (caolan/ediff-write-merge-buffer-and-maybe-kill
                ediff-buffer-C merge-store-file 'show-file save-and-continue))
              ((eq ediff-autostore-merges t)
               ;; ask for file name
               (setq merge-store-file
                     (read-file-name "Save the result of the merge in file: "))
               (caolan/ediff-write-merge-buffer-and-maybe-kill
                ediff-buffer-C merge-store-file nil save-and-continue))
              ((and (ediff-buffer-live-p ediff-meta-buffer)
                    (ediff-with-current-buffer ediff-meta-buffer
                                               (ediff-merge-metajob)))
               ;; The parent metajob passed nil as the autostore file.
               nil)))
    ))

(add-hook 'ediff-quit-merge-hook #'caolan/ediff-maybe-save-and-delete-merge)

(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(setenv "TEST_USE_ANSI" "1")

(require 'flyspell)
(setq flyspell-mode-on t)

(use-package flycheck)
;;  :ensure t)
;;  :init (global-flycheck-mode))

(use-package shell-pop
  :ensure t
  :bind ("<s-escape>" . shell-pop))

;;  (setq shell-pop-default-directory "/home/dgm")
  (setq shell-pop-set-internal-mode "shell")
  (setq shell-pop-set-internal-mode-shell "/bin/bash")
  (setq shell-pop-set-window-height 30) ;the number for the percentage of the selected window. if 100, shell-pop use the whole of selected window, not spliting.
  (setq shell-pop-set-window-position "bottom") ;shell-pop-up position. You can choose "top" or "bottom".

  ;; https://emacs.stackexchange.com/questions/14876/how-can-i-make-ansi-color-codes-inside-the-prompt-show-up-in-shell-mode
  (set-face-attribute 'comint-highlight-prompt nil
                      :inherit nil)

  ;;;;;;;;;;;;;;;;;;;;;; http://amitp.blogspot.com/2007/04/emacs-color-in-shell-buffers.html
  (setq ansi-color-names-vector ; better contrast colors
        ["black" "red4" "green4" "yellow4"
         "blue3" "magenta4" "cyan4" "white"])
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  ;; (setq shell-pop-window-size 30)
  ;;(setq shell-pop-full-span t)
  ;;(setq shell-pop-window-position "bottom")
  ;;:config
  ;;(setq shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell shell-pop-term-shell)))))
  ;;(setq shell-pop-term-shell "/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  ;;(shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*IPython3\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*IPython\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*stata\\*" display-buffer--same-window-action) display-buffer-alist)

(use-package dumb-jump
  :ensure t
  :init (lambda ()
          (dumb-jump-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package synosaurus
  :ensure t
  :config (progn
            (setq synosaurus-backend 'synosaurus-backend-wordnet)
            (setq synosaurus-choose-method 'default)))

(use-package wordnut
  :ensure t
  :bind
  ("s-$" . wordnut-search))

(use-package olivetti
  :ensure t
  :config (setq olivetti-body-width 90))

(defun daedreth/take-screenshot ()
  "Takes a fullscreen screenshot of the current workspace"
  (interactive)
  (when window-system
  (loop for i downfrom 3 to 1 do
        (progn
          (message (concat (number-to-string i) "..."))
          (sit-for 1)))
  (message "Cheese!")
  (sit-for 1)
  (start-process "screenshot" nil "import" "-window" "root" 
             (concat (getenv "HOME") "/" (subseq (number-to-string (float-time)) 0 10) ".png"))
  (message "Screenshot taken!")))
(global-set-key (kbd "s-[") 'daedreth/take-screenshot)

(defun daedreth/take-screenshot-region ()
  "Takes a screenshot of a region selected by the user."
  (interactive)
  (when window-system
  (call-process "import" nil nil nil ".newScreen.png")
  (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1"
                (concat (getenv "HOME") "/" (subseq (number-to-string (float-time)) 0 10) ".png"))
  (call-process "rm" nil nil nil ".newScreen.png")))
(global-set-key (kbd "s-]") 'daedreth/take-screenshot-region)

(use-package ivy
  :ensure t)

(setq ivy-display-style 'fancy)

(use-package ace-window
  :ensure t
  :config
  (ace-window-display-mode)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?c aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help)))
  :bind ("M-P" . ace-window))

;; (global-set-key (kbd "M-P") 'ace-window)

(use-package swiper
  :ensure t
  :bind ("s-i" . 'swiper))

;;advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))
(advice-add 'swiper :after #'bjm-swiper-recenter)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)

(use-package mark-multiple
  :ensure t
  :bind ("s-q" . 'mark-next-like-this))

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "s-k") 'daedreth/kill-inner-word)

(defun daedreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "s-w") 'daedreth/copy-whole-word)

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "s-@") 'daedreth/copy-whole-line)

;;(global-set-key (kbd "M-s-k") 'kill-whole-line)
(global-set-key (kbd "s-%") 'kill-whole-line)

(global-subword-mode 1)

(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package page-break-lines
  :ensure t)

(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(bind-key "C-x 2" 'vsplit-other-window)
(bind-key "C-x 3" 'hsplit-other-window)

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode 1))
;;  :bind (("s-n" . smartscan-symbol-go-forward)
;;         ("s-p" . smartscan-symbol-go-backward)))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq scroll-step 1)
; Autosave every 500 typed characters. Alternative: try turning off auto save interval altogether.
(setq auto-save-interval 500)
; Scroll just one line when hitting bottom of window // ;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(use-package scratch
  :ensure t
  :commands scratch)

(visible-mode 1)
(global-set-key (kbd "s-v") 'visible-mode)

(use-package pulseaudio-control
  :ensure t)

(with-eval-after-load 'pulseaudio-control
  ;; REVIEW: Upstream should set path dynamically.
  ;; https://github.com/flexibeast/pulseaudio-control/issues/7
  (setq pulseaudio-control-pactl-path (executable-find "pactl")
        pulseaudio-control-volume-step "2%"))

(pulseaudio-control-default-keybindings)

 (put 'upcase-region 'disabled nil)
 (put 'downcase-region 'disabled nil)
 (put 'narrow-to-region 'disabled nil)
 (put 'dired-find-alternate-file 'disabled nil)
 (put 'set-goal-column 'disabled nil)

;; connect to irc on invocation but don't autojoin any channels (require 'rcirc)
;;  (add-to-list 'rcirc-server-alist
;;                       '("irc.freenode.net")) ;; this code stopped working after my customizations following the mini emacs guide
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#emacs" "#python" "#sml" "#nasm" "#gcc"))))

  ;; minimize fringe
  (setq-default indicate-empty-lines nil)

  ;; Add keybindings for commenting regions of text
  (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

  ;; Base dir
  (cd "~/")

(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

;; (global-set-key (kbd "s-/") 'mark-whole-word) !!here TODO. REBIND.

(setq next-line-add-newlines t)

 (defadvice imenu (around unfold-it compile activate)
      (save-restriction
        (widen)
        ad-do-it))

 (defun my-imenu-rescan ()
   (interactive)
   (imenu--menubar-select imenu--rescan-item))

    (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "s-?") 'ido-goto-symbol)

(use-package etags)
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
        (unless (integerp x)
          (push (prin1-to-string x t) tag-names)))
      tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-`") 'ido-find-file-in-tag-files)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-.") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(use-package interleave
  :ensure t)

(use-package pdf-tools
  :config (pdf-tools-install)
  ;; open pdfs scaled to fit page
          (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
          (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
          (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(with-eval-after-load "pdf-view"
(defun pdf-view-scroll-up-or-next-page (&optional arg)
  "Scroll page up ARG lines if possible, else go to the next page.
When `pdf-view-continuous' is non-nil, scrolling upward at the
bottom edge of the page moves to the next page.  Otherwise, go to
next page only on typing SPC (ARG is nil)."
  (interactive "P")
  (if (or pdf-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
            (cur-page (pdf-view-current-page)))
        (when (or (= (window-vscroll) (image-scroll-up arg))
                  ;; Workaround rounding/off-by-one issues.
                  (memq pdf-view-display-size
                        '(fit-height fit-page)))
          (pdf-view-next-page-command)
          (when (/= cur-page (pdf-view-current-page))
            (image-bob)
            (image-bol 1))
          (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-up arg)))

(defun pdf-view-scroll-down-or-previous-page (&optional arg)
  "Scroll page down ARG lines if possible, else go to the previous page.
When `pdf-view-continuous' is non-nil, scrolling downward at the
top edge of the page moves to the previous page.  Otherwise, go
to previous page only on typing DEL (ARG is nil)."
  (interactive "P")
  (if (or pdf-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
            (cur-page (pdf-view-current-page)))
        (when (or (= (window-vscroll) (image-scroll-down arg))
                  ;; Workaround rounding/off-by-one issues.
                  (memq pdf-view-display-size
                        '(fit-height fit-page)))
          (pdf-view-previous-page-command)
          (when (/= cur-page (pdf-view-current-page))
            (image-eob)
            (image-bol 1))
          (set-window-hscroll (selected-window) hscroll)))
(image-scroll-down arg))))

(when (require 'pdf-tools nil t)
   ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000")) 
   ;; (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; Amber's original combination. Too agressive for me. 
   (setq pdf-view-midnight-colors '("black" . "#EDD1B9" )) ; peach is the answer.
   (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
   (pdf-tools-install t t t))

(use-package markdown-mode
  :ensure t)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whitespace character of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; DGM adds this as C-k is bound to both kill-line and kill-visual-line
(defadvice kill-visual-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-@") 'prelude-smart-open-line)
(global-set-key (kbd "s-@") 'open-line)

(require 'setup-cedet)

(setq  helm-display-header-line nil)

(helm-autoresize-mode -1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

;; (setq helm-split-window-in-side-p t)
(setq helm-split-window-inside-p t)

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
;;(when window-system
;;  (custom-set-faces
;;   '(helm-selection ((t (:background "#4682b4" :foreground "#F5f5f5"))))))
;; Change Helm's selection color
;; Orange3: "#Cd8500"
(set-face-attribute 'helm-selection nil 
                    :background nil
                    :foreground "Orange3") 
;; ;; Change Helm's color of Source!!
;; (set-face-attribute 'helm-source-header nil 
;;                    :background "DeepSkyBlue4"
;;                    :foreground "black")

(set-face-attribute 'helm-buffer-directory nil :background 'unspecified :foreground "#1e90ff" :weight 'bold)

(require 'bookmark+)

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  ;;:config
  ;;(add-to-list 'ledger-report-mode) ;; Gives error Compiler-macro error for add-to-list: (wrong-number-of-arguments (3 . 5) 2)
  :mode "\\.dat\\'")

;;(setq-default indicate-empty-lines t)
;;(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;;(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
;;(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
;;;;;;; (set-fringe-bitmap-face 'tilde 'font-lock-function-name-face) ;; for blue tilde

(use-package vi-tilde-fringe)
(global-vi-tilde-fringe-mode 1)

  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
;;  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'yas-minor-mode)
  (diminish 'rainbow-mode)
;;  (diminish 'undo-tree-mode)
  (diminish 'editorconfig-mode)
;;  (diminish 'smartparens-mode) ;; added in -text.org
  (diminish 'minimal-mode)
  (diminish 'org-mode)
  (diminish 'org-indent-mode)
  (diminish 'volatile-highlights-mode) 
  (diminish 'highlight-symbol-mode) 
  (diminish 'pandoc-mode) 
;;  (diminish 'projectile-mode) 
  (diminish 'browse-kill-ring-mode) 
  (diminish 'auto-fill-mode) 
  (diminish 'refill-mode) 
  (diminish 'helm-gtags-mode) 
  (diminish 'vi-tilde-fringe-mode)
  (diminish 'dired)

(defmacro hy--shell-with-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer."
  (-let [shell-process
         (gensym)]
    `(-let [,shell-process
            (hy-shell-get-process)]
       (with-current-buffer (process-buffer ,shell-process)
         ,@forms))))

(defmacro hy--shell-with-font-locked-shell-buffer (&rest forms)
  "Execute FORMS in the shell buffer with font-lock turned on."
  `(hy--shell-with-shell-buffer
    (save-current-buffer
      (unless (hy--shell-buffer?)
        (setq hy-shell-buffer (hy--shell-get-or-create-buffer)))
      (set-buffer hy-shell-buffer)

      (unless (font-lock-mode) (font-lock-mode 1))
      (unless (derived-mode-p 'hy-mode) (hy-mode))

      ,@forms)))

(defun hy--shell-faces-to-font-lock-faces (text &optional start-pos)
  "Set all 'face in TEXT to 'font-lock-face optionally starting at START-POS."
  (let ((pos 0)
        (start-pos (or start-pos 0)))
    (while (and (/= pos (length text))
                (setq next (next-single-property-change pos 'face text)))
      (let* ((plist (text-properties-at pos text))
             (plist (-if-let (face (plist-get plist 'face))
                        (progn (plist-put plist 'face nil)  ; Swap face
                               (plist-put plist 'font-lock-face face))
                      plist)))
        (set-text-properties (+ start-pos pos) (+ start-pos next) plist)
        (setq pos next)))))

(defun hy--shell-fontify-prompt-post-command-hook ()
  "Fontify just the current line in `hy-shell-buffer' for `post-command-hook'.

Constantly extracts current prompt text and executes and manages applying
`hy--shell-faces-to-font-lock-faces' to the text."
  (-when-let* (((_ . prompt-end) comint-last-prompt)
               (_ (and prompt-end
                       (> (point) prompt-end)  ; new command is being entered
                       (hy--shell-current-buffer-a-process?))))  ; process alive?
      (let* ((input (buffer-substring-no-properties prompt-end (point-max)))
             (deactivate-mark nil)
             (buffer-undo-list t)
             (font-lock-buffer-pos nil)
             (text (hy--shell-with-font-locked-shell-buffer
                    (delete-region (line-beginning-position) (point-max))
                    (setq font-lock-buffer-pos (point))
                    (insert input)
                    (font-lock-ensure)
                    (buffer-substring font-lock-buffer-pos (point-max)))))
        (hy--shell-faces-to-font-lock-faces text prompt-end))))

(defun hy--shell-font-lock-turn-on ()
  "Turn on fontification of current line for hy shell."
  (hy--shell-with-shell-buffer
   (hy--shell-kill-buffer)

   (setq-local hy-shell-buffer nil)

   (add-hook 'post-command-hook
             'hy--shell-fontify-prompt-post-command-hook nil 'local)
   (add-hook 'kill-buffer-hook
             'hy--shell-kill-buffer nil 'local)))

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(defun yank-file-name ()
  "Yanks current buffer file name and its path to the kill ring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Yanks buffer file name and its path '%s' to the kill ring." filename))))

(use-package edit-list :commands edit-list)

(use-package forecast
  :config
  (setq forecast-latitude  40.4
        forecast-longitude -3.7
        forecast-city "Madrid"
        forecast-country "Spain"
        forecast-api-key "b580b82a056cdb3f6089f08ece4b2eb6"
  )
)

(setq calendar-latitude 40.4)
(setq calendar-longitude -3.7)
(setq calendar-location-name "Madrid, Spain")

(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Madrid"
                                "Fuengirola"
                                "Malaga")))

(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))

;; function to open wttrin with first city on list
(defun bjm/wttrin ()
    "Open `wttrin' without prompting, using first city in `wttrin-default-cities'"
    (interactive)
    ;; save window arrangement to register 
    (window-configuration-to-register :pre-wttrin)
    (delete-other-windows)
    ;; save frame setup
    (save-frame-config)
    (set-frame-width (selected-frame) 130)
    (set-frame-height (selected-frame) 48)
    ;; call wttrin
    (wttrin-query (car wttrin-default-cities))
    )

(use-package typing 
  :init
  (autoload 'typing-of-emacs "typing" nil t)
  :config
  (progn
    (setq toe-starting-length 6)
    (setq toe-starting-time-per-word 2)
    (setq toe-max-length 20)))

(setq sentence-end-double-space nil)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun my/shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (mapconcat 'identity (shuffle-list list) "\n"))))

;; (bind-key "s-:" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

  (defun my/sort-sexps-in-region (beg end)
    "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
    (interactive "r")
    (let ((input (buffer-substring beg end))
          list last-point form result)
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (setq last-point (point-min))
          (setq form t)
          (while (and form (not (eobp)))
            (setq form (ignore-errors (read (current-buffer))))
            (when form
              (add-to-list
               'list
               (cons
                (prin1-to-string form)
                (buffer-substring last-point (point))))
              (setq last-point (point))))
          (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
          (delete-region (point-min) (point))
          (insert (mapconcat 'cdr list "\n"))))))

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(define-skeleton article-skeleton
  "Inserts a Latex article skeleton into current buffer.
This only makes sense for empty buffers."
  "\n"
  "# -*- coding: utf-8 -*-\n"
  "# -*- find-file-hook: org-babel-execute-buffer -*-\n"
  "#+TITLE:\n"
  "#+SUBTITLE:\n" 
  "#+AUTHOR:\n" 
  "#+DATE:\n" 
  "#+OPTIONS: toc:4\n" 
  "#+OPTIONS: H:2 num:nil toc:nil \\n:nil @:t ::t |:t ^:{} _:{} *:t TeX:t LaTeX:t\n"
  "#+PROPERTY: header-args :tangle yes\n"
  "#+LATEX_HEADER: \\usepackage{org-preamble-pdflatex}\n"
  "#+LATEX_HEADER: \\usepackage{graphicx}\n"
  "#+LATEX_HEADER: \\usepackage{longtable}\n"
  "#+LATEX_HEADER: \\usepackage{float}\n"
  "#+LANGUAGE: "str | "spanish""\n"
  "#+LATEX_HEADER: \\usepackage["str | "spanish""]{babel}\n"
  "#+LATEX_HEADER: \\usepackage{sectsty}\n"
  "#+LATEX_HEADER: \\sectionfont{\\normalfont\\scshape}\n"
  "#+LATEX_HEADER: \\subsectionfont{\\normalfont\\bfseries}\n"
  "#+LATEX_HEADER: \\subsubsectionfont{\\normalfont\\itshape}\n"
  "#+LATEX_HEADER: \\pdfoptionpdfminorversion=6\n"
  "#+LATEX_HEADER: \\usepackage{prelim2e}\n"
  "#+LATEX_HEADER: \\immediate\\write18{sh ./vc}\n"
  "#+LATEX_HEADER: \\input{vc}\n"
  "#+LATEX_HEADER: \\renewcommand*{\\PrelimText}{\\textnormal{\\small\\textcolor{black!40}{author: \\VCAuthor\\ -- date: \\VCDateISO\\ -- time: \\VCTime\\ -- commit: \\texttt{\\VCRevision}}}}\n\n"
   "" _ "\n\n"
  "\\printbibliography")

(define-skeleton beamer-skeleton
  "Inserts a Latex beamer skeleton into current buffer.
This only makes sense for empty buffers."
  "\n"
  "# -*- coding: utf-8 -*-\n"
  "# -*- find-file-hook: org-babel-execute-buffer -*-\n"
  "#+TITLE:Fake title\n"
  "#+BEAMER_HEADER: \\title[Short Title]{Long Title}\n"
  "#+BEAMER_HEADER: \\author[\\textcolor{lightgray}{DGM \\& Mora}]{Daniel Guinea-Martin\\inst{1} and Ricardo Mora\\inst{2}\n"
  "#+BEAMER_HEADER: \\institute[]{\\inst{1} Dept.Sociology I, UNED  \\and \\inst{2} Dept. Economics, UC3M}\n"
  "#+DATE:\n"
  "#+LaTeX_CLASS: beamer\n"
  "#+LaTeX_CLASS_OPTIONS: [xcolor={svgnames,x11names},compress]\n"
  "#+LATEX_HEADER: \\usepackage{sanchez-dgm}\n"
  "#+BEAMER_THEME:\n"
  "#+BEAMER_FONT_THEME: professionalfonts\n"
  "#+OPTIONS: H:2 num:t\n"
  "\n\n"
  "" _ "")

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

  (defun mrb/set-input-method()
    (interactive)
    (setq default-input-method "spanish-prefix")
    ;; Toggle only if it not active yet
    (if (not current-input-method)
        (toggle-input-method)))

 ;;(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "s-s") #'mrb/set-input-method)

(use-package undo-propose)

(defun reverse-paragraphs (beg end)
  "Reverse the order of paragraphs in a region.
From a program takes two point or marker arguments, BEG and END."
  (interactive "r")
  (when (> beg end)
    (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; the last paragraph might be missing a trailing newline
    (goto-char end)
    (setq end (point-marker))
    ;; the real work.
    (goto-char beg)
    (let (paragraphs fix-newline)
      (while (< beg end)
	;; skip to the beginning of the next paragraph instead of
	;; remaining on the position separating the two paragraphs
	(when (= 0 (forward-paragraph 1))
	  (goto-char (1+ (match-end 0))))
	(when (> (point) end)
	  (goto-char end))
	(setq paragraphs (cons (buffer-substring beg (point))
			       paragraphs))
	(delete-region beg (point)))
      ;; if all but the last paragraph end with two newlines, add a
      ;; newline to the last paragraph
      (when (and (null (delete 2 (mapcar (lambda (s)
					   (when (string-match "\n+$" s -2)
					     (length (match-string 0 s))))
					 (cdr paragraphs))))
		 (when (string-match "\n+$" (car paragraphs) -2)
		   (= 1 (length (match-string 0 (car paragraphs))))))
	(setq fix-newline t)
	(setcar paragraphs (concat (car paragraphs) "\n")))
      ;; insert paragraphs
      (dolist (par paragraphs)
	(insert par))
      (when fix-newline
	    (delete-char -1)))))

(use-package try
  :ensure t)

;; I know that string is in my Emacs somewhere!
(require 'cl)
(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

;; (global-set-key [f7] 'search-all-buffers)

(defun xah-insert-column-az ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode.
URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2019-03-07"
  (interactive)
  (let (
        ($startChar (string-to-char (read-string "Start char: " "a")))
        ($howmany (string-to-number (read-string "How many: " "26")))
        ($colpos (- (point) (line-beginning-position))))
    (dotimes ($i $howmany )
      (progn
        (insert-char (+ $i $startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char $colpos)))))

(use-package twittering-mode)

(global-set-key (kbd "s-;") 'ivy-switch-buffer)

(global-set-key (kbd "s-*") 'electric-buffer-list)
 ; This hook run after buffer formatted, so it is necessary to re-fontify it...
 (add-hook 'electric-buffer-menu-mode-hook
	   '(lambda ()
	      (font-lock-mode 1)
	      (font-lock-fontify-buffer)))

(setq ido-default-buffer-method 'selected-window)
(unless ido-mode
  (ido-mode 1))
(global-set-key (kbd "s-n") 'ido-switch-buffer)
(global-set-key (kbd "C-c b") 'ido-switch-buffer)

(global-set-key (kbd "s--") #'helm-resume)

(use-package cheatsheet
  :ensure t
  :config
  (cheatsheet-add :group 'Bindings
                  :key "describe-personal-keybinding"
                  :description "Personal bindings")
  (cheatsheet-add :group 'Bindings
                  :key "C-h m"
                  :description "Bindings for all modes active in buffer")
  (cheatsheet-add :group 'Bookmarks
                  :key "C-x r m"
                  :description "Create bookmark")
  (cheatsheet-add :group 'Bookmarks
                  :key "C-x r l"
                  :description "List bookmark")
  (cheatsheet-add :group 'Bookmarks
                  :key "C-u a"
                  :description "Annotate bookmark where point is")
  (cheatsheet-add :group 'Bookmarks
                  :key "a"
                  :description "Read note on bookmark where point is")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "s-<tab>"
                  :description "ambrevar/switch-to-last-buffer")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "C-x b"
                  :description "helm-mini. Cycles EXWM buffers")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "s-n"
                  :description "ido-switch-buffer: C-c b when in an external app")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "s-l"
                  :description "helm-projectile-switch-to-buffer. Includes current P's and EXWM buffers")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "C-c y"
                  :description "bury-buffer")
  (cheatsheet-add :group 'Buffer-Cycling
                  :key "s-;"
                  :description "ivy-switch-buffer. Like iBuffer: list of buffers for editing")
  (cheatsheet-add :group 'Buffer-List
                  :key "s-*"
                  :description "electric-buffer-list")
  (cheatsheet-add :group 'Buffer-List
                  :key "C-x C-b"
                  :description "iBuffer. List of buffers for editing")
  (cheatsheet-add :group 'Buffer-Closing
                  :key "C-M-s-k"
                  :description "Close-all-buffers. s is the super key")
  (cheatsheet-add :group 'Browsing
                  :key "&"
                  :description "Open eww url in Chromium")
  (cheatsheet-add :group 'Browsing
                  :key "C-c h g"
                  :description "helm-google-suggest gives Google results in Helm buffer. Type slow")
  (cheatsheet-add :group 'Browsing
                  :key "s-b"
                  :description "helm-surfraw")
  (cheatsheet-add :group 'Browsing
                  :key "C-\{"
                  :description "daedreth/launch-browser")
  (cheatsheet-add :group 'Browsing
                  :key "here"
                  :description "Web browsing: Maps and Wolfram included")
  (cheatsheet-add :group 'Browsing
                  :key "M-x sos"
                  :description "StackOverflow only")
  (cheatsheet-add :group 'Browsing
                  :key "M-x my-helm-stackoverflow-lookup"
                  :description "Alternative for StackOverflow only")
  (cheatsheet-add :group 'Common
                  :key "C-x C-c"
                  :description "save-buffers-kill-emacs")
  (cheatsheet-add :group 'Debugging
                  :key "M-x gdb"
                  :description "IDE-like interface, with specialized buffers for controlling breakpoints, stack frames, and other aspects of the debugger state.")
  (cheatsheet-add :group 'Dired
                  :key "C-x d"
                  :description "helm-mode-dired. TAB to select; RET to jump to selection")
  (cheatsheet-add :group 'Dired
                  :key "/"
                  :description "Dired narrow to match filter")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "C-d"
                  :description "Open dired when in P session")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "M-C"
                  :description "Copy marked files in P session to another opened Dired dir")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "M-R"
                  :description "Move files in P session to another opened Dired dir")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "C-c f"
                  :description "Create Virtual Dired buffer with files marked in helm-projectile-find-files: s-u")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "C-c a"
                  :description "Add files marked to Virtual Dired buffer in helm-projectile-find-files: s-u")
  (cheatsheet-add :group 'Dired-Projectile
                  :key "C-c d"
                  :description "Remove entries from Virtual Dired buffer from helm-projectile-find-files session: s-u")
  (cheatsheet-add :group 'Editing
                  :key "s-@"
                  :description "open-line and prelude-smart-open-line. Insert an empty line after the current line.")
  (cheatsheet-add :group 'Editing
                  :key "C-x C-q"
                  :description "read-only-mode")
  (cheatsheet-add :group 'Editing
                  :key "s-v"
                  :description "See raw contents that usually are invisible-like hyperlinks")
  (cheatsheet-add :group 'Editing
                  :key "M-SPC"
                  :description "just-one-space: Delete all spaces and tabs around point, leaving one space-or N spaces")
  (cheatsheet-add :group 'Editing
                  :key "M-D"
                  :description "Duplicate line or region")
  (cheatsheet-add :group 'Editing
                  :key "comment-box"
                  :description "Puts region inside a box")
  (cheatsheet-add :group 'Editing
                  :key "C-c ;"
                  :description "Comment and uncomment region")
  (cheatsheet-add :group 'Editing
                  :key "s-k"
                  :description "Kills the entire word your cursor is in")
  (cheatsheet-add :group 'Editing
                  :key "M-z"
                  :description "zap-up-to-char")
  (cheatsheet-add :group 'Editing
                  :key "s-w"
                  :description "Copy whole word")
  (cheatsheet-add :group 'Editing
                  :key "s-@"
                  :description "Copies a line where your cursor is in")
  (cheatsheet-add :group 'Editing
                  :key "s-%"
                  :description "Kills whole line where your cursor is in")
  (cheatsheet-add :group 'Editing
                  :key "yank-file-name"
                  :description "Yanks current buffer file name and its path to the kill ring")
  (cheatsheet-add :group 'Editing
                  :key "C-`"
                  :description "ido-find-file-in-tag-files")
  (cheatsheet-add :group 'Editing
                  :key "M-x reverse-paragraphs"
                  :description "Reverse the order of paragraphs in a region")
  (cheatsheet-add :group 'Editing
                  :key "M-x xah-insert-column-az"
                  :description "Inserts numbers or letters in column. Read dgm.org")
  (cheatsheet-add :group 'Editing
                  :key "C-x a"
                  :description "Join this line to previous and fix up whitespace at join")
  (cheatsheet-add :group 'Editing
                  :key "s-\\"
                  :description "align-regexp")
  (cheatsheet-add :group 'Editing
                  :key "s-/"
                  :description "indent-region")
  (cheatsheet-add :group 'Editing
                  :key "M-/"
                  :description "hippie-expand")
  (cheatsheet-add :group 'Editing
                  :key "s-c"
                  :description "helm-colors")
  (cheatsheet-add :group 'Editing
                  :key "s-!"
                  :description "toggle-theme")
  (cheatsheet-add :group 'Editing
                  :key "s-/"
                  :description "indent-region")
  (cheatsheet-add :group 'Editing
                  :key "s-y"
                  :description "ivy-yasnippet")
  (cheatsheet-add :group 'Editing
                  :key "s-#"
                  :description "sudo-edit")
  (cheatsheet-add :group 'Editing-Killing
                  :key "M-w"
                  :description "kill-ring-save: Save the region as if killed, but don’t kill it. When no active region, copy a single line instead")
  (cheatsheet-add :group 'Editing-Killing
                  :key "C-w"
                  :description "kill-region: Kill text between point and mark. When no active region, kill single line instead")
  (cheatsheet-add :group 'Editing-Killing
                  :key "C-k"
                  :description "kill-line and kill-visual-line. kill a line, including whitespace characters until next non-whitespace character of next line")
  (cheatsheet-add :group 'Editing-MultipleRegions
                  :key "s-q"
                  :description "Mark next occurence of a region and edit all al once")
  (cheatsheet-add :group 'Editing-MultipleRegions
                  :key "C-;"
                  :description "iedit: Edit multiple regions in the same way simultaneously")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m l"
                  :description "mc/edit-lines: When active region spans multiple lines, add cursor to each line")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m a"
                  :description "mc/edit-beginnings-of-lines")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m e"
                  :description "mc/edit-ends-of-lines")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m E"
                  :description "mc/mark-more-like-this-extended")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m N"
                  :description "mc/mark-next-like-this. MC not based on continuous lines, but on keywords")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m P"
                  :description "mc/mark-previous-like-this. MC not based on continuous lines, but on keywords")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m u"
                  :description "mc/unmark-next-like-this. MC not based on continuous lines, but on keywords")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m U"
                  :description "mc/unmark-previous-like-this. MC not based on continuous lines, but on keywords")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m A"
                  :description "mc/mark-all-like-this: Works on same line. Mark region 1st. MC based on keywords")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m z"
                  :description "mc/insert-numbers")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m r"
                  :description "mc/mark-all-in-region")
  (cheatsheet-add :group 'Editing-MultipleCursors
                  :key "C-c C-m h"
                  :description "mc-hide-unmatched-lines-mode")
  (cheatsheet-add :group 'Email
                  :key "C-*"
                  :description "mu4e")
  (cheatsheet-add :group 'Email
                  :key "a"
                  :description "Lists possible actions when inside message like Show In Browser or Show Thread")
  (cheatsheet-add :group 'Expressions-Eval
                  :key "C-x C-e"
                  :description "sanityinc/eval-last-sexp-or-region")
  (cheatsheet-add :group 'Expressions-Copying
                  :key "C-M w"
                  :description "sp-copy-sexp: Copies next bit of the sexp")
  (cheatsheet-add :group 'Expressions-Find
                  :key "C-c f"
                  :description "find-function")
  (cheatsheet-add :group 'Expressions-Find
                  :key "C-c ."
                  :description "find-function-at-point")
  (cheatsheet-add :group 'Expressions-Killing
                  :key "C-M k"
                  :description "sp-kill-sexp: Kills next bit of the sexp")
  (cheatsheet-add :group 'Expressions-Killing
                  :key "C-k"
                  :description "sp-kill-hybrid-sexp: Kills sexp from point onwards")
  (cheatsheet-add :group 'Expressions-Killing
                  :key "M-k"
                  :description "sp-backward-kill-sexp: Kills previous bit of sexp")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M a"
                  :description "Begin of function")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M e"
                  :description "End of function")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M f"
                  :description "sp-forward-sexp: Block movement")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M b"
                  :description "sp-backward-sexp: Block movement")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M n"
                  :description "sp-next-sexp: Top-level-ish transversal")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M p"
                  :description "sp-previous-sexp: Top-level-ish transversal")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "M-n"
                  :description "smartscan-symbol-go-forward. Set in elpa/smartscan.el")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "M-p"
                  :description "smartscan-symbol-go-backward. Set in elpa/smartscan.el")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "M-'"
                  :description "smartscan-symbol-replace. Set in smartscan.el")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-<down>"
                  :description "sp-down-exp: Traversing lists")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-<up>"
                  :description "sp-up-exp: Traversing lists")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "M-<down>"
                  :description "sp-backward-down-sexp: Traversing lists")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "M-<up>"
                  :description "sp-backward-up-sexp: Traversing lists")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M a"
                  :description "Begin of function")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M e"
                  :description "End of function")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M f"
                  :description "sp-forward-sexp: Block movement")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M b"
                  :description "sp-backward-sexp: Block movement")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-M n"
                  :description "sp-next-sexp: Top-level-ish transversal")
  (cheatsheet-add :group 'Expressions-Movement
                  :key "C-S b"
                  :description "sp-backward-symbol: Free-form movement")
  (cheatsheet-add :group 'Expressions-Movements
                  :key "C-S f"
                  :description "sp-forward-symbol: Free-form movement")
  (cheatsheet-add :group 'Expressions-Marking
                  :key "C-M SPC"
                  :description "mark-sexp function of lisp.el")
  (cheatsheet-add :group 'Expressions-Org
                  :key "s-~"
                  :description "Wrap region with matching tilde in Org")
  (cheatsheet-add :group 'Expressions-Org
                  :key "s-="
                  :description "Wrap region with matching equality in Org")
  (cheatsheet-add :group 'Expressions-Wrapping
                  :key "C-M SPC and Wrapping symbol"
                  :description "Wrap region with matching characters-parentheses, brackets, etc-depending on mode")
  (cheatsheet-add :group 'Expressions-Wrapping
                  :key "C-c and Parenthesis"
                  :description "Wrap word with matching characters-parentheses, brackets, etc")
  (cheatsheet-add :group 'Expressions-Wrapping-Slurp
                  :key "C-<right>"
                  :description "sp-forward-slurp-sexp: Slurping extends the inclusion of parentheses")
  (cheatsheet-add :group 'Expressions-Wrapping-Slurp
                  :key "C-<left>"
                  :description "sp-backward-slurp-sexp: Slurping extends the inclusion of parentheses")
  (cheatsheet-add :group 'Expressions-Wrapping-Barf
                  :key "M-<right>"
                  :description "sp-forward-barf-sexp: Burfing contracts the extension of parentheses")
  (cheatsheet-add :group 'Expressions-Wrapping-Barf
                  :key "M-<left>"
                  :description "sp-backward-barf-sexp: Barfing contracts the extension of parentheses")
  (cheatsheet-add :group 'Expressions-Unwrapping
                  :key "M-\["
                  :description "sp-backward-unwrap-sex: Unwraps current exp")
  (cheatsheet-add :group 'Expressions-Unwrapping
                  :key "M-\]"
                  :description "sp-unwrap-sex: Unwrapps next exp")
  (cheatsheet-add :group 'Expressions-Swapping
                  :key "C-M t"
                  :description "sp-transpose-sexp")
  (cheatsheet-add :group 'EXWM
                  :key "s-e"
                  :description "helm-run-external-command")
  (cheatsheet-add :group 'EXWM
                  :key "s-r"
                  :description "exwm-reset: Reset the state of the selected window: non-fullscreen, line-mode, etc")
  (cheatsheet-add :group 'EXWM
                  :key "C-q"
                  :description "exwm-input-send-next-key: the next key is sent literally to the application")
  (cheatsheet-add :group 'EXWM
                  :key "s-&"
                  :description "ambrevar/exwm-start: Open in external application")
  (cheatsheet-add :group 'Find-char
                  :key "C-o"
                  :description "avy-goto-word-1: Jump to char at word start")
  (cheatsheet-add :group 'Find-char
                  :key "s-z"
                  :description "avy-goto-char: Jump to any char in window")
  (cheatsheet-add :group 'Find-Files
                  :key "C-x f"
                  :description "ido-recentf-open: Find recent file")
  (cheatsheet-add :group 'Find-Files
                  :key "C-x C-f"
                  :description "helm-find-files")
  (cheatsheet-add :group 'Find-Files
                  :key "M-x helm-for-filess"
                  :description "Find in opened, recent, bookmarked files; files in current dir; files anywhere with locate")
  (cheatsheet-add :group 'Find-Files
                  :key "C-x C-g"
                  :description "deft-find-file")
  (cheatsheet-add :group 'Find-Files
                  :key "s-d"
                  :description "helm-find frontend for *nix FIND command in current dir; C-u chooses dir")
  (cheatsheet-add :group 'Find-Files
                  :key "s-D"
                  :description "helm-locate frontend of *unix LOCATE command")
  (cheatsheet-add :group 'Find-Dirs-Projectile
                  :key "s-+"
                  :description "helm-projectile-find-dir: Find dirs in current P project")
  (cheatsheet-add :group 'Find-Files-Projectile
                  :key "C-c p a"
                  :description "List files with same name, different extensions in current P project")
  (cheatsheet-add :group 'Find-Files-Projectile
                  :key "s-|"
                  :description "helm-projectile-find-file-dwim: Find file in P project based on context at point")
  (cheatsheet-add :group 'Find-Files-Projectile
                  :key "C-c p e"
                  :description "List recently visited files in current P project")
  (cheatsheet-add :group 'Find-Files-Projectile
                  :key "s-\`"
                  :description "helm-projectile-find-other-file: Same name, different extension")
  (cheatsheet-add :group 'Find-Occurrences
                  :key "s-o"
                  :description "helm-occur: TAB to temporarily move to highlighted match")
  (cheatsheet-add :group 'Find-Occurrences
                  :key "s-i"
                  :description "swiper")
  (cheatsheet-add :group 'Find-Occurences
                  :key "s-\'"
                  :description "helm-regexp to test regexp interactively and save it. Actions on C-z")
  (cheatsheet-add :group 'Find-Ocurrences
                  :key "M-x helm-swoop"
                  :description "Show lines matching a pattern; cursor jumps to the line we go to")
  (cheatsheet-add :group 'Find-Org
                  :key "s-p"
                  :description "helm-org-in-buffer-headings: Find Org heading")
  (cheatsheet-add :group 'Find-Org
                  :key "s-u"
                  :description "Rifle search in Org files")
  (cheatsheet-add :group 'Grep
                  :key "search-all-buffers"
                  :description "Greps all opened buffers")
  (cheatsheet-add :group 'Grep
                  :key "s-f"
                  :description "helm-ag: grep-like program implemented in C")
  (cheatsheet-add :group 'Grep-Helm
                  :key "C-c h w"
                  :description "wgrep-helm: Edit a helm-grep-mode buffer and apply those changes to the file buffer")
  (cheatsheet-add :group 'Grep-Projectile
                  :key "s-g"
                  :description "helm-projectile-grep: Search symbol at point. If region active, searches region")
  (cheatsheet-add :group 'Grep-Projectile
                  :key "C-s"
                  :description "Grep in project when using P. C-u for recursive grep")
  (cheatsheet-add :group 'Helm
                  :key "C-c h"
                  :description "Helm prefix / If in C-x C-f session, list visited files/dirs")
  (cheatsheet-add :group 'Helm
                  :key "C-c C-i"
                  :description "Insert selection: columns")
  (cheatsheet-add :group 'Helm
                  :key "C-w"
                  :description "Yank word at point into Helm prompt")
  (cheatsheet-add :group 'Helm
                  :key "M-n"
                  :description "Yank symbol at point into Helm prompt")
  (cheatsheet-add :group 'Helm-Bibtex
                  :key "C-c \]"
                  :description "helm-bibtex: search BibTeX files for refs to insert into the current document")
  (cheatsheet-add :group 'Helm-Menu
                  :key "C-c h w"
                  :description "helm-buffer-switch-other-window: Works on a helm menu like the one by C-x b")
  (cheatsheet-add :group 'Helm-Menu
                  :key "C-c h k"
                  :description "helm-buffer-run-kill-persistent: Works on a helm menu like the one by C-x b")
  (cheatsheet-add :group 'Help
                  :key "helm-descbinds"
                  :description "Currently active key bindings are searcheable")
  (cheatsheet-add :group 'Help
                  :key "C-c ?"
                  :description "Help in a Helm session")
  (cheatsheet-add :group 'Help
                  :key "s-m"
                  :description "helm-man-woman: Manual entries for *nix commands")
  (cheatsheet-add :group 'Help
                  :key "s-a"
                  :description "helm-apropos for commands, functions, variables and faces. Original in C-h d")
  (cheatsheet-add :group 'Help
                  :key "s-\)"
                  :description "Search emacs, elisp and CL info pages")
  (cheatsheet-add :group 'Help
                  :key "helm-info-*"
                  :description "Search info nodes for various topics like emacs, elisp, gdb, etc. TAB to view")
  (cheatsheet-add :group 'Help
                  :key "C-h f"
                  :description "helpful-callable: Show help for function, macro or special form named SYMBOL")
  (cheatsheet-add :group 'Help
                  :key "C-h k"
                  :description "helpful-key")
  (cheatsheet-add :group 'Help
                  :key "C-h v"
                  :description "helpful-variable")
  (cheatsheet-add :group 'History
                  :key "C-c C-l"
                  :description "History in Eshell, Shell and Minibuffer")
  (cheatsheet-add :group 'iMenu
                  :key "s-?"
                  :description "ido-goto-symbol. Refresh imenu and jump to a place in the buffer using Ido")
  (cheatsheet-add :group 'iMenu
                  :key "M-i"
                  :description "helm-semantic-or-imenu")
  (cheatsheet-add :group 'Input
                  :key "s-s"
                  :description "mrb/set-input-method: Change input method to Spanish prefix")
  (cheatsheet-add :group 'Input
                  :key "C-\\"
                  :description "toggle-input-method. Use one back slash")
  (cheatsheet-add :group 'Kill
                  :key "M-y"
                  :description "helm-show-kill-ring")
  (cheatsheet-add :group 'Kill-Projectile
                  :key "M-D"
                  :description "Kill marked item in P session")
  (cheatsheet-add :group 'Lisp
                  :key "C-c h TAB"
                  :description "helm-lisp-completion-at-point for elisp")
  (cheatsheet-add :group 'Lisp
                  :key "M-:"
                  :description "Evaluate EXPRESSION and pretty-print its value.")
  (cheatsheet-add :group 'Lisp
                  :key "s-\("
                  :description "helm-eval-expression-with-eldoc interactively evaluates elisp exp")
  (cheatsheet-add :group 'Lisp
                  :key "s-\)"
                  :description "ap/helm-info-emacs-elisp-cl: Emacs and eLisp manual with Helm")
  (cheatsheet-add :group 'Mark
                  :key "C-."
                  :description "Pushes point to mark-ring and does not activate the region")
  (cheatsheet-add :group 'Mark
                  :key "C-SPC"
                  :description "cua-set-mark: Set mark at where point is, clear mark, or jump to mark.")
  (cheatsheet-add :group 'Mark
                  :key "M-`"
                  :description "Jumps to the local mark, respecting the mark-ring order")
  (cheatsheet-add :group 'Mark
                  :key "s-RET"
                  :description "helm-all-mark-rings: Find mark in local and global rings")
  (cheatsheet-add :group 'Mark
                  :key "s-SPC"
                  :description "er/expand-region")
  (cheatsheet-add :group 'Mark
                  :key "s-BKSPC"
                  :description "er/contract-region")
  (cheatsheet-add :group 'Mark
                  :key "C-x C-x"
                  :description "Exchange point and mark without activating region")
  (cheatsheet-add :group 'Minibuffer
                  :key "C-M-e"
                  :description "Miniedit: enter minibuffer edit")
  (cheatsheet-add :group 'Minibuffer
                  :key "C-c C-c"
                  :description "Miniedit: exit minibuffer edit")
  (cheatsheet-add :group 'Movement
                  :key "s-,"
                  :description "beginning-of-buffer")
  (cheatsheet-add :group 'Movement
                  :key "s-."
                  :description "end-of-buffer")
  (cheatsheet-add :group 'Org
                  :key "C-c @"
                  :description "Insert as Org link current highlighted file in P session")
  (cheatsheet-add :group 'Org
                  :key "M-Shift-<right>"
                  :description "Demote heading and all subheadings")
  (cheatsheet-add :group 'Org
                  :key "M-<right>"
                  :description "Demote only current heading but NOT its subheadings")
  (cheatsheet-add :group 'Org
                  :key "C-c C-o"
                  :description "org-open-at-point: Open links")
  (cheatsheet-add :group 'Org
                  :key "C-c C-x M-w"
                  :description "org-copy-subtree. Copy subtree to kill ring. With a numeric prefix argument N, copy the N sequential subtrees")
  (cheatsheet-add :group 'Org
                  :key "C-c C-n"
                  :description "Move to next heading")
  (cheatsheet-add :group 'Org-recipes
                  :key "C-c i"
                  :description "Insert raw code under a heading")
  (cheatsheet-add :group 'Packages
                  :key "M-x try"
                  :description "Try a package before installing for good")
  (cheatsheet-add :group 'Password
                  :key "M-x password-store"
                  :description "password-store")
  (cheatsheet-add :group 'Password
                  :key "M-x helm-pass"
                  :description "helm-pass")
  (cheatsheet-add :group 'Prefixes
                  :key "C-x r"
                  :description "Registers and bookmarks")
  (cheatsheet-add :group 'Prefixes
                  :key "C-c"
                  :description "Org commands")
  (cheatsheet-add :group 'Prefixes
                  :key "C-x r"
                  :description "Registers and bookmarks")
  (cheatsheet-add :group 'Projectile
                  :key "s-h"
                  :description "helm-projectile: select project and buffers/files")
  (cheatsheet-add :group 'Projectile
                  :key "s-j"
                  :description "helm-projectile-find-file: Powerful P session for file management")
  (cheatsheet-add :group 'Reading
                  :key "xah-toggle-margin-right"
                  :description "For reading novel or documentation: Toggle right margin")
  (cheatsheet-add :group 'Recent
                  :key "s--"
                  :description "helm-resume previous H session, inputs included. C-u lets choose Helm buffer")
  (cheatsheet-add :group 'Register
                  :key "s-x"
                  :description "helm-register. Then TAB or RET to insert. Equivalent to C-x r i")
  (cheatsheet-add :group 'Redshift
                  :key "C-\("
                  :description "Redshift on")
  (cheatsheet-add :group 'Redshift
                  :key "C-\)"
                  :description "Redshift off")
  (cheatsheet-add :group 'Replace
                  :key "C-c s"
                  :description "Global replacement by dynamically building regular expressions")
  (cheatsheet-add :group 'Replace
                  :key "C-c q"
                  :description "Decide to replace per match by dynamically building regular expressions")
  (cheatsheet-add :group 'Replace
                  :key "M-%"
                  :description "anzu-query-replace")
  (cheatsheet-add :group 'Replace
                  :key "C-M-%"
                  :description "anzu-query-replace-regexp")
  (cheatsheet-add :group 'Revert
                  :key "C-c r"
                  :description "Replace buffer text visited file on disk. C-u offers to revert from latest auto-save file")
  (cheatsheet-add :group 'Scratch
                  :key "M-x scratch"
                  :description "With C-u you choose mode, otherwise current mode is used")
  (cheatsheet-add :group 'Screenshot
                  :key "s-["
                  :description "Takes a fullscreen screenshot of the current workspace")
  (cheatsheet-add :group 'Screenshot
                  :key "s-]"
                  :description "Takes a screenshot of a region selected by the user")
  (cheatsheet-add :group 'Security
                  :key "s-_"
                  :description "daedreth/lock-screen")
  (cheatsheet-add :group 'Shell
                  :key "C-x m"
                  :description "Open eshell")
  (cheatsheet-add :group 'Shell
                  :key "C-x M"
                  :description "Open eshell even if one is active")
  (cheatsheet-add :group 'Shell
                  :key "M-e"
                  :description "Open Eshell when in P session")
  (cheatsheet-add :group 'Skeleton
                  :key "M-x article-skeleton"
                  :description "Inserts a Latex article skeleton into buffer")
  (cheatsheet-add :group 'Skeleton
                  :key "M-x beamer-skeleton"
                  :description "Inserts a Latex beamer skeleton into current buffer")
  (cheatsheet-add :group 'Spelling
                  :key "C-M-$"
                  :description "Change dictionary")
  (cheatsheet-add :group 'Spelling
                  :key "M-$"
                  :description "Spellcheck word or region")
  (cheatsheet-add :group 'System
                  :key "s-t"
                  :description "helm-top for TOP program to display Linux processes")
  (cheatsheet-add :group 'Typing
                  :key "M-x typing-of-emacs"
                  :description "Practise typing")
  (cheatsheet-add :group 'Undo-Propose
                  :key "M-x undo-propose"
                  :description "Cycle through the list of undo's as normal in a new temporary buffer")
  (cheatsheet-add :group 'Undo-Propose
                  :key "C-c C-c"
                  :description "Add the chain of undo's as a single edit to the undo history")
  (cheatsheet-add :group 'Undo-Propose
                  :key "C-c C-d"
                  :description "Ediff the proposed chain of undo's")
  (cheatsheet-add :group 'Undo-Propose
                  :key "C-c C-k"
                  :description "Cancel undo-propose")
  (cheatsheet-add :group 'Undo-Tree
                  :key "C-z"
                  :description "undo-tree-undo: Undo changes. C-u limits it to current region")
  (cheatsheet-add :group 'Undo-Tree
                  :key "C-x u"
                  :description "undo-tree-visualize: undo. Visualize the current buffer's undo tree")
  (cheatsheet-add :group 'Weather
                  :key "M-x forecast"
                  :description "forecast package")
  (cheatsheet-add :group 'Weather
                  :key "M-x wttrin"
                  :description "forecast package with drawings")
  (cheatsheet-add :group 'Weather
                  :key "M-x sunrise-sunset"
                  :description "Shows times for sunrise and sunset in minibuffer. More settings in dgm.org")
  (cheatsheet-add :group 'Windows
                  :key "C-x 1"
                  :description "zygospore-toggle-delete-other-windows RET: lets you revert C-x 1-delete-other-window-by pressing C-x 1 again")
  (cheatsheet-add :group 'Windows
                  :key "M-P"
                  :description "ace-window")
  (cheatsheet-add :group 'Windows
                  :key "C-x 2"
                  :description "Vertical split windows and move to new window")
  (cheatsheet-add :group 'Windows
                  :key "C-x 3"
                  :description "Horizontal split windows and move to new window")
  (cheatsheet-add :group 'Windows
                  :key "C-$"
                  :description "ambrevar/toggle-window-split: Transpose windows")
  (cheatsheet-add :group 'Windows
                  :key "C-&"
                  :description "rotate-windows")
  (cheatsheet-add :group 'Windows
                  :key "s-M-<left>"
                  :description "shrink-window-horizontally")
  (cheatsheet-add :group 'Windows
                  :key "s-M-<right>"
                  :description "enlarge-window-horizontally")
  (cheatsheet-add :group 'Windows
                  :key "s-M-<down>"
                  :description "shrink-window")
  (cheatsheet-add :group 'Windows
                  :key "s-M-<up>"
                  :description "enlarge-window")
  (cheatsheet-add :group 'Window-Movement
                  :key "s-<left>"
                  :description "windmove-left")
  (cheatsheet-add :group 'Window-Movement
                  :key "s-<right>"
                  :description "windmove-right")
  (cheatsheet-add :group 'Window-Movement
                  :key "s-<up>"
                  :description "windmove-up")
  (cheatsheet-add :group 'Window-Movement
                  :key "s-<down>"
                  :description "windmove-down")
  :bind ("C-c C-s" . cheatsheet-show)
  )

(global-set-key (kbd "s-,") 'beginning-of-buffer)
(global-set-key (kbd "s-.") 'end-of-buffer)

(provide 'dgm)

(message "Starter Kit User (DGM) File loaded.")
