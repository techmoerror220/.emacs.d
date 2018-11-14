  (setq insert-directory-program (executable-find "ls"))

  (setq sentence-end-double-space nil)

    (defun fullscreen (&optional f)
      (interactive)
      (set-frame-parameter f 'fullscreen
                           (if (frame-parameter f 'fullscreen) nil 'fullboth)))
    (global-set-key (kbd "C-c f") 'fullscreen)
    (add-hook 'after-make-frame-functions 'fullscreen)

  (load "dired-x")

  (eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(require 'stripe-buffer)
(add-hook 'org-mode-hook 'org-table-stripes-enable)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)

(use-package avy
  :ensure t
  :bind
    ("s-s" . avy-goto-char))  ;; goes literally to any char

(define-key global-map (kbd "C-o") 'avy-goto-word-1) ;; goes to word that starts with a given char



 (add-hook 'prog-mode-hook 'linum-mode)

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(require 'iedit)

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

(global-set-key (kbd "C-;") 'iedit-dwim)

;; dgm's customizations of python
(elpy-enable)
(require 'live-py-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; (elpy-use-ipython)
;; (elpy-use-ipython "ipython3") ;; error "elpy-use-ipython is deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup")


;; tip from https://github.com/jorgenschaefer/elpy/issues/992
;; to correct IPython 5's new prompt behavior that spitted out lots of nonsense and unreadeable characters as if it was a binary file
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")


;; tips from: "Emacs - the Best Python Editor?" at https://realpython.com/blog/python/emacs-the-best-python-editor/
;; Elpy comes with =flymake= by default to support syntax checking. However =flycheck= gives realtime syntax checking.
;; But =flycheck= slows emacs to death, so I disable it!
;; (when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Now if we make pep8 errors when we save the file the errors will be corrected automatically
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; https://github.com/jorgenschaefer/elpy/issues/979
   ;; For elpy
;; (setq elpy-rpc-python-command "python3")
;; For interactive shell
   ;; (setq python-shell-interpreter "python3")

(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

;; Yuksel says there is a bug in =elpy= mode so that it conflicts with yasnippet expansion. He proposes this (see: https://www.youtube.com/watch?v=0kuCeS-mfyc)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; (define-key global-map (kbd "C-;") 'iedit-mode)

;; not sure where this goes, but I guess I need it somewhere
;; (require 'jedi)

(add-hook 'c-mode-common-hook
    (lambda ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  (ggtags-mode 1))))

(setq global-mark-ring-max 5000     ; increase mark ring to contain 5000 entries
      mark-ring-max 5000            ; increase kill ring to contain 5000 entries
      mode-require-final-newline t) ; add a newline to end of file

(setq
 kill-ring-max 5000 ; increase kill-ring capacity
;; kill-whole-line t  ; if NIL, killwhole line and move the next line up / commented out by dgm as it might interere with kill-whole-line-or-region mode
)

;; default to 4 visible spaces to display a tab
(setq-default tab-width 4)

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

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

 (defun my-pkg-init()
   (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
   (clean-aindent-mode t)
   (setq clean-aindent-is-simple-indent t)
   (define-key global-map (kbd "RET") 'newline-and-indent))
 (add-hook 'after-init-hook 'my-pkg-init)

(require 'undo-tree)
;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;; update any change made on file to the current buffer
(global-auto-revert-mode)

(setq-default fill-column 72)

;; always display ibuffer in another window
(setq ibuffer-use-other-window t)

(add-hook 'after-init-hook 'global-company-mode)

(require 'duplicate-thing)
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

;; set dired listing options
(if (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alDhp"))

;; make sure dired buffers end in a slash so we can identify them easily
(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
          (lambda()
             (setq truncate-lines 1)))

;; open files in external programs
;; (from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;; consider replacing with https://github.com/thamer/runner
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

  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;  (require 'flycheck-tip)
;;  (define-key global-map (kbd "\C-c \C-n") 'flycheck-tip-cycle)
;;  (setq flycheck-display-errors-function 'ignore)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'highlight-symbol)

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

;; (require 'info+) no longer available in MELPA

;; A quick major mode help with discover-my-major
(global-unset-key (kbd "C-h h"))        ; original "\C-h h" displays "hello world" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: rainbow-mode              ;;
;;                                    ;;
;; GROUP: Help -> Rainbow             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;;R-mode-hook runs when you open a new source buffer, so anything you put in that will only effect your source buffers.inferior-ess-mode-hook runs when you start an R console, so anything in there should only apply to the console buffer and not the source.
(add-hook 'R-mode-hook 'rainbow-mode)
(add-hook 'inferior-ess-mode-hook 'rainbow-mode)

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

(add-to-list 'load-path "/home/dgm/.emacs.d/src/ado-mode-1.15.1.4/lisp")
(require 'ado-mode)

(require 'which-key)
(which-key-mode)

;; require the main file containing common functions
(require 'eval-in-repl)
(setq comint-process-echoes t)

;; truncate lines in comint buffers
(add-hook 'comint-mode-hook
          (lambda()
            (setq truncate-lines 1)))

;; Scroll down for input and output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

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

;; (load-file "/home/dgm/.emacs.d/src/jedi-starter.el")

;; ;; Global Jedi config vars
;;
;; (defvar jedi-config:use-system-python nil
;;   "Will use system python and active environment for Jedi server.
;; May be necessary for some GUI environments (e.g., Mac OS X)")
;;
;; (defvar jedi-config:with-virtualenv nil
;;   "Set to non-nil to point to a particular virtualenv.")
;;
;; (defvar jedi-config:vcs-root-sentinel ".git")
;;
;; (defvar jedi-config:python-module-sentinel "__init__.py")
;;
;; ;; Helper functions
;;
;; ;; Small helper to scrape text from shell output
;; (defun get-shell-output (cmd)
;;   (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))
;;
;; ;; Ensure that PATH is taken from shell
;; ;; Necessary on some environments without virtualenv
;; ;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
;;
;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
;;   (interactive)
;;   (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator)))
;;
;;
;;   (require 'jedi)
;;
;; ;;  Costumization copied from https://www.youtube.com/watch?v=6BlTGPsjGJk
;; ;;  I think what follows is more robust
;; ;;  ;; Hook up to auto-complete
;; ;;  (add-to-list 'ac-sources 'ac-source-jedi-direct)
;; ;;  ;; Enable for python-mode
;; ;;  (add-hook 'python-mode-hook 'jedi:setup)
;;
;; ;;  (defvar jedi-config:with-virtualenv nil
;; ;;     "set to non-nil to point to a particular virtualenv.")
;;
;;
;; ;;  ;; Variables to help find the project root
;; ;;  (defvar jedi-config:vcs-root-sentinel ".git")
;; ;;  (defvar jedi-config:python-module-sentinel "__init__.py")
;; ;;
;; ;;  ;; Function to find project root given a buffer
;; ;;  (defun get-project-root (buf repo-type init-file)
;; ;;    (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))
;; ;;
;; ;;  (defvar jedi-config:find-root-function 'get-project-root)
;; ;;
;; ;;  ;; And call this on initialization
;; ;;  (defun current-buffer-project-root ()
;; ;;      (funcall jedi-config:find-root-function
;; ;;               (current-buffer)
;; ;;               jedi-config:vcs-root-sentinel
;; ;;               jedi-config:python-module-sentinel))
;;
;;
;; ;; costumization copied straight from https://github.com/wernerandrew/jedi-starter/blob/master/jedi-starter.el
;; ;; Alternative methods of finding the current project root
;;     ;; Method 1: basic
;;     (defun get-project-root (buf repo-file &optional init-file)
;;       "Just uses the vc-find-root function to figure out the project root.
;;        Won't always work for some directory layouts."
;;       (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
;; 	     (project-root (vc-find-root buf-dir repo-file)))
;; 	(if project-root
;; 	    (expand-file-name project-root)
;; 	  nil)))
;;
;;     ;; Method 2: slightly more robust
;;     (defun get-project-root-with-file (buf repo-file &optional init-file)
;;       "Guesses that the python root is the less 'deep' of either:
;;          -- the root directory of the repository, or
;;          -- the directory before the first directory after the root
;;             having the init-file file (e.g., '__init__.py'."
;;
;;       ;; make list of directories from root, removing empty
;;       (defun make-dir-list (path)
;;         (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
;;                           (split-string path "/"))))
;;       ;; convert a list of directories to a path starting at "/"
;;       (defun dir-list-to-path (dirs)
;;         (mapconcat 'identity (cons "" dirs) "/"))
;;       ;; a little something to try to find the "best" root directory
;;       (defun try-find-best-root (base-dir buffer-dir current)
;;         (cond
;;          (base-dir ;; traverse until we reach the base
;;           (try-find-best-root (cdr base-dir) (cdr buffer-dir)
;;                               (append current (list (car buffer-dir)))))
;;
;;          (buffer-dir ;; try until we hit the current directory
;;           (let* ((next-dir (append current (list (car buffer-dir))))
;;                  (file-file (concat (dir-list-to-path next-dir) "/" init-file)))
;;             (if (file-exists-p file-file)
;;                 (dir-list-to-path current)
;;               (try-find-best-root nil (cdr buffer-dir) next-dir))))
;;
;;          (t nil)))
;;
;;       (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
;;              (vc-root-dir (vc-find-root buffer-dir repo-file)))
;;         (if (and init-file vc-root-dir)
;;             (try-find-best-root
;;              (make-dir-list (expand-file-name vc-root-dir))
;;              (make-dir-list buffer-dir)
;;              '())
;;           vc-root-dir))) ;; default to vc root if init file not given
;;
;;     ;; Set this variable to find project root
;;     (defvar jedi-config:find-root-function 'get-project-root-with-file)
;;
;;     (defun current-buffer-project-root ()
;;       (funcall jedi-config:find-root-function
;;                (current-buffer)
;;                jedi-config:vcs-root-sentinel
;;                jedi-config:python-module-sentinel))
;;
;;     (defun jedi-config:setup-server-args ()
;;       ;; little helper macro for building the arglist
;;       (defmacro add-args (arg-list arg-name arg-value)
;;         `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
;;       ;; and now define the args
;;       (let ((project-root (current-buffer-project-root)))
;;
;;         (make-local-variable 'jedi:server-args)
;;
;;         (when project-root
;;           (message (format "Adding system path: %s" project-root))
;;           (add-args jedi:server-args "--sys-path" project-root))
;;
;;         (when jedi-config:with-virtualenv
;;           (message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
;;           (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))
;;
;;     ;; Use system python
;;     (defun jedi-config:set-python-executable ()
;;       (set-exec-path-from-shell-PATH)
;;       (make-local-variable 'jedi:server-command)
;;       (set 'jedi:server-command
;;            (list (executable-find "python") ;; may need help if running from GUI
;;                  (cadr default-jedi-server-command))))
;;
;;     ;; Now hook everything up
;;     ;; Hook up to autocomplete
;;     (add-to-list 'ac-sources 'ac-source-jedi-direct)
;;
;;     ;; Enable Jedi setup on mode start
;;     (add-hook 'python-mode-hook 'jedi:setup)
;;
;;     ;; Buffer-specific server options
;;     (add-hook 'python-mode-hook
;;               'jedi-config:setup-server-args)
;;     (when jedi-config:use-system-python
;;       (add-hook 'python-mode-hook
;;                 'jedi-config:set-python-executable))
;;
;;     ;; And custom keybindings
;;     (defun jedi-config:setup-keys ()
;;       (local-set-key (kbd "M-.") 'jedi:goto-definition)
;;       (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
;;       (local-set-key (kbd "M-?") 'jedi:show-doc)
;;       (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
;;
;;     ;; Don't let tooltip show up automatically
;;     (setq jedi:get-in-function-call-delay 10000000)
;;     ;; Start completion at method dot
;;     (setq jedi:complete-on-dot t)
;;     ;; Use custom keybinds
;;     (add-hook 'python-mode-hook 'jedi-config:setup-keys)
;; )

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: recentf-ext    ;;
;;                         ;;
;; GROUP: Files -> Recentf ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package recentf-ext
  :ensure t)


  ;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: ztree  ;;
;;                 ;;
;; GROUP: No group ;;
  ;;;;;;;;;;;;;;;;;;;;;
;; since ztree works with files and directories, let's consider it in
;; group Files

(require 'ztree-diff)
(require 'ztree-dir)


;;,-----------------
;;| PACKAGE: rebox2
;;|
;;| GROUP: No group
;;`-----------------

;; Ojo: solo funciona si se llama M-x rebox-mode
(require 'rebox2)
(global-set-key [(meta q)] 'rebox-dwin-fill)
(global-set-key [(shift meta q)] 'rebox-dwin-no-fill)


;; PACKAGE: helpful
;; GROUP: No group

                                        ;  https://github.com/Wilfred/helpful

(require 'helpful)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "\C-h f") #'helpful-callable)
(global-set-key (kbd "\C-h v") #'helpful-variable)
(global-set-key (kbd "\C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
;; (global-set-key (kbd "\C-s d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
;;  (global-set-key (kbd "\C-s-f") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
;; (global-set-key (kbd "\C-s-c") #'helpful-command)

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

(use-package magit
  :ensure t
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

(use-package edit-server
  :ensure t
  :config
  (edit-server-start)
;;  (setq edit-server-default-major-mode 'markdown-mode)
(setq edit-server-new-frame nil))

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

(defun hrs/visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (interactive)
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

;; (require 'r-autoyas)
;; (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)

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

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

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

  (use-package gpastel
    :ensure t
    :config 
   (gpastel-start-listening))

(setq default-frame-alist '((font . "Pragmata Pro Mono-16")))
(add-to-list 'default-frame-alist '(line-spacing . 0.06))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")
;      browse-url-generic-program "qutebrowser")

(require 'fortune)
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq apropos-do-all t
      mouse-yank-at-point t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)

;;(winner-mode)
;;(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

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

(use-package whole-line-or-region
  :ensure t)

(add-to-list 'whole-line-or-region-extensions-alist
             '(comment-dwim whole-line-or-region-comment-dwim nil))

(whole-line-or-region-global-mode 1)

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(setenv "TEST_USE_ANSI" "1")

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
  :ensure t)

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

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(use-package ivy
  :ensure t)

;; (setq scroll-conservatively 100)

(use-package ace-window
  :ensure t
  :config
  (ace-window-display-mode)
  :bind ("M-P" . ace-window))

;; (global-set-key (kbd "M-P") 'ace-window)

(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package swiper
  :ensure t)

(global-set-key (kbd "<s-return>") 'swiper)
(setq ivy-display-style 'fancy)

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

(use-package avy
  :ensure t
  :bind
    ("s-s" . avy-goto-char))

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
(global-set-key (kbd "s-,") 'daedreth/copy-whole-word)

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "s-.") 'daedreth/copy-whole-line)

(global-set-key (kbd "s-l") 'kill-whole-line)

(global-subword-mode 1)

(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hungry-delete
  :ensure t
  :config
    (global-hungry-delete-mode))

  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'yas-minor-mode)
  (diminish 'rainbow-mode)

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

(use-package transpose-frame
  :ensure t
  :bind ("C-c t" . transpose-frame))

(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :commands ace-jump-mode
  :bind ("C-s-s" . ace-jump-mode))

(use-package racket-mode
  :ensure t
  :commands racket-mode
  :config
  (setq racket-smart-open-bracket-enable t))

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(chicken guile racket scheme))
  (setq geiser-default-implementation '(racket)))

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode 1))
;;  :bind (("s-n" . smartscan-symbol-go-forward)
;;         ("s-p" . smartscan-symbol-go-backward)))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(use-package scratch
  :ensure t
  :commands scratch)

(use-package visible-mode
  :bind (("s-h" . visible-mode)))

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

  (defun my-mark-current-word (&optional arg allow-extend)
    "Put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "No word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))

(define-key global-map (kbd "C-c x") 'my-mark-current-word)

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

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

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

(global-set-key (kbd "s-/") 'mark-whole-word)

(setq next-line-add-newlines t)

(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

(use-package visible-mark
  :ensure t
  :defer 1
  :init
  (global-visible-mark-mode 1))

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

(require 'etags)
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

(message "Starter Kit User (DGM) File loaded.")
