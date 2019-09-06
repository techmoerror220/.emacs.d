;; commented out because ipython not available  (dolist (package '(python-mode ipython))
  (dolist (package '(python-mode))
    (unless (package-installed-p package)
      (package-install package)))

  (autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
    
  (setq python-shell-interpreter "ipython3")
;  (setq python-shell-interpreter "ipython")
;  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq python-shell-interpreter-args "--simple-prompt -pprint")
;;   python-shell-interpreter "ipython"
;;   python-shell-interpreter "python3"

;;   python-shell-interpreter-args ""
;;   python-shell-interpreter-args "--simple-prompt --pprint")
   
;;;;   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;;;   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;;;   python-shell-completion-setup-code
;;;;;     "from IPython.core.completerlib import module_completion"
;;   python-shell-completion-module-string-code
;;;;   python-shell-completion-string-code
;;;;   "';'.join(module_completion('''%s'''))\n"
;;;;   python-shell-completion-string-code
;;;;     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (when (require 'cython-mode nil 'no-error)
    (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
    (add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
    (add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode)))

  (with-eval-after-load "python"
    ;; try to get indent/completion working nicely
    ;;;;;;; dgm on 9 march 2019: (setq python-indent-trigger-commands '(company-indent-for-tab-command indent-for-tab-command yas-expand yas/expand))
    ;; readline support is wonky at the moment
    (setq python-shell-completion-native-enable nil))

  ;; simple evaluation with C-ret originally in Ista's code that I, dgm, have changed to S-return to mimic behaviour in R as explained by the great KHJ in
  ;; https://kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
;;  (require 'eval-in-repl-python)
;;  (define-key python-mode-map "\C-c\C-c" 'eir-eval-in-python)
;;  (define-key python-mode-map (kbd "<M-return>") 'eir-eval-in-python)
;;  (define-key python-mode-map (kbd "<S-return>") 'eir-eval-in-python)

  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (with-eval-after-load 'python
      '(let ((python-shell-completion-native-enable t)
             (python-shell-completion-native-output-timeout
              python-shell-completion-native-try-output-timeout)
             (python-shell-completion-native-get-completions
              (get-buffer-process (current-buffer))
              nil "_")))
      ))

(use-package elpy)
(elpy-enable)
(use-package live-py-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; (elpy-use-ipython)
;; (elpy-use-ipython "ipython3") ;; error "elpy-use-ipython is deprecated; see https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup")


;; tip from https://github.com/jorgenschaefer/elpy/issues/992
;; to correct IPython 5's new prompt behavior that spitted out lots of nonsense and unreadeable characters as if it was a binary file
;; See also https://emacs.stackexchange.com/questions/16637/how-to-set-up-elpy-to-use-python3
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")


;; tips from: "Emacs - the Best Python Editor?" at https://realpython.com/blog/python/emacs-the-best-python-editor/
;; Elpy comes with =flymake= by default to support syntax checking. However =flycheck= gives realtime syntax checking.
;; But =flycheck= slows emacs to death, so I disable it!
;; (when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Now if we make pep8 errors when we save the file the errors will be corrected automatically
(use-package py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; https://github.com/jorgenschaefer/elpy/issues/979
   ;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
   ;; (setq python-shell-interpreter "python3")

(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

;; Yuksel says there is a bug in =elpy= mode so that it conflicts with yasnippet expansion. He proposes this (see: https://www.youtube.com/watch?v=0kuCeS-mfyc)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; (define-key global-map (kbd "C-;") 'iedit-mode)

;; not sure where this goes, but I guess I need it somewhere
;; (require 'jedi)

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

;;(setq jedi:setup-keys t)
;;(setq jedi:complete-on-dot t)
;;(add-hook 'python-mode-hook 'jedi:setup)

;;(setq jedi-custom-file (expand-file-name "jedi-custom.el" user-emacs-directory))
;;(when (file-exists-p jedi-custom-file)
;;  (load jedi-custom-file))

(provide 'starter-kit-python)

(message "Starter Kit Python File loaded.")
