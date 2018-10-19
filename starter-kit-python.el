;; commented out because ipython not available  (dolist (package '(python-mode ipython))
  (dolist (package '(python-mode))
    (unless (package-installed-p package)
      (package-install package)))

(autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
    
  (setq
;;   python-shell-interpreter "ipython"
   python-shell-interpreter "python3"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
;;   python-shell-completion-module-string-code
   python-shell-completion-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(when (require 'cython-mode nil 'no-error)
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode)))

(message "Starter Kit Python loaded.")

(with-eval-after-load "python"
    ;; try to get indent/completion working nicely
    (setq python-indent-trigger-commands '(company-indent-for-tab-command indent-for-tab-command yas-expand yas/expand))
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
