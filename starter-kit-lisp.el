(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
;; (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'run-starter-kit-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
;; (add-hook 'emacs-lisp-mode-hook 'idle-highlight)


(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil) 
;; (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)

(add-hook 'clojure-mode-hook 'run-starter-kit-coding-hook)
;; (add-hook 'clojure-mode-hook 'idle-highlight)

(font-lock-add-keywords 'clojure-mode
                        '(("(\\|)" . 'esk-paren-face)))

(defface esk-clojure-trace-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

(setq esk-clojure-trace-face 'esk-clojure-trace-face)

;; This will make relevant lines stand out more in stack traces
(defun sldb-font-lock ()
  (font-lock-add-keywords nil
                          '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)"
                             1 esk-clojure-trace-face)
                            ("[0-9]+: \\(java.*\\)"
                             1 esk-clojure-trace-face)
                            ("[0-9]+: \\(swank.*\\)"
                             1 esk-clojure-trace-face)
                            ("\\[\\([A-Z]+\\)\\]"
                             1 font-lock-function-name-face))))

(add-hook 'sldb-mode-hook 'sldb-font-lock)

(defun slime-jump-to-trace (&optional on)
  "Jump to the file/line that the current stack trace line references.
Only works with files in your project root's src/, not in dependencies."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp "[0-9]: \\([^$(]+\\).*?\\([0-9]*\\))")
    (let ((line (string-to-number (match-string 2)))
          (ns-path (split-string (match-string 1) "\\."))
          (project-root (locate-dominating-file default-directory "src/")))
      (find-file (format "%s/src/%s.clj" project-root
                         (mapconcat 'identity ns-path "/")))
      (goto-line line))))

(eval-after-load 'slime
  '(progn
     (defalias 'sldb-toggle-details 'slime-jump-to-trace)
     (defun sldb-prune-initial-frames (frames)
       "Show all stack trace lines by default."
       frames)))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

;; You might like this, but it's a bit disorienting at first:


(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session.

Kills existing SLIME session, if any."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "pom.xml"))))
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("src/" "target/classes/" "test/"))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))

  ;; symbols for some overlong function names
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode
      (mapcar
       (lambda (pair)
         `(,(car pair)
           (0 (progn (compose-region
                      (match-beginning 0) (match-end 0)
                      ,(cadr pair))
                     nil))))
       '(("\\<fn\\>" ?ƒ)
         ("\\<comp\\>" ?∘)
         ("\\<partial\\>" ?þ)
         ("\\<complement\\>" ?¬)))))

;;(use-package scheme
;;  :mode ("\\.scm\\'" . scheme-mode))

;;(add-hook 'scheme-mode-hook 'geiser-mode)

;;(add-hook 'scheme-mode-hook 'run-starter-kit-coding-hook)
;; ;; (add-hook 'scheme-mode-hook 'idle-highlight)
;;(font-lock-add-keywords 'scheme-mode
;;			'(("(\\|)" . 'esk-paren-face)))

(add-hook 'lisp-mode-hook 'run-starter-kit-coding-hook)
;; (add-hook 'lisp-mode-hook 'idle-highlight)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

(use-package racket-mode
  :ensure t
  :commands racket-mode
  :mode 
  ("\\.rkt[dl]?\\'" . racket-mode) ;; to enable racket-mode when I open a .rkt file. See http://coldnew.github.io/coldnew-emacs/#orgheadline1
  :config 
  (setq racket-smart-open-bracket-enable t))

(add-hook 'racket-mode-hook 'run-starter-kit-coding-hook)
;; (add-hook 'racket-mode-hook 'idle-highlight)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

;;(add-hook 'racket-mode-hook
;;          (lambda ()
;;            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

;; (sp-local-pair 'racket-mode "'" nil :actions nil) 
;; (sp-local-pair 'racket-mode "`" nil :actions nil)

;;(use-package geiser
;;  :ensure t
;;  :defer t
;;  :config
;;  (setq geiser-active-implementations '(racket chicken guile)) 
;;  (setq geiser-default-implementation '(racket)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode +1)
    (sp-use-paredit-bindings)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)))

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil) 
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil) 

(sp-local-pair 'racket-mode "'" nil :actions nil) 
(sp-local-pair 'racket-mode "`" nil :actions nil)

(sp-local-pair 'fundamental-mode "'" nil :actions nil) 
(sp-local-pair 'fundamental-mode "`" nil :actions nil)

;;   (smartparens-global-mode 1)
;;  (require 'smartparens-config) ;; To use the default configuration that smartparens provides for Lisp modes generally and for racket-mode specifically

  (message "Starter Kit Lisp loaded.")
