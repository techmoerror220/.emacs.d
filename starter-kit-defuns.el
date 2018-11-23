(require 'thingatpt)
(require 'imenu)

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun local-column-number-mode ()
    (make-local-variable 'column-number-mode)
    (column-number-mode t))
  
  (defun local-comment-auto-fill ()
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode t))
  
;;  (defun turn-on-idle-highlight-mode ()
;;    (idle-highlight-mode t))

;;  (defun turn-on-hl-line-mode ()
;;    (require 'hl-line+)
;;    (global-hl-line-mode nil)
;;    (toggle-hl-line-when-idle)
;;    (hl-line-when-idle-interval 2))

;; From uncle dave's dot files
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
      
  (defun turn-on-save-place-mode ()
    (setq save-place t))

;; already in dgm.org
;;  (defun turn-on-whitespace ()
;;    (whitespace-mode t))

(add-hook 'starter-kit-coding-hook 'local-column-number-mode)

(add-hook 'starter-kit-coding-hook 'local-comment-auto-fill)

(add-hook 'starter-kit-coding-hook 'starter-kit-pretty-lambdas)

;; (add-hook 'starter-kit-coding-hook 'turn-on-hl-line-mode) ;; hl-line+ not found in emacs 27

;; (add-hook 'starter-kit-coding-hook 'turn-on-idle-highlight-mode)

(defun run-starter-kit-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'starter-kit-coding-hook))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
;;  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Credit: Alex Ott
(defun cleanup-buffer-hook ()
  "A hook for cleanup-buffer. cleanup-buffer-modes should be a list
  of modes you want the cleanup-buffer function applied to routinely. "
  (when (member major-mode cleanup-buffer-modes)
    (cleanup-buffer)))
;; now files int he modes listed in cleanup-buffer-mode will be
;; automatically cleaned every time they are saved. 
(add-hook 'before-save-hook 'cleanup-buffer-hook)

(setq cleanup-buffer-modes 
      '(haskell-mode emacs-lisp-mode lisp-mode racket-mode
                     ess-mode erlang-mode clojure-mode ruby-mode))

(defun recentf-ido-find-file () 
  "Find a recent file using Ido." 
  (interactive) 
  (let* ((file-assoc-list 
          (mapcar (lambda (x) 
                    (cons (file-name-nondirectory x) 
                          x)) 
                  recentf-list)) 
         (filename-list 
          (remove-duplicates (mapcar #'car file-assoc-list) 
                             :test #'string=)) 
         (filename (ido-completing-read "Choose recent file: " 
                                        filename-list 
                                        nil 
                                        t))) 
    (when filename 
      (find-file (cdr (assoc filename 
                             file-assoc-list))))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(message "Starter Kit Defuns loaded.")
