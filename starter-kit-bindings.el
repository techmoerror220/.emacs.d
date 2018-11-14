(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev                      ;; try to expand word "dynamically", searching the current buffer.
   try-expand-dabbrev-all-buffers          ;; try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill            ;; try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially        ;; try to complete text as a file name, as many characters as unique.
   try-complete-file-name                  ;; try to complete text as a file name.
   try-complete-all-abbrevs                ;; try to expand word before point accoriding to all abbrev tables.
   try-expand-list                         ;; try to complete the current line to an entire line in the buffer.
   try-expand-line                         ;; try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially      ;; try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol)               ;; try to complete word as an Emacs Lisp symbol
 )

(global-set-key [f1] 'menu-bar-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)
  
  (require 'visual-regexp)
  (define-key global-map (kbd "C-c r") 'vr/replace) ;; note from dgm: now it seems these keys are binded to opening =helm-bibtex=
  (define-key global-map (kbd "C-c q") 'vr/query-replace)

(setq ibuffer-saved-filter-groups
      '(("home"
	     ("emacs-config" (or (filename . ".emacs.d")
			                 (filename . "emacs-config")))
	     ("Org" (or (mode . org-mode)
		            (filename . "OrgMode")))
	     ("Web Dev" (or (mode . html-mode)
			            (mode . css-mode)))
	     ("Magit" (name . "\*magit"))
	     ("ESS" (mode . ess-mode))
         ("LaTeX" (mode . latex-mode))
	     ("Help" (or (name . "\*Help\*")
		             (name . "\*Apropos\*")
		             (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook
	      '(lambda ()
	         (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

;;  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
;;  (global-set-key (kbd "C-x f") 'recentf-ido-find-file) ;; commented out until helm and ido are made to work together

  (when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; dgm comments this out because Shift-Arrows should work in org mode for choosing dates and because instead of windmove I will use ace-window by the great abo-abo.
;; (windmove-default-keybindings) 
;; (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
;; (global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
;; (setq windmove-wrap-around t)

  ;; resizing 'windows' (i.e., inside the frame)
  (global-set-key (kbd "s-M-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "s-M-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "s-M-<down>") 'shrink-window)
  (global-set-key (kbd "s-M-<up>") 'enlarge-window)

   (defun rotate-windows ()
     "Rotate your windows" (interactive) (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
  (t
   (setq i 1)
   (setq numWindows (count-windows))
   (while  (< i numWindows)
     (let* (
            (w1 (elt (window-list) i))
            (w2 (elt (window-list) (+ (% i numWindows) 1)))
            (b1 (window-buffer w1))
            (b2 (window-buffer w2))
            (s1 (window-start w1))
            (s2 (window-start w2))
            )
       (set-window-buffer w1  b2)
       (set-window-buffer w2 b1)
       (set-window-start w1 s2)
       (set-window-start w2 s1)
       (setq i (1+ i)))))))

  (global-set-key (kbd "C-c m") 'rotate-windows)

(global-set-key (kbd "C-x a") 'join-line)

(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

(global-set-key (kbd "C-x m") 'eshell)

(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "s--") 'shell)

 (add-to-list 'display-buffer-alist
              `(,(rx string-start "*shell*" string-end)
               (display-buffer-below-selected)))

;;  (require 'smex)
;;  (smex-initialize)  
;;  (global-set-key (kbd "M-x") 'smex)  ;; I think this is superseded by helm now
;;  (global-set-key (kbd "C-x C-m") 'smex) 
;;  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;  (global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)
  ;; This is your old M-x.
  ;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;  (setq smex-show-unbound-commands t)
;;  (smex-auto-update 30)

;; (global-set-key (kbd "C-x C-m") 'smex)

;;  (setq mac-option-modifier 'meta)

  ;; (global-set-key [(meta z)] 'undo) ;; M-z is for zap to char on my watch
  ;; (require 'redo+) 
  ;;(global-set-key [(alt a)] 'mark-whole-buffer)
  ;;(global-set-key [(alt v)] 'yank)
  ;; (global-set-key [(alt c)] 'kill-ring-save)
  ;;(global-set-key [(alt x)] 'kill-region)
  ;;(global-set-key [(alt s)] 'save-buffer)
  ;;(global-set-key [(alt f)] 'isearch-forward)
  ;;(global-set-key [(alt g)] 'isearch-repeat-forward)
  ;; (global-set-key [(alt z)] 'undo)

(global-set-key (kbd "C-M-x") 'view-url)

(global-set-key (kbd "C-h a") 'apropos)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

  (global-set-key (kbd "C-c j") (lambda () (interactive) (switch-or-start 'jabber-connect "*-jabber-*")))
  (global-set-key (kbd "C-c i") (lambda () (interactive) (switch-or-start (lambda ()
                                                                       (rcirc-connect "irc.freenode.net"))
                                                                     "*irc.freenode.net*")))
  (global-set-key (kbd "C-c J") 'jabber-send-presence)
  (global-set-key (kbd "C-c M-j") 'jabber-disconnect)
;;  (global-set-key (kbd "C-x g") 'magit-status) ;; now in dgm.org

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-dwim)

  (require 'ag)
  (define-key global-map "\C-x\C-a" 'ag) 
  (define-key global-map "\C-x\C-r" 'ag-regexp)

;; new bindings by DGM to try and use 'helm-ag
;;  (define-key global-map "\C-x\C-a" 'helm-ag) 
;;  (define-key global-map "\C-x\C-r" 'helm-ag-regexp)

(use-package winner
  :config
  (winner-mode t)
  :bind (("C-c <down>" . winner-undo)
         ("C-c <up>" . winner-redo)))

;; Old khj's code
;;  (winner-mode 1)
;;  (global-set-key (kbd "C-c <up>") 'winner-undo)
;;  (global-set-key (kbd "C-c <down>") 'winner-redo)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

  ;; I can't remember ever having meant to use C-z to suspend the frame
  (global-set-key (kbd "C-z") 'undo)

  (setq cua-enable-cua-keys nil)
  (global-unset-key (read-kbd-macro "C-<return>"))
  (setq cua-rectangle-mark-key (kbd "C-M-<return>"))
  (cua-mode)

;; To start a rectangle, use [C-return] and extend it using the normal
;; movement keys (up, down, left, right, home, end, C-home,
;; C-end). Once the rectangle has the desired size, you can cut or
;; copy it using C-w and M-w, and you can
;; subsequently insert it - as a rectangle - using C-y.  So
;; the only new command you need to know to work with cua-mode
;; rectangles is C-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;;
;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [C-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with [blanks.
;;  M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].

(use-package expand-region
  :ensure t
  :bind (("C-*" . er/expand-region)
         ("M-*" . er/contract-region)))

  (require 'multiple-cursors)
  ;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  
  ;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  
  ;; Rectangular region mode
  (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
  
  ;; Mark more like this
  (global-set-key (kbd "H-a") 'mc/mark-all-like-this)
  (global-set-key (kbd "H-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "H-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "H-S-n") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "H-S-a") 'mc/mark-all-in-region)

  (set-fringe-mode '(5 . 5))
  (require 'minimal)
  (global-set-key (kbd "C-c s") 'minimal-mode)

(global-set-key (kbd "C-<escape>") 'cua-set-mark)

(provide 'starter-kit-bindings)
;;; starter-kit-bindings.el ends here

  (message "Starter Kit Bindings loaded.")
