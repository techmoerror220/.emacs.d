;;; https://gist.github.com/tujion
;;; I used to modify paredit.el directly to make it work with latex-mode.
;;; Then I learned about advice-add. Yeah! So now I can hack from the outside.
;;; Essentially we deal with the maths thingy $...$ and leave the rest to paredit.
;;; Note the syntax table for a buffer in latex-mode is different from that for a buffer in lisp-mode.
;;; For example, in latex-mode, "..." is not considered to be a string.
;;; The hack is not complete. For example we haven't written advice for paredit-kill etc. so that they respect $...$ as well.
;;; Current hack did cover most of MY use case.

(defun paredit-backward-delete-advice (fun &optional argument)
  "Deal with deletion of $ math and leave the rest to original function. We are ignoring argument and we don't deal with $$...$$."
  (if (not (string= major-mode "latex-mode"))
      (apply fun argument)
    (let ((syn (char-syntax (char-before))))
      (cond ((eq syn ?\$)
         (if (texmathp)
         (if (eq (char-syntax (char-after)) ?\$)
             (progn (delete-char +1) (delete-char -1))
           (backward-char))
           (if (eq (char-syntax (char-before (1- (point)))) ?\$)
           (backward-delete-char 2)
         (backward-char))))
        (t (apply fun argument))))))

(defun paredit-forward-delete-advice (fun &optional argument)
  "Deal with deletion of $ math and leave the rest to original function. We are ignoring argument and we don't deal with $$...$$.."
  (if (not (string= major-mode "latex-mode"))
      (apply fun argument)
    (let ((syn (char-syntax (char-after))))
      (cond ((eq syn ?\$)
         (if (texmathp)
         (if (eq (char-syntax (char-before)) ?\$)
             (progn (delete-char +1) (delete-char -1))
           (forward-char))
           (if (eq (char-syntax (char-after (1+ (point)))) ?\$)
           (forward-delete-char 2)
         (forward-char))))
        (t (apply fun argument))))))

(defun paredit-dollar (&optional n)
  ;; relies on the texmathp texmathp-why from auctex
  "If inside math caused by dollars then move outside math. If region active, wrap region in dollars. If no region active, insert dollars and put point between them."
  (interactive "P")
  (let ((mathp (texmathp)) (why (car texmathp-why)))
    (cond ((region-active-p) (paredit-insert-pair n ?$ ?$ 'goto-char))
      ((paredit-in-string-escape-p) (insert ?$))
      ((and  mathp (or (string= why "$")  (string= why "$$"))) ;inside $...$ or $$...$$. I personally will not use $$...$$
       ;;move past $. If there is no $ ahead, we have unbalanced thing, so insert closing $.
       (if (not (search-forward why nil t)) (insert why)))
      (t (paredit-insert-pair n ?$ ?$ 'goto-char)))))

;;; I also defined a nonsmart version of doublequote which does not escape things, so that I can work with tikz more easily.
(add-hook 'LaTeX-mode-hook '(lambda ()
                  (paredit-mode)
                  (auto-complete-mode)
                  (visual-line-mode)
                  (advice-add 'paredit-backward-delete :around #'paredit-backward-delete-advice)
                  (advice-add 'paredit-forward-delete :around #'paredit-forward-delete-advice)
                  (advice-add 'paredit-doublequote :override #'paredit-nonsmart-doublequote)))

;; Our idea is simple
;; if region active, always wrap.
;; if in escape mode, insert literally.
;; if in environment, e.g., math or string, then jump out in the forward direction.
;; if the environment is somehow missing closing delimiter, insert one at point.
;; if not in environment, insert pair and place point in between.
(defun paredit-nonsmart-doublequote (&optional n)
  "Insert or wrap region by doublequote pair"
  (interactive "P")
  (cond ((region-active-p) (paredit-insert-pair n ?\" ?\" 'goto-char))
    ((paredit-in-string-escape-p) (insert ?\"))
    ((paredit-tmp-syntax-in-string-p) (if (not (search-forward "\"" nil t)) (insert ?\")))
    (t (paredit-insert-pair n ?\" ?\" 'goto-char))))


(defun paredit-tmp-syntax-in-string-p (&optional state)
  "Make a temp syntax table that treats doublequotes as string delimiters and then test if we are in a string."
  (let ((tmp-syntax-table (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?\" "\"" tmp-syntax-table)
    (with-syntax-table tmp-syntax-table
      (and (nth 3 (or state (paredit-current-parse-state)))
       t))))


;;; The LOL part. I also rebind "\" to self-insert-command.
;;; Yes, I just hate paredit for taking away my control over "\".
(defun paredit-in-char-p-advice (&optional position)
  "Always nil. I don't want paredit to manage backslash."
  nil)
(advice-add 'paredit-in-char-p :override #'paredit-in-char-p-advice)


;;; forward-slurp-sexp.el
(defun paredit-forward-slurp-sexp-advice (fun &optional argument)
  "forward slurp $"
  (if (not (string= major-mode "latex-mode"))
      (apply fun argument)
    (save-excursion
      (let ((mathp (texmathp)) (why (car texmathp-why)))
        (if (and mathp (or (string= why "$") (string= why "$$"))
                 (= (car (parse-partial-sexp (cdr texmathp-why) (point))) 0))
            ;; inside $...$ and at top level inside maths
            (progn (search-forward why)
                   (backward-delete-char (length why))
                   (forward-sexp (or argument 1))
                   (insert why))
          ;; otherwise delegate
          (apply fun argument))))))

(provide 'latex-paredit-hack)
