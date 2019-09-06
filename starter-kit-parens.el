(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-d" . delete-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ;; ("C-c '"  . wrap-with-single-quotes)  ;; messes around with this binding needed by =org-edit-src-exit=
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))

 (require 'smartparens-text)
 (require 'smartparens-org)
 (require 'smartparens-ess)
 (require 'smartparens-markdown)
 (require 'smartparens-racket)
 (require 'smartparens-latex)

(sp-with-modes 'org-mode
  (sp-local-pair "~" "~" :bind "s-~")
  (sp-local-pair "=" "=" :bind "s-="))

(sp-local-pair 'org-mode-hook "=" "=")
  ;; org-mode. This is not working though...
  ;;    (sp-local-pair 'org-mode "~" "~")
  ;; (sp-local-pair 'org-mode-hook "=" "=") ; =select= region, hit = then region -> =region= in org-mode
  ;;    (sp-local-pair 'org-mode "*" "*") ; select region, hit * then region -> *region* in org-mode
  ;;    (sp-local-pair 'org-mode "/" "/") ; select region, hit / then region -> /region/ in org-mode
  ;;    (sp-local-pair 'org-mode "_" "_") ; select region, hit _ then region -> _region_ in org-mode
  ;;    (sp-local-pair 'org-mode "+" "+") ; select region, hit + then region -> +region+ in org-mode
  ;;    (sp-local-pair 'org-mode "$" "$") ; 
  ;;    ;; (sp-local-pair 'org-mode "`" "'") ; not working, as it waits for second `
  ;;   (sp-local-pair 'org-mode "``" "''")

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

(defvar org-electric-pairs '(
                             (?/ . ?/) 
                             (?= . ?=) 
                             (?~ . ?~)
                             ) 
  "Electric pairs for Org-mode.")

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

(provide 'starter-kit-parens)

(message "Starter Kit Parens File loaded.")
