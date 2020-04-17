;;; laurence-mono-theme.el --- A monochrome theme for Emacs, by Laurence
;;; Commentary:
;;; Code:
(deftheme laurence-mono
  "Laurence's custom monochrome theme.")

(let ((background "#141414")
      (standard-text "snow")
      (dark-grey "#616161")
      (medium-grey "#8e8e8e")
      (light-grey "#c5c5c5"))
  (custom-theme-set-faces
   'laurence-mono
   ;; Defaults
   `(default ((t (:background ,background :foreground ,standard-text :family "Input" :height 120 :width normal))))
   '(cursor ((t (:background  "lawn green"))))
   `(fringe ((t (:background ,dark-grey))))
   `(region ((t (:background ,light-grey))))
   '(minibuffer-prompt ((t (:weight bold))))
   `(link ((t (:foreground ,light-grey :underline t))))
   ;; prog-mode
   '(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-type-face ((t (:foreground ,light-grey :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,medium-grey))))
   `(font-lock-function-name-face ((t (:slant italic))))
   `(font-lock-constant-face ((t (:foreground ,medium-grey :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,dark-grey))))
   `(font-lock-string-face ((t (:foreground ,light-grey))))
   `(font-lock-preprocessor-face ((t (:slant italic))))
   ;; org-mode
   `(org-level-1 ((t (:height 1.8 :weight bold))))
   `(org-level-2 ((t (:height 1.4))))
   `(org-level-3 ((t (:height 1.2))))
   `(org-level-4 ((t (:height 1.1))))
   `(org-level-5 ((t (:height 1.1))))
   `(org-level-6 ((t (:height 1.0))))
   '(org-todo ((t (:foreground "red"))))
   '(org-done ((t (:foreground "green" :strike-through t))))
   '(org-checkbox-statistics-done ((t (:foreground "green" :strike-through nil))))
   `(org-checkbox ((t (:box (:line-width -1 :style released-button :color ,dark-grey) :background ,dark-grey))))
   ;; ERC
   `(erc-input-face ((t (:inherit default))))))

(provide-theme 'laurence-mono)
;;; laurence-mono-theme.el ends here