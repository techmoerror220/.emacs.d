(add-to-list 'load-path "/home/dgm/.emacs.d/elpa/ess-20210104.809/")

(require 'ess-site)
(require 'ess-r-mode)

;; (setq ess-local-process-name "R")

;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;     (progn
;;       (delete-other-windows)
;;       (setq w1 (selected-window))
;;       (setq w1name (buffer-name))
;;       (setq w2 (split-window w1))
;;       (R)
;;       (set-window-buffer w2 "*R*")
;;       (set-window-buffer w1 w1name))))

;; (defun my-ess-eval ()
;;   (interactive)
;;   (my-ess-start-R)
;;   (if (and transient-mark-mode mark-active)
;;       (call-interactively 'ess-eval-region)
;;     (call-interactively 'ess-eval-line-and-step)))

;; (add-hook 'ess-mode-hook
;;       '(lambda()
;;          (local-set-key [(shift return)] 'my-ess-eval)))

;; (add-hook 'inferior-ess-mode-hook
;;       '(lambda()
;;          (local-set-key [C-up] 'comint-previous-input)
;;          (local-set-key [C-down] 'comint-next-input)))

;; (setq display-buffer-alist
;;       `((,(rx bos "*R*")
;;          (display-buffer-reuse-window display-buffer-at-bottom)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))))

(setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil))))
