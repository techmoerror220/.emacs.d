(server-start)

  (use-package exwm 
    :ensure t
    :config 
  
    ;; necessary to configure exwm manually
    (require 'exwm-config)

    ;; fringe size, most people prefer 1 (uncle dave's setup)
    (fringe-mode 3)

;; dgm comments this as it appears to not be working!! reverts to old (server-star)
;;    (require 'server)
;;      (unless (server-running-p)
;;        (server-start))

    (exwm-config-default))

    ;; this just enables exwm, it started automatically once everything is ready
;; commented out now that I have the Ferguson setup    (exwm-enable))

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

(defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-<left>") #'windmove-left)
(exwm-input-set-key (kbd "s-<down>") #'windmove-down)
(exwm-input-set-key (kbd "s-<up>") #'windmove-up)
(exwm-input-set-key (kbd "s-<right>") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") 'ambrevar/toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right))

(push ?\s-  exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-o") #'ambrevar/toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(add-to-list 'exwm-input-prefix-keys ?\C-q)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\M-h] . [S-end select])
        ([?\M-d] . [C-S-right ?\C-x])
        ([M-backspace] . [C-S-left ?\C-x])
        ;; escape
        ([?\C-g] . [escape])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  (ambrevar/define-keys helm-map
                        "s-\\" 'helm-toggle-resplit-and-swap-windows)
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (when (fboundp 'ambrevar/helm-locate-meta)
    (exwm-input-set-key (kbd "s-F") #'ambrevar/helm-locate-meta))
  (exwm-input-set-key (kbd "s-g") 'ambrevar/helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") 'ambrevar/helm-grep-git-all-or-ag))

(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))

;;; External application shortcuts.
(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
(exwm-input-set-key (kbd "s-r") #'ambrevar/exwm-start)

(when (require 'helm-exwm nil t)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  (ambrevar/define-keys
   helm-exwm-map
   "M-d" 'helm-buffer-run-kill-persistent
   "S-<return>" 'helm-buffer-switch-other-window)
  ;; Launcher
  (exwm-input-set-key (kbd "s-r") 'helm-run-external-command)
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))

(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'ambrevar/switch-to-last-buffer)

(defun ambrevar/exwm-start-lock () (interactive) (start-process "slock" nil "slock"))
(exwm-input-set-key (kbd "s-z") #'ambrevar/exwm-start-lock)

(defun ambrevar/exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'ambrevar/exwm-start-screenshot)

(use-package sudo-edit
  :ensure t
  :bind
    ("s-e" . sudo-edit))

(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

(defun ambrevar/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)

(setq exwm-workspace-number 1
      exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

(dolist (k '(("<XF86AudioLowerVolume>"
              "amixer sset Master 5%-")
             ("<XF86AudioRaiseVolume>"
              "amixer set Master unmute; amixer sset Master 5%+")))
  (let ((f (lambda () (interactive)
             (save-window-excursion
               (start-process-shell-command (cadr k) nil (cadr k))))))
    (exwm-input-set-key (kbd (car k)) f)))

(defun exwm-async-run (name)
  (interactive)
  (start-process name nil name))

(defun daedreth/launch-browser ()
  (interactive)
  (exwm-async-run "chromium"))

(defun daedreth/lock-screen ()
  (interactive)
  (exwm-async-run "slock"))

(global-set-key (kbd "<s-escape>") 'daedreth/launch-browser)
(global-set-key (kbd "<s-@>") 'daedreth/lock-screen)

(provide 'starter-kit-exwm)

(message "Starter Kit User EXWM File loaded.")
