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

(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
;; (exwm-input-set-key (kbd "s-r") #'ambrevar/exwm-start)

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

;; (require 'functions) ;; this is the first thing loaded in the whole process so no need to use it here again
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

;;(global-set-key (kbd "<s-escape>") 'daedreth/launch-browser) ;; commented out as <s-w> also lunches the browser and <s-W> does so in other window
(global-set-key (kbd "<s-@>") 'daedreth/lock-screen)

(provide 'starter-kit-exwm)

(message "Starter Kit User EXWM File loaded.")
