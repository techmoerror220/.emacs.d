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

(exwm-input-set-key (kbd "s-<left>") #'windmove-left)
(exwm-input-set-key (kbd "s-<down>") #'windmove-down)
(exwm-input-set-key (kbd "s-<up>") #'windmove-up)
(exwm-input-set-key (kbd "s-<right>") #'windmove-right)
(exwm-input-set-key (kbd "s-r") #'exwm-reset) 
;; (exwm-input-set-key (kbd "s-b") #'eww) ;; commented out as I now use starter-kit-search-engine-with-eww.org
;; (exwm-input-set-key (kbd "s-D") #'kill-this-buffer) I don't need a shortcut for this
;; (exwm-input-set-key (kbd "s-b") #'list-buffers)   "s-b" mapped to helm-mini in starter-kit-helm.org
;;(exwm-input-set-key (kbd "s-f") #'find-file)       "s-f" mapped to helm-find-files in starter-kit-helm.org 

(when (require 'functions)
  (exwm-input-set-key (kbd "C-$") 'ambrevar/toggle-window-split))
;; DGM comments out as never used
  ;;(exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
  ;;(exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
  ;;(exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
  ;;(exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right))

(push ?\s-  exwm-input-prefix-keys)
;; (exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split) ;; no lo veo claro 
;; (exwm-input-set-key (kbd "s-o") #'ambrevar/toggle-single-window) ;; not working... =s-o= not recognized, don't know why. 
;; DGM comments out as never used: 
;; (exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)

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

(use-package sudo-edit
  :ensure t
  :bind
    ("s-\#" . sudo-edit))

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

;;(defun daedreth/mu4e ()
;;  (interactive)
;;  (exwm-async-run "mu"))

(global-set-key (kbd "C-\{") 'daedreth/launch-browser) ;; commented out as <s-w> also launches the browser and <s-W> does so in other window
(global-set-key (kbd "s-_") 'daedreth/lock-screen)

(use-package exwm-edit)

;; Redshift off
(exwm-input-set-key (kbd "C-\)")
(lambda () (interactive) (start-process "" nil "redshift" "-x")))

;; Redshift on
(exwm-input-set-key (kbd "C-\(")
                    (lambda () (interactive) (start-process "" nil "redshift" "-O" "3500")))

(provide 'starter-kit-exwm)

(message "Starter Kit User EXWM File loaded.")
