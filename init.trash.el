;; Technomancy config for Audio. Never worked
    (dolist (k '(("<XF86AudioLowerVolume>"
                  "amixer sset Master 5%-")
                 ("<XF86AudioRaiseVolume>"
                  "amixer set Master unmute; amixer sset Master 5%+")))
      (let ((f (lambda () (interactive)
                 (save-window-excursion
                   (start-process-shell-command (cadr k) nil (cadr k))))))
        (exwm-input-set-key (kbd (car k)) f)))
