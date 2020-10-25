;; dgm comments this as it appears to not be working!! reverts to old (server-star)
;;    (require 'server)
;;      (unless (server-running-p)
;;        (server-start))

    ;; this just enables exwm, it started automatically once everything is ready
;; commented out now that I have the Ferguson setup    (exwm-enable))

;;;;; multiple screens when working on the laptop
;; From: https://github.com/ch11ng/exwm/wiki#randr-multi-screen

;; (exwm-input-set-key (kbd "s-<left>") #'windmove-left)
;; (exwm-input-set-key (kbd "s-<down>") #'windmove-down)
;; (exwm-input-set-key (kbd "s-<up>") #'windmove-up)
;; (exwm-input-set-key (kbd "s-<right>") #'windmove-right)
;; (exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
;; (global-set-key (kbd "C-\{") 'daedreth/launch-browser)

;; Technomancy config for Audio. Never worked
    (dolist (k '(("<XF86AudioLowerVolume>"
                  "amixer sset Master 5%-")
                 ("<XF86AudioRaiseVolume>"
                  "amixer set Master unmute; amixer sset Master 5%+")))
      (let ((f (lambda () (interactive)
                 (save-window-excursion
                   (start-process-shell-command (cadr k) nil (cadr k))))))
        (exwm-input-set-key (kbd (car k)) f)))
