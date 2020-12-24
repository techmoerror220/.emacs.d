     ;; (defun exwm-change-screen-hook ()
     ;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
     ;;          default-output)
     ;;     (with-temp-buffer
     ;;       (call-process "xrandr" nil t nil)
     ;;       (goto-char (point-min))
     ;;       (re-search-forward xrandr-output-regexp nil 'noerror)
     ;;       (setq default-output (match-string 1))
     ;;       (forward-line)
     ;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
     ;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
     ;;         (call-process
     ;;          "xrandr" nil nil nil
     ;;          "--output" (match-string 1) "--primary" "--auto"
     ;;          "--output" default-output "--off")
     ;;         (setq exwm-randr-workspace-output-plist (list 0 (match-string
     ;;   1)))))))
;; 3 "HDMI-2" is my monitor in Malaga
  ;; (when (string=(system-name) "toshiba")
  ;;   (require 'exwm-randr)
  ;;   ;;(setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  ;;   ;; (setq exwm-randr-workspace-output-plist '(0 "HDMI-2"))
  ;;   (setq exwm-randr-workspace-monitor-plist
  ;;         '(0 "HDMI-2" 1  "HDMI-2" 2 "HDMI-2" 3 "HDMI-2" 4 "HDMI-2"
  ;;             5 "HDMI-2" 6 "HDMI-2" 7 "HDMI-2" 8 "HDMI-2" 9 "HDMI-2"))
  ;;   (add-hook 'exwm-randr-screen-change-hook
  ;;             (lambda ()
  ;;               (start-process-shell-command
  ;;                ;; "xrandr" nil "xrandr --output HDMI-2 --left-of LVDS1 --auto")))
  ;;                "xrandr" nil "xrandr --output eDP1-1 --off --output HDMI-2 --auto")))
  ;;   (exwm-randr-enable)
