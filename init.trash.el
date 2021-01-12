 ;; (exwm-input-set-key (kbd "s-!") #'daedreth/launch-browser)

 ;; ;; Redshift off
 ;; (exwm-input-set-key (kbd "s-\)")
 ;;                     (lambda () (interactive) (efs/kill-redshift)))

 ;; ;; Redshift on
 ;; (exwm-input-set-key (kbd "s-\(")
 ;;                     (lambda () (interactive) (efs/start-redshift)))


;; on Why Pragmata Pro doesn't work, read here: https://github.com/hiavi/pragmatapro/issues/9
;; Set default font. First one found is selected.
;; (cond
;;  ((eq window-system nil) nil)
;; ((font-existsp "Pragmata Pro Mono")
;;  (set-face-attribute 'default nil :height 156 :font "Pragmata Pro Mono")))

;;  ((font-existsp "FiraCode")
;;   (set-face-attribute 'default nil :height 121 :font "FiraCode"))
;;  ((font-existsp "Monoid")
;;   (set-face-attribute 'default nil :height 121 :font "Monoid"))
;;  ((font-existsp "Inconsolata")
;;  (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
;; ((font-existsp "Input Mono Compressed")
;;  (set-face-attribute 'default nil :height 131 :font "Input Mono Compressed"))
;; ((font-existsp "Menlo")
;;  (set-face-attribute 'default nil :height 121 :font "Menlo"))
;;  ((font-existsp "Consolas")
;;  (set-face-attribute 'default nil :height 121 :font "Consolas"))
;; ((font-existsp "Monaco")
;;  (set-face-attribute 'default nil :height 121 :font "Monaco"))
;; ((font-existsp "Envy Code R")
;;   (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
;; ((font-existsp "Source Code Pro")
;;  (set-face-attribute 'default nil :height 121 :font "Source Code Pro")))

;; on Why Pragmata Pro doesn't work, read here: https://github.com/hiavi/pragmatapro/issues/9
;; Set default font. First one found is selected.
;; (cond
;;  ((eq window-system nil) nil)
;; ((font-existsp "Pragmata Pro Mono")
;;  (set-face-attribute 'default nil :height 156 :font "Pragmata Pro Mono")))

;;  ((font-existsp "FiraCode")
;;   (set-face-attribute 'default nil :height 121 :font "FiraCode"))
;;  ((font-existsp "Monoid")
;;   (set-face-attribute 'default nil :height 121 :font "Monoid"))
;;  ((font-existsp "Inconsolata")
;;  (set-face-attribute 'default nil :height 121 :font "Inconsolata"))
;; ((font-existsp "Input Mono Compressed")
;;  (set-face-attribute 'default nil :height 131 :font "Input Mono Compressed"))
;; ((font-existsp "Menlo")
;;  (set-face-attribute 'default nil :height 121 :font "Menlo"))
;;  ((font-existsp "Consolas")
;;  (set-face-attribute 'default nil :height 121 :font "Consolas"))
;; ((font-existsp "Monaco")
;;  (set-face-attribute 'default nil :height 121 :font "Monaco"))
;; ((font-existsp "Envy Code R")
;;   (set-face-attribute 'default nil :height 121 :font "Envy Code R"))
;; ((font-existsp "Source Code Pro")
;;  (set-face-attribute 'default nil :height 121 :font "Source Code Pro")))


;; Line-spacing tweak
;; Set this to a different number depending on taste and the fonr
;; selected. The value can be a integer or decimal number.
;; if integer: it means pixels, added below each line.
;; if float (e.g 0.02): a scaling factor relative to current window's default line height.
;; if nil: add no extra spacing.

;; (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 1920x312 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal")


 ;; eDP1-1
 ;; eDP-1
 ;; xrandr --output HDMI-2 --auto
 ;; xrandr --output eDP-1 --off --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 1366x0 --rotate normal

 ;; (when (string=(system-name) "toshiba")
 ;;   (setq exwm-randr-workspace-monitor-plist
 ;;         '(0 "HDMI-2" 1  "HDMI-2" 2 "HDMI-2" 3 "HDMI-2" 4 "HDMI-2"
 ;;             5 "HDMI-2" 6 "HDMI-2" 7 "HDMI-2" 8 "HDMI-2" 9 "HDMI-2"))
 ;;   (add-hook 'exwm-randr-screen-change-hook
 ;;             (lambda ()
 ;;               (start-process-shell-command
 ;;                "xrandr" nil "xrandr --output HDMI-2 --auto")))
 ;;   (exwm-randr-enable)
 ;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;; NOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; (when (string=(system-name) "toshiba")
 ;;   (exwm-randr-enable)
 ;;   (setq exwm-randr-workspace-monitor-plist
 ;;         '(0 "eDP-1"))
 ;;   (add-hook 'exwm-randr-screen-change-hook
 ;;             (lambda ()
 ;;               (start-process-shell-command
 ;;                "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 1920x312 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal"))))

;; From dual.sh
;; xrandr --output eDP-1 --mode 1366x768 --pos 0x0 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 1366x0 --rotate normal

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
