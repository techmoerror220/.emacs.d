;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; REVIEW: Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; REVIEW: Pressing "s-a" ('emms-smart-browse) loses the cursor.
;;; Sometimes waiting helps.  Calling emms-smart-browse manually does not trigger the issue.
;;; https://github.com/ch11ng/exwm/issues/366

;;; REVIEW: helm-mini with follow-mode hangs when using EXWM.
;;; https://github.com/emacs-helm/helm/issues/1889

;;; Rename buffer to window title.
(defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") 'ambrevar/toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right))

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
;;(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-o") #'ambrevar/toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

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

(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'ambrevar/switch-to-last-buffer)
;; (when (require 'evil nil t)
;;   (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
;;   (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
;; (exwm-input-set-key (kbd "s-t") #'ambrevar/org-switch-agenda-file)
;; (exwm-input-set-key (kbd "s-T") #'ambrevar/org-switch-agenda-file-other-window)
;; (exwm-input-set-key (kbd "s-<return>") #'ambrevar/eshell-or-new-session)
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
;; (when (fboundp 'emms-all)
;;   (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
;;   (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
;;   (if (fboundp 'helm-emms)
;;       (exwm-input-set-key (kbd "s-A") #'helm-emms)
;;     (exwm-input-set-key (kbd "s-A") #'emms)))
;; (when (or (fboundp 'mu4e)
;;           (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path)))
;;   (exwm-input-set-key (kbd "s-m") #'ambrevar/mu4e-headers))
;; (exwm-input-set-key (kbd "s-n") #'ambrevar/elfeed-switch-back) ; "n" for "news"
;; (exwm-input-set-key (kbd "s-e") #'ambrevar/eww-switch-back)
;; (exwm-input-set-key (kbd "s-E") #'eww)

;; (when (fboundp 'helm-pass)
;;   (defun ambrevar/helm-pass-for-page ()
;;     "Default prompt to current exwm-title"
;;     (interactive)
;;     (require 'helm-pass)
;;     (helm :sources 'helm-pass-source-pass
;;           :input (cond
;;                   ((derived-mode-p 'eww-mode)
;;                    (let* ((url (replace-regexp-in-string ".*//\\([^/]*\\).*" "\\1" (eww-current-url)))
;;                           (domain (split-string url "\\.")))
;;                      (concat (nth (- (length domain) 2) domain) "." (nth (1- (length domain)) domain))))
;;                   ((and (derived-mode-p 'exwm-mode) exwm-title)
;;                    (let* ((title exwm-title) url domain)
;;                      (with-temp-buffer
;;                        (insert title)
;;                        (goto-char (point-min))
;;                        (while (and (not (eobp))
;;                                    (not (thing-at-point-url-at-point)))
;;                          (forward-word))
;;                        (setq url (thing-at-point-url-at-point)))
;;                      (if (not url)
;;                          ""
;;                        (if (not (string-match (rx "//"
;;                                                   (group
;;                                                    (* (not (any "/")))
;;                                                    "."
;;                                                    (* (not (any "." "/"))))
;;                                                   "/")
;;                                               url))
;;                            ""
;;                          (setq domain (split-string (match-string 1 url) "\\."))
;;                          (concat (nth (- (length domain) 2) domain) "." (nth (1- (length domain)) domain)))))))
;;           :buffer "*helm-pass*"))
;;   (exwm-input-set-key (kbd "s-p") #'ambrevar/helm-pass-for-page))

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

;;; Lock screen
(defun ambrevar/exwm-start-lock () (interactive) (start-process "slock" nil "slock"))
(exwm-input-set-key (kbd "s-z") #'ambrevar/exwm-start-lock)

;;; Screenshot
(defun ambrevar/exwm-start-screenshot () (interactive) (start-process-shell-command "scrot" nil "scrot ~/temp/screen-%F-%T.png"))
(exwm-input-set-key (kbd "<print>") #'ambrevar/exwm-start-screenshot)

;; ;;; Volume control
;; (when (require 'pulseaudio-control nil t)
;;   (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
;;   (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
;;   (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
;;   (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
;;   (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
;;   (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun ambrevar/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Costumization from Technomancy's wm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq exwm-workspace-number 9
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; http://p.hagelb.org/exwm-ff-tabs
  ;; Using ido to change "tabs" in Firefox!
  ;;
  ;; For this to work properly you need to stop opening new tabs and open
  ;; everything in new windows. It sounds crazy, but then you can use ido
  ;; to switch between "tabs" and everything is wonderful.
  ;;
  ;; Step 1: about:config -> browser.tabs.opentabfor.middleclick -> false
  ;; Step 2: change whatever "open link in new tab" binding in Saka Key or
  ;;         whatever you use to open the link in a new window
  ;; Step 3: rebind ctrl-t to open a new window as well
  ;; Step 4: place the following in chrome/userChrome.css in your FF profile:
  ;;         #tabbrowser-tabs { visibility: collapse !important; }
  ;; Step 5: add this code to your exwm config:
  ;; Step 6: restart your browser and enjoy your new C-x b fanciness!
;;   (defun pnh-trim-non-ff ()
;;     (delete-if-not (apply-partially 'string-match "- Mozilla Firefox$")
;;                    ido-temp-list))

;;   (add-hook 'exwm-manage-finish-hook
;;             (defun pnh-exwm-manage-hook ()
;;               (when (string-match "Firefox" exwm-class-name)
;;                 (exwm-workspace-move-window 3)
;;                 (exwm-layout-hide-mode-line)
;;                 (setq ido-make-buffer-list-hook 'pnh-trim-non-ff))
;;               (when (string-match "Chromium" exwm-class-name)
;;                 (exwm-workspace-move-window 1)
;;                 (exwm-layout-hide-mode-line))))

;;   (add-hook 'exwm-update-title-hook
;;             (defun pnh-exwm-title-hook ()
;;               (when (string-match "Firefox" exwm-class-name)
;;                 (exwm-workspace-rename-buffer exwm-title))))

;;   (setq browse-url-firefox-arguments '("-new-window"))

;; (add-hook 'exwm-manage-finish-hook
;;           (defun pnh-exwm-manage-hook ()
;;             (when (or (string= exwm-class-name "URxvt")
;;                       (string= exwm-class-name "love"))
;;               (exwm-input-release-keyboard))
;;             (when (string-match "Chromium" exwm-class-name)
;;               (exwm-layout-hide-mode-line))
;;             (when (string-match "Firefox" exwm-class-name)
;;               (setq ido-make-buffer-list-hook 'pnh-trim-non-ff)
;;               (exwm-layout-hide-mode-line))))

;; (exwm-enable-ido-workaround)

  (dolist (k '(("<XF86AudioLowerVolume>"
                "amixer sset Master 5%-")
               ("<XF86AudioRaiseVolume>"
                "amixer set Master unmute; amixer sset Master 5%+")))
    (let ((f (lambda () (interactive)
               (save-window-excursion
                 (start-process-shell-command (cadr k) nil (cadr k))))))
      (exwm-input-set-key (kbd (car k)) f)))

;; from https://github.com/daedreth/UncleDavesEmacs
;; but it does not work either

    ;; (dolist (k '(XF86AudioLowerVolume
    ;;            XF86AudioRaiseVolume))
    ;; (cl-pushnew k exwm-input-prefix-keys))


    ;; From Damien Cassou
    ;; Bind C-q so that the next key is sent literally to the
    ;; application
    (add-to-list 'exwm-input-prefix-keys ?\C-q)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; (exwm-input-set-simulation-keys
;;    (mapcar (lambda (c) (cons (kbd (car c)) (cdr c)))
;; ;;           `(("C-b" . left)
;;             '(("C-b" . left)
;;              ("C-f" . right)
;;              ("C-p" . up)
;;              ("C-n" . down)
;;              ("C-a" . home)
;;              ("C-e" . end)
;;              ("M-v" . prior)
;;              ("C-v" . next)
;;              ("C-d" . delete)
;;              ("C-m" . return)
;;              ("C-i" . tab)
;;              ("C-g" . escape)
;; ;;             ("C-s" . ?\C-f)  ;; with ? in the original, Technomancy code, but it doesn't work for me.
;;              ("C-s" . \C-f)
;;              ("C-y" . ?\C-v)
;;              ("M-w" . ?\C-c)
;;              ("M-<" . C-home)
;;              ("M->" . C-end)
;;              ("C-t" . C-n)
;;              ("C-M-h" . C-backspace)
;;              ("C-k" . (S-end delete))  ;; from https://github.com/daedreth/UncleDavesEmacs
;;              ("M-h" . (S-end select)))))    ;; toma! este me lo he inventado yo copiando el anterior.
;; Note <M-> only shows Firefox's menu in line mode
;; C-l takes you to the address bar
;; C-j takes you to the focus search bar

;; new way from
;; https://github.com/ch11ng/exwm/wiki/Configuration-Example
;; This one works!!!

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
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ([?\M-h] . [S-end select])
        ([?\M-d] . [C-S-right ?\C-x])
        ([M-backspace] . [C-S-left ?\C-x])
        ;; escape
        ([?\C-g] . [escape])
        ;; search
        ([?\C-s] . [?\C-f])))

  ;; (global-set-key (kbd "C-x m")
  ;;                 (defun pnh-eshell-per-workspace (n)
  ;;                   (interactive "p")
  ;;                   (eshell (+ (case n (4 10) (16 20) (64 30) (t 0))
  ;;                              exwm-workspace-current-index))))

(provide 'init-exwm)
