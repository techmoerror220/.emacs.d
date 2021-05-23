;; -*- coding: utf-8 -*-

;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.

;;,-----------------------------
;;| Technomancy's recommendation
;;`-----------------------------

;; This avoids many
;; [common](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)
;; [problems](https://github.com/melpa/melpa/issues/2342) with `package.el`, and
;; it allows you to seamlessly roll back upgrades which introduce breaking
;; changes as well as guaranteeing that all machines you work on share the same
;; versions.
;; in:
;; /media/dgm/blue/documents/programming/emacs/CoolEmacsGuys/technomancy/emacs-starter-kit/README.markdown
;; where it says "ini.el", it originally says "my-autoload.el".

;; (defun pnh-reinit-libs ()
;;  (interactive)
;;  (let ((generated-autoload-file (concat user-emacs-directory "init.el")))
;;    (dolist (d (directory-files (concat user-emacs-directory "lib") t "^[^\.]"))
;;      (dolist (f (directory-files d t "\\.el$"))
;;        (byte-compile-file f))
;;      (update-directory-autoloads d))))

;; (dolist (l (directory-files (concat user-emacs-directory "lib") nil "^[^\.]"))
;;  (add-to-list 'load-path (concat user-emacs-directory "lib/" l))
;;   (autoload (internl) (concat l ".el")))

;; (when (not (file-exists-p (concat user-emacs-directory "init.el")))
;;  (pnh-reinit-libs))

;; (load (concat user-emacs-directory "init.el"))


;; DGM: I've commented out because I've created the "lib" directory BUT I don't know what is meant to be in the
;; "my-autoload.el" file

;; But this produces this:

;; Warning (initialization): An error occurred while loading  ‘/home/dgm/.emacs.d/init.el’:

;; error: Recursive load, /home/dgm/.emacs.d/init.el, /home/dgm/.emacs.d/init.el, /home/dgm/.emacs.d/init.el, /home/dgm/.emacs.d/init.el, /home/dgm/.emacs.d/init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; John Wiegley's setup tweaked
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184  ;; 402.653.184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;;; original bit by JW (though with =gc-cons-percentage= on)
;;; Alphapapa suggests 10 or 100MB (100000000) at most.
;;; Another use says 300\MBOX{}
;;; Originally, I found that 3000000 (3 MB) was okay
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold (* 2 1000 1000))  ;; originally 10MB or 10000000 or 10.000.000
             ;; originally. This setting is by DW
             ;;gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; DW: Now set the garbage collection threshold lower. 2MB
;; (setq gc-cons-threshold (* 5 1000 1000))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; However, I comment it out because with emacs 27 we get a message
;; saying this is not needed any more.
;; (package-initialize)

;; (require 'cl) ;; deprecated
(require 'cl-lib)
(require 'ffap)
(require 'ansi-color)


;; Native compilation
(when (string= (system-name) "lenovo")
  (setq package-native-compile t))

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; Use 6 of the 8 cores for asynchronous compilation
(setq comp-async-jobs-number 6)

;; Compile everything under these directories using 6 cores
;; (native-compile-async "~/.emacs.d/" 6 t)
;; (native-compile-async "~/.emacs.d/elpa/" 6 t)

;;; DGM on 5th August 2019 trying to fix <(file-error
;;; "https://elpa.gnu.org/packages/archive-contents" "Bad Request")>
;;; Tip from:
;;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
;;; Apparently it is a bug in emacs
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;;; Tip 1 from Ambrevar's init.el
;; I guess responsiveness is improved by a low gc-cons-threshold, but
;; speed is improved by a high gc-cons-threshold.
;; from: https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/

;;; Speed up init.
;;Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
;; (defun ambrevar/reset-gc-cons-threshold ()
;;   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
;; ;; 511 (setq gc-cons-threshold (* 64 1024 1024))
;; ;;(setq gc-cons-threshold (* 64 1024 1024))
;; (setq gc-cons-threshold 800000)
;; (add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;; ;;Temporarily disable the file name handler.
;; (setq default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (defun ambrevar/reset-file-name-handler-alist ()
;;   (setq file-name-handler-alist
;;         (append default-file-name-handler-alist
;;                 file-name-handler-alist))
;;   (cl-delete-duplicates file-name-handler-alist :test 'equal))
;; (add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;; (setq gc-cons-percentage 0.1)
;; ;(run-with-idle-timer 5 t #'garbage-collect)
;; (setq garbage-collection-messages t)

;;; Tip 2 from Ambrevar's init.el

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
;; (setq package-user-dir (concat dotfiles-dir "elpa")) ;; if enabled
;; this with exwm, then it blocks emacs

;; original config about custom-file
;; (setq custom-file (concat dotfiles-dir "custom.el"))
;; (add-to-list 'load-path (expand-file-name
;;                          "lisp" (expand-file-name
;;                                  "org" (expand-file-name
;;                                         "src" dotfiles-dir))))

;; daviwil's alternative. Apparently, he got it from Ambrevar too!
;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;From https://github.com/danielmai/.emacs.d/blob/master/init.el


;;; Set up package. Originally in =starter-kit.org= but when I
;;; downloaded everything anew from the github repository =use-package=
;;; didn't work until I moved this here.
;;; DGM on 2 Oct 2019: changing all http's to https as per: https://www.reddit.com/r/emacs/comments/aug9in/failed_to_verify_signature_archivecontentssig/

;;; DGM on 6 nov 2019: added this line to be able to load emacs or else
;;; I had a signature fail message. I have to solve this
(setq package-check-signature nil)

;; We require the <package.el> package to bring in the environment all the package management functions
(require 'package)

(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; From DW. In the very first load, check if there is a package archive cloned
;; in your computer. Unless the archive exists, refresh the package list so that
;; future calls to packages functions know which packages you're referring to.
(unless package-archive-contents
  (package-refresh-contents))

;; Initialise the packages, avoiding a re-initialisation?
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))

;; Emacs 27: Warning (package): Unnecessary call to ‘package-initialize’ in init
;; file. Typically this initializes the package system and preprares it to be
;; used.
;; (package-initialize)


;;; Add support to package.el for pre-filtering available packages
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")


(defadvice package--add-to-archive-contents
    (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            (funcall package-filter-function
                     (car package)
                     (funcall (if (fboundp 'package-desc-version)
                                  'package--ac-desc-version
                                'package-desc-vers)
                              (cdr package))
                     archive))
    ad-do-it))


(defvar melpa-exclude-packages
  ;;      '(slime magit)
  '()
  "Don't install Melpa versions of these packages.")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (not (string-equal archive "melpa"))
             (not (memq package melpa-exclude-packages))))))


;; Make sure `use-package' is available (used to configure the rest of the packages)
;; Bootstrap use-package
;; Install use-package if it's not already installed.
(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; Configure `use-package' prior to loading it.
(setq use-package-enable-imenu-support t)
(setq use-package-minimum-reported-time 0)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'use-package)
(setq-default use-package-minimum-reported-time 0)
(setq use-package-minimum-reported-time 0)
(eval-and-compile (setq-default use-package-verbose t))
(eval-and-compile (setq use-package-verbose t))
(setq use-package-always-ensure t) ;; The :ensure keyword causes the package(s) to be installed automatically if not already present on your system


;; Other options from Prot(eval-and-compile
;; (setq use-package-always-ensure nil)
;; (setq use-package-always-defer nil)
;; (setq use-package-always-demand nil)
;; (setq use-package-expand-minimally nil)

;; More Prot Stuff
;; The following is VERY IMPORTANT.  Write hooks using their real name
;; instead of a shorter version: after-init ==> `after-init-hook'.
;;
;; This is to empower help commands with their contextual awareness,
;; such as `describe-symbol'.
;; (setq use-package-hook-name-suffix nil)

;;; Garbage collection the automated way
;;; my system collapsed when I used it!!
;; (use-package gcmh
;;   :ensure t
;;   :init
;;   (gcmh-mode 1))

(use-package bind-key)


;; tip from https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=810640

(setq tramp-ssh-controlmaster-options nil)
(setq-default tramp-ssh-controlmaster-options nil)

;; tramp package from
;; https://github.com/danielmai/.emacs.d/blob/master/config.org
;; TRAMP (Transparent Remote Access, Multiple Protocols) is a package for editing remote files, similar to AngeFtp or efs. Whereas the others use FTP to connect to the remote host and to transfer the files, TRAMP uses a remote shell connection (rlogin, telnet, ssh). It can transfer the files using rcp or a similar program, or it can encode the file contents (using uuencode or base64) and transfer them right through the shell connection.
;; Tramp was formerly called RCP or rcp.el.
(use-package tramp
  :defer t)

;; Do this to quicken startup:
;; https://emacs.stackexchange.com/questions/14708/debugging-slowness-in-init-file-not-hostname-related
;; because I was getting the startup process tripped until I did this.
;;;;;;;;;; commented out by dgm on January the 10th as i rather go for
;;;;;;;;;;;; the nil option
;;;;;;;;;;;; (setq tramp-ssh-controlmaster-options "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; async ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; activate for all packages. Advised in Helm's wiki
;; also, uncle dave says: Lets us use asynchronous processes wherever
;; possible, pretty useful.
(use-package async
  :init (dired-async-mode 1)
  :config
  (setq async-bytecomp-allowed-packages '(all))
  )


;; Deal with the "Failed to verify signature archive-contents.sig"
;; (use-package gnu-elpa-keyring)

(use-package gnu-elpa-keyring-update)

;; (setq package-check-signature nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  exwm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sacha says: (server-start) permits the use of emacsclient,
;; emacsclient, and org-protocol.
;; emacs --daemon, which starts a server automatically but with --daemon, Emacs doesn't start off
;; in a graphical environment.

;; dgm comments out on Nov 13th 2019 to try and stop emacs from
;; creating new frames
;; (server-start)
(require 'server)
(or (server-running-p) (server-start))

;; When stating the client from .xinitrc, =save-buffer-kill-terminal= will =force-kill= Emacs before it can run through =kill-emacs-hook=.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; (defun efs/exwm-update-class ()
;;   (exwm-workspace-rename-buffer exwm-class-name))

(use-package cl-generic)

(use-package xelb)

(use-package exwm
  :init
  (load "/home/dgm/.emacs.d/src/ambrevar/functions.el")
  (require 'functions)

  (defun ambrevar/exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  (defun ambrevar/exwm-start-in-char-mode ()
    (when (string-prefix-p "emacs" exwm-instance-name)
      (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
  (add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)

  (defun exwm-async-run (name)
    (interactive)
    (start-process name nil name))

  (defun daedreth/launch-browser ()
    (interactive)
    (exwm-async-run "chromium"))

  ;; recall it doesn't prompt for anything. You just have to enter your password.
  (defun daedreth/lock-screen ()
    (interactive)
    (exwm-async-run "slock"))

  (defun ambrevar/exwm-start (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun rotate-windows ()
    "Rotate your windows" (interactive) (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
                                              (t
                                               (setq i 1)
                                               (setq numWindows (count-windows))
                                               (while  (< i numWindows)
                                                 (let* (
                                                        (w1 (elt (window-list) i))
                                                        (w2 (elt (window-list) (+ (% i numWindows) 1)))
                                                        (b1 (window-buffer w1))
                                                        (b2 (window-buffer w2))
                                                        (s1 (window-start w1))
                                                        (s2 (window-start w2))
                                                        )
                                                   (set-window-buffer w1  b2)
                                                   (set-window-buffer w2 b1)
                                                   (set-window-start w1 s2)
                                                   (set-window-start w2 s1)
                                                   (setq i (1+ i)))))))

  ;; When window "class" updates, use it to set the buffer name
  ;; (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Wallpaper function
  (defun efs/set-wallpaper ()
    (interactive)
    (start-process-shell-command
     "feh" nil "feh --bg-scale /home/dgm/Pictures/500350.jpg"))
  ;;  "feh" nil "feh --bg-scale /home/dgm/Pictures/500350.jpg"
  ;; "feh" nil "feh --bg-scale /home/dgm/Pictures/fedora-jules-verne-nautilus-submarine-1920x1080.jpg"
  ;; Mordickay and Rigby

  ;; Function to run apps in background
  (defun efs/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  ;; External apps to run before EXWM starts
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet")
  ;; (efs/run-in-background "redshift")

  ;; ("Evince" (exwm-workspace-move-window 3))
  ;; ("okular" (exwm-workspace-move-window 3))
  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Firefox-esr" (exwm-workspace-move-window 3))
      ("Chromium" (exwm-workspace-move-window 3))
      ("Spotify" (exwm-workspace-move-window 3))  ;; don't know why it doesn't work
      ;; for Spotify
      ("Gnome-terminal" (exwm-workspace-move-window 0))
      ("Amule" (exwm-workspace-move-window 0))
      ("Arandr" (exwm-workspace-move-window 0))))

  (defun efs/update-displays ()
    (efs/run-in-background "autorandr --change --force")
    ;; (efs/set-wallpaper)
    (message "Display config: %s"
             (string-trim (shell-command-to-string "autorandr --current"))))

  (defun efs/exwm-init-hook ()
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 4))

  (require 'exwm-config)
  ;; (require 'exwm-systemtray)
  (require 'exwm-randr)

  ;; necessary to configure exwm manuall
  :config
  ;; fringe size, most people prefer 1 (uncle dave's setup)
  ;; (fringe-mode 3)  ;; commented out on 14 march 21 to use a different setting
  ;; in <starter-kit-misc.org>
  (exwm-config-default)

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode)

  ;; Polybar is in charge of the tray now
  ;; (exwm-systemtray-enable)
  ;; (setq exwm-systemtray-height 16)

  (setq exwm-workspace-number 10
        exwm-layout-show-all-buffers t   ;;  allow an EXWM buffer to be displayed
        ;;  in any workspace. If I choose it, then I move that buffer to the
        ;;  workspace from which I am calling it (changed to t on 11 april 2021).
        exwm-workspace-show-all-buffers t) ;; if t, allows showing buffers on
  ;; other workspaces. (changed to t on 11 april 2021).
  ;; But DW has it set to t.
  ;; What DW is trying to do is to automatically move windows between workspaces.
  ;;  On 08/01/21
  ;;  I change it to nil as having it set to t doesn't let me call firefox
  ;;  windows in workspaces other than 3

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; For global bindings that work even in X buffers, use <exwm-input-set-key>
  ;; Also, these keybindings work while your're running EXWM (for normal EXWM
  ;; buffers. Not for external buffers). You don't have to
  ;; re-start it, as you would if you used exwm-input-global-keys (which updates
  ;; only after you restart exwm).
  ;; Commented by DGM on 25 dic 2020
  ;;  other workspaces.
  ;; (dotimes (i 10)
  ;;   (exwm-input-set-key (kbd (format "s-%d" i))
  ;;                       `(lambda ()
  ;;                          (interactive)
  ;;                          (exwm-workspace-switch-create ,i))))

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; Media keys
  (exwm-input-set-key
   (kbd "<XF86AudioRaiseVolume>")
   (lambda ()
     (interactive) (start-process-shell-command
                    "pactl" nil "pactl set-sink-volume 0 +5% && pactl set-sink-volume 1 +5%")))
  (exwm-input-set-key
   (kbd "<XF86AudioLowerVolume>")
   (lambda ()
     (interactive) (start-process-shell-command
                    "pactl" nil "pactl set-sink-volume 0 -5% && pactl set-sink-volume 1 -5%")))
  (exwm-input-set-key
   (kbd "<XF86AudioMute>")
   (lambda ()
     (interactive) (start-process-shell-command
                    "pactl" nil "pactl set-sink-mute 0 toggle && pactl set-sink-mute 1 toggle")))

  (defun display-backlight-brightness ()
    (message "Backlight at %s"
             (car (split-string
                   (shell-command-to-string "xbacklight -get")
                   "\\." t))))

  (exwm-input-set-key
   (kbd "<XF86MonBrightnessUp>")
   (lambda ()
     (interactive)
     (start-process-shell-command
      "xbacklight" nil "xbacklight -inc 5")
     (display-backlight-brightness)
     ))

  (exwm-input-set-key
   (kbd "<XF86MonBrightnessDown>")
   (lambda ()
     (interactive)
     (start-process-shell-command
      "xbacklight" nil "xbacklight -dec 5")
     (display-backlight-brightness)))

  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Redshift off
  (exwm-input-set-key (kbd "s-\)")
                      (lambda () (interactive) (start-process-shell-command "redshift" nil "redshift" "-x")))

  ;; Redshift on
  ;; -PO 3500 -m randr
  (exwm-input-set-key (kbd "s-\(")
                      (lambda () (interactive) (start-process-shell-command "redshift" nil "redshift" "-O 3500 -P")))


  ;; I comment this out as I think the next caters for it.
  ;; (push ?\s-  exwm-input-prefix-keys)

  ;; These keys should always pass through to Emacs.
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\C-\;
          ?\M-x
          ?\M-:
          ?\C-\M-j  ;; Buffer list (ivy)
          ?\C-\M-k  ;; Browser list
          ?\s-o     ;; Buffer list (helm)
          ?\s-e     ;; Launch external programs
          ;;?\M-s-up   ;; enlarge-window. Don't know why this syntax doesn't work
          ;;?\M-s-down ;; shrink-window
          ?\M-&
          ?\M-`
          ?\s-\;
          ?\C-q))

  ;; (add-to-list 'exwm-input-prefix-keys ?\C-q)
  ;; Ctrl+Q will enable the next key to be sent directly
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

  ;; Set up global key kindings. These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  ;; Though it is true that this other way of doing it worked as well
  ;; (exwm-input-set-key (kbd "s-<left>") #'windmove-left)
  ;; (exwm-input-set-key (kbd "s-<down>") #'windmove-down)
  ;; (exwm-input-set-key (kbd "s-<up>") #'windmove-up)
  ;; (exwm-input-set-key (kbd "s-<right>") #'windmove-right)
  ;; (exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)
  ;; (exwm-input-set-key (kbd "s-:") #'ambrevar/toggle-window-split)
  ;; (exwm-input-set-key (kbd "s-;") #'rotate-windows)

  (setq exwm-input-global-keys
        `(
          ;; Reset to line mode (C-c C-k switches to char-mode via
          ;; exwm-input-release-keyboard
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ;;          ([s-prior] . rotate-windows)

          ;; launch applications via shell command
          ([?\s-&] . ambrevar/exwm-start)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-:] . ambrevar/toggle-window-split)
          ([?\s-\;] . rotate-windows) ;; este no funciona bien, no se por que.

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number
          ;; (0-9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable)

  ;;   (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 1920x312 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal");

  ;; xrandr commented out as now <autorandr> takes care of all this.
  (when (string= (system-name) "toshiba")
    (exwm-randr-enable)
    (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --mode 1366x768 --pos 1920x312 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

    ;; (setq exwm-randr-workspace-monitor-plist
    ;;       '(0 "eDP-1"))

    ;; React to display connectivity changes, do initial display update
    (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
    (efs/update-displays))

  (when (string= (system-name) "lenovo")
    (exwm-randr-enable)
    (start-process-shell-command "xrandr" nil "xrandr --dpi 270 --output eDP-1 --primary --mode 3840x2160 --pos 0x0 --rotate normal")

  ;; (when (string= (system-name) "lenovo")
  ;;   (exwm-randr-enable)
  ;;   (start-process-shell-command "xrandr" nil "--dpi 270 --fb 7680x4320 \
  ;;      --output eDP-1 --mode 3840x2160 --pos 0x0 --rotate normal \
  ;;      --output HDMI-1-0 --primary --mode 1920x1080 --scale 2x2 --pos 3840x0 --rotate normal")

    ;; React to display connectivity changes, do initial display update
    (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
    (efs/update-displays))

  ;; for officePC
  (when (string=(system-name) "officePC")
    (exwm-randr-enable)
    ;;(setq exwm-randr-workspace-output-plist '(0 "VGA1"))
    (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1"))
    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
                (start-process-shell-command
                 ;; "xrandr" nil "xrandr --output HDMI-2 --left-of LVDS1 --auto")))
                 "xrandr" nil "xrandr --output HDMI-1 --auto")))

    (defun exwm-change-screen-hook ()
      (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
            default-output)
        (with-temp-buffer
          (call-process "xrandr" nil t nil)
          (goto-char (point-min))
          (re-search-forward xrandr-output-regexp nil 'noerror)
          (setq default-output (match-string 1))
          (forward-line)
          (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
              (call-process "xrandr" nil nil nil "--output" default-output "--auto")
            (call-process
             "xrandr" nil nil nil
             "--output" (match-string 1) "--primary" "--auto"
             "--output" default-output "--off")
            (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))))   ;;; este parentesis cierra el call a use-package to install exwm

;; (use-package exwm-edit)

;; Check for start-up errors. See =~/.profile=
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

(use-package delight)

;;; changed to <nil> on March, 15th, 2021
;;; debug options from https://github.com/ch11ng/exwm/wiki
(setq debug-on-error nil)
;; (setq debug-on-quit t)
(setq edebug-all-forms nil)

;; ;; Common Lisp compatability
(require 'cl-lib)

;; ;; Temporary workaround for eshell bug in 24.3.1
;; ;; http://zpcat.blogspot.com/2013/08/configure-eshell-mode-after-upgrade.html
(require 'esh-mode)

;; ;; Package Locations
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      ;; (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; ;; Load up Org Mode and Babel
;; ;; load up the main file
;; ;; org-mode windmove compatibility
(require 'org)
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

;; This tells Emacs to open all .org files in org-mode (http://sachachua.com/blog/2007/12/emacs-getting-things-done-with-org-basic/)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

 ;;; Higher garbage collection threshold
;; (setq gc-cons-threshold 20000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; The Power of UTF8 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable UTF-8 by default. From: lopez-ibanez.eu/dotemacs.html
(prefer-coding-system 'utf-8)
;;; other stuff from https://github.com/izahn/emacs-starter-kit
;;; kjh says that setting this coding system prevents emacs from choking on melpa file listings
;;; see also https://masteringemacs.org/article/working-coding-systems-unicode-emacs
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2. (Petersen's way. I commented it out now and
;; leave it as it was originally)
;; (if (boundp 'buffer-file-coding-system)
;;    (setq-default buffer-file-coding-system 'utf-8)
;;  (setq default-buffer-file-coding-system 'utf-8))
;; originally I had (setq buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq         buffer-file-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; next version as in Sacha Chua's dotfiles
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; UTF-8 as default encoding (permanently choose a encoding system in
;; emacs for opening and saving). See http://ergoemacs.org/emacs/emacs_encoding_decoding_faq.html
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; DGM on 4 October 2019 following Bern Hansen
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


;; DGM on 3 December 2020: default input method
;; tip from: https://emacs.stackexchange.com/questions/418/setting-and-activating-the-default-input-method

(setq default-input-method "spanish-prefix")

(defvar use-default-input-method t)
(make-variable-buffer-local 'use-default-input-method)
(defun activate-default-input-method ()
  (interactive)
  (if use-default-input-method
      (activate-input-method default-input-method)
    (inactivate-input-method)))
(add-hook 'after-change-major-mode-hook 'activate-default-input-method)
(add-hook 'minibuffer-setup-hook 'activate-default-input-method)
(defun inactivate-default-input-method ()
  (setq use-default-input-method nil))
(add-hook 'c-mode-hook 'inactivate-default-input-method)
(add-hook 'prog-mode-hook 'inactivate-default-input-method)


;;;;;;; dgm on 13 December 2018 to try and not get the conflict with utf-8-emacs
;; from https://stackoverflow.com/questions/24904208/emacs-windows-org-mode-encoding
;; (modify-coding-system-alist   'file "" 'utf-8-unix)  ;; this is the line that the guys in stackoverflow say fix everything
;; (setq coding-system-for-read  'utf-8)
;; (setq coding-system-for-write 'utf-8)

;;; Tuhdo's setup for the records
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
;; (prefer-coding-system 'utf-8)



;; Note in =starter-kit-org.org= there are two more lines on the coding system for the org mode case.

;; other stuff from
;; https://superuser.com/questions/410100/how-to-make-emacs-accept-utf-8-from-the-keyboard
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; (setq default-sendmail-coding-system 'utf-8-unix)


;;;;;;;;;;;;;;;;;;;;;;;;;;; PATH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell)

(setq-default exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-check-startup-files t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads: Dropbox ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Autostart Dropbox
(call-process-shell-command "(sleep 10s && ~/.dropbox-dist/dropboxd) &" nil 0)


;;;;;;;;;;;;;;;;;;;;;
;; Local variables ;;
;;;;;;;;;;;;;;;;;;;;;

;; From https://alhassy.github.io/init/
;; Let's always load local variables that we've marked as safe. ( I tend to use loads of such locals! )
(setq enable-local-variables :safe)




;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ambrevar stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prerequisites

;; (let ((minver "26.1"))
;;   (when (version< emacs-version minver)
;;     (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; ;;; Speed up init.
;; ;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
;; (defun ambrevar/reset-gc-cons-threshold ()
;;   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
;; (setq gc-cons-threshold (* 64 1024 1024))
;; (add-hook 'after-init-hook 'ambrevar/reset-gc-cons-threshold)
;; ;;; Temporarily disable the file name handler.
;; (setq default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (defun ambrevar/reset-file-name-handler-alist ()
;;   (setq file-name-handler-alist
;;         (append default-file-name-handler-alist
;;                 file-name-handler-alist))
;;   (cl-delete-duplicates file-name-handler-alist :test 'equal))
;; (add-hook 'after-init-hook 'ambrevar/reset-file-name-handler-alist)

;; ;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
;; (setq load-prefer-newer t)

;; ;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;; ;;; that `require' can find the files.
;; ;;; This must be done before moving `user-emacs-directory'.
;; (add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; ;;; Move user-emacs-directory so that user files don't mix with cache files.
;; (setq user-emacs-directory "~/.cache/emacs/")


;; ;;; Site Lisp folder for local packages and development.
;; ;; We need to roll it out manually since we want it first in the `load-path',
;; ;; while `normal-top-level-add-subdirs-to-load-path' appends it to the very end.
;; (defun ambrevar/package-refresh-load-path (path)
;;   "Add every non-hidden sub-folder of PATH to `load-path'."
;;   (when (file-directory-p path)
;;     (dolist (dir (directory-files path t "^[^\\.]"))
;;       (when (file-directory-p dir)
;;         (setq load-path (add-to-list 'load-path dir))
;;         (dolist (subdir (directory-files dir t "^[^\\.]"))
;;           (when (file-directory-p subdir)
;;             (setq load-path (add-to-list 'load-path subdir))))))))
;; (let ((site-lisp (expand-file-name "site-lisp/" "~/.local/share/emacs/")))
;;   (add-to-list 'load-path site-lisp)
;;   (ambrevar/package-refresh-load-path site-lisp))

;; ;;; Local config.  See below for an example usage.
;; (load "local-before" t)

;; (require 'functions)
;; (require 'main)
;; ;; (require 'visual)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; Assembly
;; (push 'nasm-mode package-selected-packages)

;; ;;; ChangeLog
;; (defun ambrevar/change-log-set-indent-rules ()
;;   (setq tab-width 2 left-margin 2))
;; (add-hook 'change-log-mode-hook 'ambrevar/change-log-set-indent-rules)

;; ;;; Diff
;; ;;; TODO: In diff-mode, both `[[` and `C-M-a` do not go back to previous index
;; ;;; once they are at the beginning of an index.
;; (nconc package-selected-packages '(ztree))

;; ;;; Dired
;; ;;; Dired is loaded after init.el, so configure it only then.
;; ;;; TODO: Improve dired-du:
;; ;;; - Hangs when the `ls` time format is changed.
;; ;;; - Cache recursive results.
;; (nconc package-selected-packages '(dired-du))
;; (with-eval-after-load 'dired (require 'init-dired))

;; ;;; Daemons.
;; (nconc package-selected-packages '(daemons))

;; ;;; Eshell
;; ;;; Extend completion.
;; (nconc package-selected-packages '(fish-completion bash-completion
;;                                                    pcomplete-extension pcmpl-args pcmpl-git))
;; (nconc package-selected-packages '(esh-autosuggest))
;; (with-eval-after-load 'eshell (require 'init-eshell))
;; (autoload 'ambrevar/eshell-or-new-session "eshell")

;; ;;; Expand region.
;; (nconc package-selected-packages '(expand-region))
;; (when  (require 'expand-region nil t)
;;   (global-set-key (kbd "C-=") 'er/expand-region))

;; ;;; Helm
;; (nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
;; (when (require 'helm-config nil t) (require 'init-helm))

;; ;;; Highlight
;; (nconc package-selected-packages '(hl-todo))
;; (when (require 'hl-todo nil t)
;;   (add-to-list 'hl-todo-keyword-faces `("REVIEW" . ,(alist-get "TODO" hl-todo-keyword-faces nil nil 'equal)))
;;   (global-hl-todo-mode)
;;   ;; (global-set-key (kbd "M-s M-o") 'hl-todo-occur)
;;   (define-key hl-todo-mode-map (kbd "M-s t") 'hl-todo-occur))

;; ;;; Iedit
;; (nconc package-selected-packages '(iedit))
;; (when (require 'iedit nil t)
;;   (global-set-key (kbd "C-;") 'iedit-mode))

;; ;;; Image
;; ;;; TODO: Disable white frame.
;; ;;; I think it's the cursor.
;; ;;; Evil-mode reverts cursor changes.
;; ;; (set-face-foreground 'cursor "black")
;; ;;; TODO: Implement other sxiv features:
;; ;;; - Gamma
;; ;;; - Marks
;; ;;; - Gallery
;; ;;; TODO: Is it possible to display an image fullscreen?
;; ;;; TODO: Image+: Dot no auto-adjust animated files
;; ;;; https://github.com/mhayashi1120/Emacs-imagex/issues/10
;; ;;; TODO: Image+: Restore animation state
;; ;;; https://github.com/mhayashi1120/Emacs-imagex/issues/9
;; ;; (nconc package-selected-packages '(image+))
;; ;; (with-eval-after-load 'image
;; ;;   (setq image-animate-loop t)
;; ;;   (add-hook 'image-mode-hook 'image-toggle-animation)
;; ;;   (require 'image+ nil t))

;; ;;; Indentation engine fix.
;; ;; (require 'smiext "init-smiext")

;; ;;; Indentation style guessing.
;; ;; (nconc 'package-selected-packages '(dtrt-indent))

;; ;;; Info-colors
;; ;; (nconc 'package-selected-packages '(info-colors))
;; (when (require 'info-colors nil t)
;;   (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;; ;;; Lisp
;; (nconc package-selected-packages '(lispy lispyville rainbow-delimiters geiser slime))
;; (with-eval-after-load 'lisp-mode (require 'init-lisp))
;; (setq geiser-repl-history-filename (expand-file-name "geiser_history" user-emacs-directory))

;; ;;; Magit
;; ;;; Magit can be loaded just-in-time.
;; ;; (nconc package-selected-packages '(magit magit-todos))
;; ;; (with-eval-after-load 'magit
;; ;;   (setq auto-revert-mode-text "")
;; ;;   (set-face-foreground 'magit-branch-remote "orange red")
;; ;;   (setq git-commit-summary-max-length fill-column)
;; ;;   ;; Customize what to fold by default.
;; ;;   ;; (push (cons [* commitbuf] 'hide) magit-section-initial-visibility-alist)
;; ;;   ;; Avoid conflict with WM.
;; ;;   (define-key magit-mode-map (kbd "s-<tab>") nil)
;; ;;   (setq magit-diff-refine-hunk 'all)
;; ;;   (when (require 'magit-todos nil t)
;; ;;     ;; REVIEW: Default scanner does not work on Guix because Git needs be compiled with PCRE.
;; ;;     (setq magit-todos-scanner #'magit-todos--scan-with-find|grep)
;; ;;     (magit-todos-mode)))
;; ;; (when (fboundp 'magit-status)
;; ;;   (global-set-key (kbd "C-x g") 'magit-status))

;; ;;; Mail
;; ;;; mu4e is usually site-local and not part of ELPA.
;; (when (or (fboundp 'mu4e)
;;           (delq nil (mapcar (lambda (path) (string-match "/mu4e/\\|/mu4e$" path)) load-path)))
;;   ;; (nconc package-selected-packages '(mu4e-maildirs-extension))
;;   (nconc package-selected-packages '(helm-mu)))
;; (with-eval-after-load 'mu4e
;;   ;; mu4e-conversation must be enabled here.
;;   ;; REVIEW: https://github.com/djcb/mu/issues/1258
;;   (when (require 'mu4e-conversation nil t)
;;     (global-mu4e-conversation-mode)
;;     ;; (setq mu4e-debug t)
;;     (setq mu4e-headers-show-threads nil
;;           mu4e-headers-include-related nil)
;;     ;; For testing purposes:
;;     ;; (set-face-background mu4e-conversation-sender-1 "#335533")
;;     ;; (set-face-background mu4e-conversation-sender-2 "#553333")
;;     ;; (set-face-background mu4e-conversation-sender-3 "#333355")
;;     ;; (set-face-background mu4e-conversation-sender-4 "#888855")
;;     ;; (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)
;;     (add-hook 'mu4e-conversation-hook 'flyspell-mode)
;;     ;; (add-hook
;;     ;;  'mu4e-conversation-before-send-hook
;;     ;;  (lambda ()
;;     ;;    (setq mu4e-compose-signature-auto-include nil)))
;;     (add-hook
;;      'mu4e-conversation-after-send-hook
;;      (lambda ()
;;        (let ((mu4e-get-mail-command "mbsync ambrevar-sent peneidhardt-sent"))
;;          (mu4e-update-mail-and-index 'run-in-background))))
;;     (add-hook 'mu4e-view-mode-hook 'auto-fill-mode))
;;   (require 'init-mu4e))
;; (autoload 'ambrevar/mu4e-headers "mu4e")

;; ;;; Makefile
;; (with-eval-after-load 'make-mode (require 'init-makefile))

;; ;;; Markdown
;; (nconc package-selected-packages '(markdown-mode))
;; (with-eval-after-load 'markdown-mode (require 'init-markdown))

;; ;;; Org-mode
;; (nconc package-selected-packages '(org-plus-contrib org-bullets helm-org-contacts)) ; org-plus contains latest Org mode.
;; (with-eval-after-load 'org (require 'init-org))
;; (autoload 'ambrevar/org-switch-agenda-file "org")
;; (autoload 'ambrevar/org-switch-agenda-file-other-window "org")

;; ;;; Packaging
;; (nconc package-selected-packages '(esup package-lint))

;; ;;; Pass
;; (nconc package-selected-packages '(helm-pass))

;; ;;; PDF
;; ;;; pdf-tools requires poppler built with cairo support.
;; ;;; We cannot defer loading as `pdf-tools-install' is required for PDF
;; ;;; association.
;; ;;; REVIEW: `save-place' does not seem to work with pdf-tools.
;; ;;; See https://github.com/politza/pdf-tools/issues/18.
;; ;;; TODO: windmove fails when selecting text and then moving up/down.
;; ;;; It only fails in Evil mode.
;; (nconc package-selected-packages '(pdf-tools))
;; (when (require 'pdf-tools nil t)
;;   ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
;;   (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; Amber
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
;;   (pdf-tools-install t t t))

;; ;;; Python
;; (with-eval-after-load 'python (require 'init-python))

;; ;;; Rainbow-mode
;; (nconc package-selected-packages '(rainbow-mode))
;; (when (require 'rainbow-mode nil t)
;;   (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
;;     (add-hook hook 'rainbow-mode)))

;; ;;; Screencast
;; ;; (nconc package-selected-packages '(camcorder))
;; (with-eval-after-load 'camcorder
;;   (setq camcorder-output-directory (expand-file-name "temp" "~")
;;         camcorder-gif-output-directory camcorder-output-directory)
;;   (setq camcorder-recording-command '("recordmydesktop" " --fps 10 --no-sound --windowid " window-id " -o " file))
;;   (add-to-list 'camcorder-gif-conversion-commands '("ffmpeg-slow" "ffmpeg -i " input-file " -vf 'fps=10,scale=1024:-1:flags=lanczos' " gif-file)))
;; (nconc package-selected-packages '(gif-screencast keycast))
;; (with-eval-after-load 'gif-screencast
;;   (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
;;   (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))

;; ;;; Shell
;; (with-eval-after-load 'sh-script (require 'init-sh))
;; ;;; Arch Linux PKGBUILD
;; (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
;; ;;; Gentoo
;; (add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))
;; (add-to-list 'auto-mode-alist '("\\.eclass\\'" . sh-mode))
;; (add-to-list 'auto-mode-alist '("package\\.accept_keywords" . sh-mode))
;; (add-to-list 'auto-mode-alist '("package\\.mask" . sh-mode))
;; (add-to-list 'auto-mode-alist '("package\\.use" . sh-mode))
;; ;;; If we ever need to edit exotic shell configs:
;; ;; (nconc package-selected-packages '(fish-mode rc-mode))

;; ;;; Srt (subtitles)
;; (add-to-list 'auto-mode-alist '("\\.srt\\'" . text-mode))

;; ;;; StackExchange
;; (nconc package-selected-packages '(sx))

;; (nconc package-selected-packages '(strace-mode))

;; (nconc package-selected-packages '(synosaurus))

;; ;;; Syntax checking
;; (nconc package-selected-packages '(flycheck helm-flycheck))
;; (when (require 'flycheck nil t) (require 'init-flycheck))

;; ;;; System packages
;; (nconc package-selected-packages '(helm-system-packages))
;; (global-set-key (kbd "C-x c #") 'helm-system-packages)

;; ;;; Terminal
;; (with-eval-after-load 'term
;;   ;; (require 'init-term)
;;   (setq term-buffer-maximum-size 0))

;; ;;; Torrent
;; (nconc package-selected-packages '(transmission))
;; (with-eval-after-load 'transmission
;;   ;; `transmission' will fail to start and will not run any hook if the daemon
;;   ;; is not up yet.
;;   ;; We need to advice the function :before to guarantee it starts.
;;   (defun ambrevar/transmission-start-daemon ()
;;     (unless (member "transmission-da"
;;                     (mapcar
;;                      (lambda (pid) (alist-get 'comm (process-attributes pid)))
;;                      (list-system-processes)))
;;       (call-process "transmission-daemon")
;;       (sleep-for 1)))
;;   (advice-add 'transmission :before 'ambrevar/transmission-start-daemon)
;;   (setq transmission-refresh-modes '(transmission-mode transmission-files-mode transmission-info-mode transmission-peers-mode)
;;         transmission-refresh-interval 1))

;; ;;; Translator
;; (nconc package-selected-packages '(google-translate))
;; (when (require 'google-translate nil t)
;;   (require 'google-translate-default-ui)
;;   ;; (global-set-key "\C-ct" 'google-translate-at-point)
;;   ;; (global-set-key "\C-cT" 'google-translate-query-translate)
;;   (defun ambrevar/google-translate-line ()
;;     "Translate current line and insert result after it, separated by ' = '."
;;     (interactive)
;;     (let* ((langs (google-translate-read-args nil nil))
;;            (source-language (car langs))
;;            (target-language (cadr langs))
;;            text
;;            result)
;;       (end-of-line)
;;       (just-one-space)
;;       (setq text (buffer-substring-no-properties
;;                   (line-beginning-position) (line-end-position)))
;;       (setq result (with-temp-buffer
;;                      (google-translate-translate
;;                       source-language target-language
;;                       text
;;                       'current-buffer)
;;                      (buffer-string))
;;             (insert "= " result)))))

;; ;;; Web forms.
;; ;;; Remove auto-fill in web edits because wikis and forums do not like it.
;; ;;; This works for qutebrowser, but may need changes for other browsers.
;; (defun ambrevar/browser-edit ()
;;   (when (require 'with-editor nil t) (with-editor-mode))
;;   (text-mode)
;;   (auto-fill-mode -1))
;; (add-to-list 'auto-mode-alist `(,(concat (getenv "BROWSER") "-editor-*") . ambrevar/browser-edit))

;; ;;; Wgrep
;; (nconc package-selected-packages '(wgrep-helm wgrep-pt))
;; (when (require 'wgrep nil t)
;;   ;; TODO: wgrep-face is not so pretty.
;;   (set-face-attribute 'wgrep-face nil :inherit 'ediff-current-diff-C :foreground 'unspecified :background 'unspecified :box nil))

;; ;;; Window manager
;; (nconc package-selected-packages '(exwm helm-exwm))
;; (nconc package-selected-packages '(pulseaudio-control))
;; (with-eval-after-load 'pulseaudio-control
;;   ;; REVIEW: Upstream should set path dynamically.
;;   ;; https://github.com/flexibeast/pulseaudio-control/issues/7
;;   (setq pulseaudio-control-pactl-path (executable-find "pactl")
;;         pulseaudio-control-volume-step "2%"))
;; (when (require 'exwm nil t) (require 'init-exwm))

;; ;;; Yasnippet
;; (nconc package-selected-packages '(yasnippet))

;; (nconc package-selected-packages '(youtube-dl))
;; (with-eval-after-load 'youtube-dl
;;   (setq youtube-dl-directory "~/temp"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Finalization

;; ;;; Don't let `customize' clutter my config.
;; (setq custom-file
;;       (if (boundp 'server-socket-dir)
;;           (expand-file-name "custom.el" server-socket-dir)
;;         (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)

;; ;;; Local config. You can use it to set system specific variables, such as the
;; ;;; external web browser or the geographical coordinates:
;; ;;
;; ;; (setq calendar-latitude 20.2158)
;; ;; (setq calendar-longitude 105.938)
;; (load "local-after" t)


;; Ambrevar's stuff (from his init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; commented out by dgm on 5 nov
;;2018 when trying to write a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starter-kit-helm file
;;;;;;;;;;;;; helm

;;(require 'helm)
;;(require 'helm-config)

;;(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
;; (when (require 'helm-config nil t) (require 'init-helm))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;; other helm-related stuff
;; commented out as flyspell makes email work very slowly and I don't
;; know if this bit of code is messing around
;;; Syntax checking
;; (nconc package-selected-packages '(flycheck helm-flycheck))
;; (when (require 'flycheck nil t) (require 'init-flycheck))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; commented out by dgm on
;;; 5 nov 2018
;;; System packages
;;;;;;;;;;;; (nconc package-selected-packages '(helm-system-packages))
;;;;;;;;;;;; (global-set-key (kbd "C-x c #") 'helm-system-packages)


;;;;;;;;;;;;;;;;;;;;;;;; commented out by dgm on 6 nov 2018
;;; Org-mode
;;(nconc package-selected-packages '(org-plus-contrib org-bullets helm-org-contacts)) ; org-plus contains latest Org mode.
;;(with-eval-after-load 'org (require 'init-org))


;;;;;;;;;;;;;;;;;;; mu4e ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following advice for reading advanced emails (with complicated
;; images) from /media/dgm/blue/documents/elibrary/computing/Linux/linuxFormat/tips/emacs2-email-video-246February2019.pdf

;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
(put 'magit-diff-edit-hunk-commit 'disabled nil)
