;;  (require 'cl)    ;; in init.el 
;;  (require 'ffap)  ;; in init.el
;;  (require 'uniquify) ;; already included in dgm.org under Better defaults
;;  (require 'ansi-color) ;; in init.el
;;  (require 'recentf) ;; already in dgm.org

(defun starter-kit-loadable-p (package)
  "Check if PACKAGE is loadable from a directory in `load-path'."
  (let ((load-file (concat (symbol-name package) ".el")))
    (catch 'file-found
      (dolist (dir load-path)
        (let ((path (expand-file-name load-file dir)))
          (when (file-exists-p path)
            (throw 'file-found path)))))))

(defun starter-kit-load (file)
  "This function is to be used to load starter-kit-*.org files."
  (org-babel-load-file (expand-file-name file
                                         dotfiles-dir)))

(defun starter-kit-compile (&optional arg)
  "Tangle and Byte compile all starter-kit files."
  (interactive "P")
  (cl-flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (mapc
     (lambda (file)
       (when (string= "org" (file-name-extension file))
         (let ((el-file (concat (file-name-sans-extension file) ".el")))
           (when (or arg
                     (not (and (file-exists-p el-file)
                               (> (age file) (age el-file)))))
             (org-babel-tangle-file file el-file "emacs-lisp")
             (byte-compile-file el-file)))))
     (apply #'append
            (mapcar
             (lambda (d)
               (when (and (file-exists-p d) (file-directory-p d))
                 (mapcar (lambda (f) (expand-file-name f d)) (directory-files d))))
             (list (concat dotfiles-dir user-login-name) dotfiles-dir))))))

(starter-kit-load "starter-kit-aspell.org")

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;;  (require 'package)
;;  (setq package-archives
;;             '(
;;              ("org"     . "http://orgmode.org/elpa/")
;;              ("gnu"     . "http://elpa.gnu.org/packages/")
;;              ("melpa"   . "https://melpa.org/packages/")))

;;;; from https://github.com/danielmai/.emacs.d/blob/master/init.el
;;(when (boundp 'package-pinned-packages)
;;  (setq package-pinned-packages
;;        '((org-plus-contrib . "org"))))


;; commented out as this blocked exwm when called from init.el
;; I think this all means that now my Emacs config does not call on Elpa on startup and that is why I can now use emacs with no internet connection
;; So comment out if no internet connection available

;; Emacs 27: Warning (package): Unnecessary call to ‘package-initialize’ in init file
;;    (package-initialize)
;;    (when (esk-online?)
;;       (ignore-errors (package-refresh-contents)))
;;    (starter-kit-load "starter-kit-elpa.org")

;;  (package-initialize)
;;  (ignore-errors (package-refresh-contents))
;;  (starter-kit-load "starter-kit-elpa.org")

;;  (if (eq system-type 'darwin)
;;      (setq system-name (car (split-string system-name "\\."))))

(setq system-specific-config (concat dotfiles-dir system-name ".el")
      system-specific-literate-config (concat dotfiles-dir system-name ".org")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-literate-config (concat dotfiles-dir user-login-name ".org")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(setq elisp-source-dir (concat dotfiles-dir "src"))
(add-to-list 'load-path elisp-source-dir)

(starter-kit-load "starter-kit-exwm.org")

(starter-kit-load "starter-kit-defuns.org")

(starter-kit-load "starter-kit-latex.org")

(starter-kit-load "starter-kit-latex-org.org")

(starter-kit-load "starter-kit-org.org")

(starter-kit-load "starter-kit-helm.org")

(starter-kit-load "starter-kit-bindings.org")

(starter-kit-load "starter-kit-misc.org")

(starter-kit-load "starter-kit-completion.org")

(starter-kit-load "starter-kit-search-engine-with-eww.org")

(starter-kit-load "starter-kit-eshell.org")

(starter-kit-load "starter-kit-lisp.org")

(starter-kit-load "starter-kit-js.org")

(starter-kit-load "starter-kit-perl.org")

(starter-kit-load "starter-kit-python.org")

(starter-kit-load "starter-kit-tufte-latex.org")

(starter-kit-load "starter-kit-stats.org")

(starter-kit-load "starter-kit-text.org")

(starter-kit-load "starter-kit-parens.org")

(starter-kit-load "starter-kit-mu4e.org")

(starter-kit-load "starter-kit-pass.org")

(starter-kit-load "starter-kit-programming.org")

(load custom-file 'noerror)

(starter-kit-load "dgm.org")

(provide 'starter-kit)

(message "Starter Kit main (starter-kit.org) file loaded.")
