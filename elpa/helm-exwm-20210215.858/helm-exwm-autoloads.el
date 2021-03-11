;;; helm-exwm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-exwm" "helm-exwm.el" (0 0 0 0))
;;; Generated autoloads from helm-exwm.el

(autoload 'helm-exwm "helm-exwm" "\
Preconfigured `helm' to list EXWM buffers allowed by FILTER.

FILTER must be a function returning non-nil for allowed buffers,
nil for disallowed buffers.  FILTER is run in the context of each
buffer.

If FILTER is nil, then list all EXWM buffers.

Example: List all EXWM buffers but those running XTerm or the URL browser.

  (helm-exwm (function
              (lambda ()
                (pcase (downcase (or exwm-class-name \"\"))
                  (\"XTerm\" nil)
                  ((file-name-nondirectory browse-url-generic-program) nil)
                  (_ t)))))

\(fn &optional FILTER)" t nil)

(autoload 'helm-exwm-switch-browser "helm-exwm" "\
Switch to some `browse-url-generic-program' windows.

See `helm-exwm-switch'.

\(fn)" t nil)

(autoload 'helm-exwm-switch-browser-other-window "helm-exwm" "\
Switch to some `browse-url-generic-program' windows in other window.

See `helm-exwm-switch'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-exwm" '("helm-exwm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-exwm-autoloads.el ends here
