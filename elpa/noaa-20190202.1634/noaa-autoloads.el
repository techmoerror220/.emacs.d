;;; noaa-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "noaa" "noaa.el" (0 0 0 0))
;;; Generated autoloads from noaa.el

(autoload 'noaa "noaa" "\
Request weather forecast data. Display the data in the buffer specified by ‘noaa-buffer-spec’.

\(fn)" t nil)

(autoload 'noaa-quit "noaa" "\
Leave the buffer specified by ‘noaa-buffer-spec’.

\(fn)" t nil)

(autoload 'noaa-mode "noaa" "\
Major mode for displaying NOAA weather data
\\{noaa-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "noaa" '("noaa-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; noaa-autoloads.el ends here
