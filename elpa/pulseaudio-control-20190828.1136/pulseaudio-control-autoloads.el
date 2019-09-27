;;; pulseaudio-control-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pulseaudio-control" "pulseaudio-control.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pulseaudio-control.el

(autoload 'pulseaudio-control-decrease-volume "pulseaudio-control" "\
Decrease volume of currently-selected Pulse sink.

Amount of decrease is specified by `pulseaudio-control-volume-step'.

\(fn)" t nil)

(autoload 'pulseaudio-control-default-keybindings "pulseaudio-control" "\
Make `C-x /' the prefix for accessing pulseaudio-control bindings.

\(fn)" t nil)

(autoload 'pulseaudio-control-display-volume "pulseaudio-control" "\
Display volume of currently-selected Pulse sink.

\(fn)" t nil)

(autoload 'pulseaudio-control-increase-volume "pulseaudio-control" "\
Increase volume of currently-selected Pulse sink.

Amount of increase is specified by `pulseaudio-control-volume-step'.

\(fn)" t nil)

(autoload 'pulseaudio-control-select-sink-by-index "pulseaudio-control" "\
Select which Pulse sink to act on, by numeric index.

Accepts number as prefix argument.

Argument SINK is the number provided by the user.

\(fn)" t nil)

(autoload 'pulseaudio-control-select-sink-by-name "pulseaudio-control" "\
Select which Pulse sink to act on, by name.

\(fn)" t nil)

(autoload 'pulseaudio-control-set-volume "pulseaudio-control" "\
Set volume of currently-selected Pulse sink.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user.

\(fn VOLUME)" t nil)

(autoload 'pulseaudio-control-toggle-current-sink-mute "pulseaudio-control" "\
Toggle muting of currently-selected Pulse sink.

\(fn)" t nil)

(autoload 'pulseaudio-control-toggle-current-source-mute "pulseaudio-control" "\
Toggle muting of currently-selected Pulse source.

\(fn)" t nil)

(autoload 'pulseaudio-control-toggle-sink-mute-by-index "pulseaudio-control" "\
Toggle muting of Pulse sink, specified by index.

Argument SINK is the number provided by the user.

\(fn SINK)" t nil)

(autoload 'pulseaudio-control-toggle-sink-mute-by-name "pulseaudio-control" "\
Toggle muting of Pulse sink, specified by name.

\(fn)" t nil)

(autoload 'pulseaudio-control-toggle-use-of-default-sink "pulseaudio-control" "\
Toggle use of @DEFAULT_SINK@ for volume operations.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pulseaudio-control" '("pulseaudio-control-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pulseaudio-control-autoloads.el ends here
