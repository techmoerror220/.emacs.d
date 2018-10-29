;;; python-x-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-help-mode" "python-help-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from python-help-mode.el

(autoload 'python-help-mode "python-help-mode" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-help-mode" '("python-help-")))

;;;***

;;;### (autoloads nil "python-x" "python-x.el" (0 0 0 0))
;;; Generated autoloads from python-x.el

(defconst python--vhl-available (if (require 'volatile-highlights nil t) t))

(defvar python-multiline-highlight python--vhl-available "\
When evaluating a statement which spans more than one line and less than a
screenful, highlight temporarily the evaluated region using `vhl/default-face'.
Requires `volatile-highlights' to be installed.")

(custom-autoload 'python-multiline-highlight "python-x" t)

(autoload 'python-shell-send-line "python-x" "\
Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell.

\(fn)" t nil)

(autoload 'python-shell-send-line-and-step "python-x" "\
Send the current line (with any remaining continuations) to the inferior Python process,
printing the result of the expression on the shell, then move on to the next
statement.

\(fn)" t nil)

(autoload 'python-shell-send-paragraph "python-x" "\
Send the current paragraph to the inferior Python process

\(fn)" t nil)

(autoload 'python-shell-send-paragraph-and-step "python-x" "\
Send the current paragraph to the inferior Python process, then move on to
the next.

\(fn)" t nil)

(autoload 'python-shell-send-region-or-paragraph "python-x" "\
Send the current region to the inferior Python process, if active.
Otherwise, send the current paragraph.

\(fn)" t nil)

(defvar python-section-delimiter "# ---" "\
Define the comment which marks the boundaries of the current code section.
See `python-shell-send-fold-or-section'.")

(custom-autoload 'python-section-delimiter "python-x" t)

(defvar python-section-highlight python--vhl-available "\
When evaluating a code fold/section with `python-shell-send-fold-or-section'
spanning more than one line, highlight temporarily the evaluated region using
`vhl/default-face'. Requires `volatile-highlights' to be installed.")

(custom-autoload 'python-section-highlight "python-x" t)

(autoload 'python-shell-send-fold-or-section "python-x" "\
Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries. When a 0 argument is provided, evaluate from
the beginning of the buffer up the current section. With a negative argument,
restart the process as well.

A code \"section\" is delimited in both directions, and in order, by:

- The nearest section delimiter (see `python-section-delimiter') contained
  within the current fold.
- The nearest fold delimiter (see `folding-mode-marks-alist').
- The buffer boundaries.

`folding-mode' doesn't need to be enabled, but the same marks are used to
define code boundaries. See `folding-add-to-marks-list' for customization.
Nested folds and sections are included: section delimiters contained within a
nested fold are ignored.

When the region to be evaluated is longer than a single line and less than a
screenful, the region is temporarily highlighted according to
`python-section-highlight'.

\(fn &optional ARG)" t nil)

(autoload 'python-x-shell-send-buffer "python-x" "\
Send the entire buffer to inferior Python process.
When called with a non-zero prefix argument, allow execution of code inside
blocks delimited by \"if __name__== '__main__':\". With a negative prefix
argument, restart the python process before evaluation.

\(fn &optional ARG)" t nil)

(autoload 'python-shell-send-fold-or-section-and-step "python-x" "\
Send the section of code at point to the inferior Python process, up to the
current fold or buffer boundaries, then move on to the next.

\(fn)" t nil)

(autoload 'python-shell-send-dwim "python-x" "\
Send the current region to the inferior Python process, if active.
Otherwise, use `python-shell-send-current-fold-or-section'

\(fn)" t nil)

(autoload 'python-forward-fold-or-section "python-x" "\
Move the point forward to the next fold or section marker. When a prefix
argument is provided, move COUNT times forward.

\(fn &optional COUNT)" t nil)

(autoload 'python-backward-fold-or-section "python-x" "\
Move the point backward to the previous fold or section marker. When a
prefix argument is provided, move COUNT times backward.

\(fn &optional COUNT)" t nil)

(autoload 'python-mark-fold-or-section "python-x" "\
Put point at beginning of this fold/section, mark at end.
The region marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following fold/section, so that the
number of sections marked equals ARG.

If ARG is negative, point is put at end of this fold/section, mark is put at
beginning of this or a previous paragraph.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is repeated
or (in Transient Mark mode) if the mark is active, it marks the next ARG
sections after the ones already marked.

\(fn &optional ARG ALLOW-EXTEND)" t nil)

(autoload 'python-shell-restart-process "python-x" "\
Restart the current Python process

\(fn)" t nil)

(autoload 'python-eldoc-for-region-or-symbol "python-x" "\
ElDoc for the current region or symbol at point. Similar to
`python-eldoc-at-point', but doesn't prompt unless given a prefix argument.

\(fn STRING)" t nil)

(autoload 'python-help-for-region-or-symbol "python-x" "\
Display documentation for the current region or symbol at point. If a prefix
argument is given, prompt for a statement to inspect.

\(fn STRING)" t nil)

(autoload 'python-shell-display-shell "python-x" "\
Display the inferior Python process in another window.

\(fn)" t nil)

(autoload 'python-shell-print-region-or-symbol "python-x" "\
Send the current region to the inferior Python process, if active; otherwise
the send the symbol at point. Print and display the result on the output buffer.

\(fn)" t nil)

(autoload 'python-x-setup "python-x" "\
Setup an ESS-like keyboard map in python-mode

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-x" '("python-")))

;;;***

;;;### (autoloads nil nil ("python-x-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-x-autoloads.el ends here
