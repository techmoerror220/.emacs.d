(use-package tex
  :ensure auctex)

  (add-hook 'latex-mode-hook
            (lambda ()
              (set-face-attribute 'font-latex-sectioning-5-face nil :inherit nil :foreground "#b58900")
              (set-face-attribute 'font-latex-sectioning-0-face nil :height 3)
              (set-face-attribute 'font-latex-sectioning-1-face nil :height 2)
              (set-face-attribute 'font-latex-sectioning-2-face nil :height 1.5)
              (set-face-attribute 'font-latex-sectioning-3-face nil :height 1.2)
              (set-face-attribute 'font-latex-sectioning-4-face nil :height 1.0)))

  (setq TeX-open-quote "“")
  (setq TeX-close-quote "”")

    ;; Synctex with Evince
    (add-hook 'TeX-mode-hook
    (lambda ()
    (add-to-list 'TeX-output-view-style
    '("^pdf$" "."
     "/usr/bin/evince  %n %o %b")))
     )

  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
       '(("PDF Viewer" "/usr/bin/evince -b -g %n %o %b")))

    ;; Make emacs aware of multi-file projects
    ;; (setq-default TeX-master nil)

    ;; Auto-raise Emacs on activation (from Skim, usually)
;;    (defun raise-emacs-on-aqua()
;;    (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
;;    (add-hook 'server-switch-hook 'raise-emacs-on-aqua)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'pandoc-mode-hook 'turn-on-reftex)  ; with Pandoc mode
  (autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" t)
  (autoload 'reftex-citation "reftex-cite" "Make citation" t)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

  ;; Make RefTeX faster
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)

  ;; Make RefTeX work with Org-Mode
  ;; use 'C-c (' instead of 'C-c [' because the latter is already
  ;; defined in orgmode to the add-to-agenda command.
  (defun org-mode-reftex-setup ()
    (load-library "reftex") 
    (and (buffer-file-name)
    (file-exists-p (buffer-file-name))
    (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

  (add-hook 'org-mode-hook 'org-mode-reftex-setup)

  ;; RefTeX formats for biblatex (not natbib), and for pandoc
  (setq reftex-cite-format
        '(
          (?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?P . "[@%l]")
          (?T . "@%l [p. ]")
          (?x . "[]{%l}")
          (?X . "{%l}")
          ))

  (setq font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{") 
          ("citetitle" "[{") 
          ("citetitles" "[{") 
          ("headlessfullcite" "[{")))

  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)

  (autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
  (setq ebib-preload-bib-files 
        '("/media/dgm/blue/documents/bibs/socbib.bib"))
  (add-hook 'LaTeX-mode-hook #'(lambda ()
          (local-set-key "\C-c v" 'ebib-insert-bibtex-key)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

      (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

    ;; Biber under AUCTeX
    (defun TeX-run-Biber (name command file)
      "Create a process for NAME using COMMAND to format FILE with Biber." 
     (let ((process (TeX-run-command name command file)))
        (setq TeX-sentinel-function 'TeX-Biber-sentinel)
        (if TeX-process-asynchronous
            process
          (TeX-synchronous-sentinel name file process))))
    
    (defun TeX-Biber-sentinel (process name)
      "Cleanup TeX output buffer after running Biber."
      (goto-char (point-max))
      (cond
       ;; Check whether Biber reports any warnings or errors.
       ((re-search-backward (concat
                             "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                             "\\(warnings?\\|error messages?\\))") nil t)
        ;; Tell the user their number so that she sees whether the
        ;; situation is getting better or worse.
        (message (concat "Biber finished with %s %s. "
                         "Type `%s' to display output.")
                 (match-string 1) (match-string 2)
                 (substitute-command-keys
                  "\\\\[TeX-recenter-output-buffer]")))
       (t
        (message (concat "Biber finished successfully. "
                         "Run LaTeX again to get citations right."))))
      (setq TeX-command-next TeX-command-default))
  
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
    )

(setq bibtex-dialect "BibTeX")

  (setq bibtex-autokey-titleword-separator "_")
  (setq bibtex-autokey-year-title-separator ":_")

  (setq reftex-default-bibliography '("/media/dgm/blue/documents/bibs/socbib.bib"))

;;    (fset 'run-vc-then-xelatex
;;    [?\M-! ?v ?c return ?\C-c ?\C-c return])
;;    (global-set-key (kbd "C-c c") 'run-vc-then-xelatex);; Run the VC command before running xelatex
;;    (fset 'run-vc-then-xelatex
;;    [?\M-! ?v ?c return ?\C-c ?\C-c return])
;;    (global-set-key (kbd "\C-c c") 'run-vc-then-xelatex)

;;  (global-set-key (kbd "\C-c v")
;;                      (lambda ()
;;                        (interactive)
;;                        (shell-command "vc")))

  (message "Starter Kit LaTeX loaded.")
