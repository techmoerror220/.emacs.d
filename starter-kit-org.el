;; Autocomplete for orgmode
  ;; (require 'org-ac)
  ;; (org-ac/config-default)

  ;; Markdown exporter
  (require 'ox-md)

  ;; (setq org-completion-use-ido t)  ;; dgm disables in case it interferes with helm
  ;; (require 'org-special-blocks)
  ;; (if window-system (require 'org-mouse))

  ;; Compatibility with WindMove
  ;; Make windmove work in org-mode:
;;  (add-hook 'org-shiftup-final-hook 'windmove-up)
;;  (add-hook 'org-shiftleft-final-hook 'windmove-left)
;;  (add-hook 'org-shiftdown-final-hook 'windmove-down)
;;  (add-hook 'org-shiftright-final-hook 'windmove-right)
  ;; (if window-system (require 'org-mouse))

(use-package ox-pandoc
  :no-require t
  :defer 10
  :ensure t)

(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'TeX-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
;; (global-set-key (kbd "C-c C-p") 'pandoc-main-hydra/body) ;; not sure it is taken

(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-toc nil)
(setq org-html-toplevel-hlevel 2)
(setq org-export-htmlize-output-type 'css)
(setq org-export-html-coding-system 'utf-8-unix)
(setq org-html-viewport nil)
(setq org-export-with-smart-quotes t)

(setq org-publish-project-alist
       '(("org"
          :base-directory "~/.emacs.d/"
          :publishing-directory "/media/dgm/blue/documents/websites/esk/"
          :publishing-function org-html-publish-to-html
          :auto-sitemap t
          :sitemap-filename "index.org"
          :sitemap-title "Emacs Starter Kit for the Social Sciences: Documentation"
          :section-numbers t
          ;;:table-of-contents t
          :html-head "<link rel=\"stylesheet\"
                 href=\"http://kieranhealy.org/css/org.css\"
                 type=\"text/css\"/>"            )))

(setq org-html-postamble nil)

(require 'ox-twbs)

(require 'ob-latex)
;; (org-babel-add-interpreter "latex")
;; (add-to-list 'org-babel-tangle-langs '("latex" "tex"))

(add-to-list 'org-babel-noweb-error-langs "latex")

(define-key global-map "\C-cl" 'org-store-link)

(org-add-link-type "ebib" 'ebib)

 (org-add-link-type
   "cite" 'ebib
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "cite:" desc)))
             (format "\\cite{%s}" path)
             (format "\\cite[%s]{%s}" desc path)
             )))))

 (org-add-link-type
   "parencite" 'ebib
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "parencite:" desc)))
             (format "\\parencite{%s}" path)
             (format "\\parencite[%s]{%s}" desc path)
)))))

(org-add-link-type
   "textcite" 'ebib
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "textcite:" desc)))
             (format "\\textcite{%s}" path)
             (format "\\textcite[%s]{%s}" desc path)
)))))

(org-add-link-type
   "autocite" 'ebib
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "autocite:" desc)))
             (format "\\autocite{%s}" path)
         (format "\\autocite[%s]{%s}" desc path)
)))))

(org-add-link-type
 "footcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcite:" desc)))
         (format "\\footcite{%s}" path)
       (format "\\footcite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "fullcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "fullcite:" desc)))
         (format "\\fullcite{%s}" path)
       (format "\\fullcite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "citetitle" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitle:" desc)))
         (format "\\citetitle{%s}" path)
       (format "\\citetitle[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "citetitles" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitles:" desc)))
         (format "\\citetitles{%s}" path)
       (format "\\citetitles[%s]{%s}" desc path)
       )))))

(org-add-link-type
   "headlessfullcite" 'ebib
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "headlessfullcite:" desc)))
             (format "\\headlessfullcite{%s}" path)
             (format "\\headlessfullcite[%s]{%s}" desc path)
)))))

(require 'org-protocol)

;;  (defun yas/org-very-safe-expand ()
;;    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)
            ))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-\C-n" 'outline-next-visible-heading)
            (local-set-key "\M-\C-p" 'outline-previous-visible-heading)
            (local-set-key "\M-\C-u" 'outline-up-heading)
            ;; table
            (local-set-key "\M-\C-w" 'org-table-copy-region)
            (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
            (local-set-key "\M-\C-l" 'org-table-sort-lines)
            ;; display images
            (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; yasnippet (using the new org-cycle hooks)
            ;;(make-variable-buffer-local 'yas/trigger-key)
            ;;(setq yas/trigger-key [tab])
            ;;(add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            ;;(define-key yas/keymap [tab] 'yas/next-field)
            ))

(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-level-1 nil :height 1.5)
            (set-face-attribute 'org-level-2 nil :height 1.2)
            (set-face-attribute 'org-level-3 nil :height 1.1)
            (set-face-attribute 'org-level-4 nil :height 1.1)
            (set-face-attribute 'org-level-5 nil :height 1.1)))

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook
        '(lambda ()
           (visual-line-mode 1)))

(dolist (fun '(ambrevar/turn-off-linum ambrevar/turn-off-indent-tabs turn-off-auto-fill))
  (add-hook 'org-mode-hook fun))

(when (require 'org-contacts nil t)
;;  (let ((contacts "~/personal/contacts/contacts.org.gpg"))
  (let ((contacts "/media/dgm/blue/documents/dropbox/org/contacts.org.gpg"))
    (when (file-exists-p contacts)
      ;; When used to auto-complete e-mail addresses, the file is automatically
      ;; loaded.  The buffer usually need not be restored by a desktop session.
      (when desktop-save-mode
        (setq desktop-files-not-to-save
              (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote (expand-file-name contacts)) "\\)")))
      (setq org-contacts-files (list contacts)))))

(setq org-use-speed-commands t)

(require 'ess) 
(require 'ob-stata)

  (org-babel-do-load-languages
   'org-babel-load-languages
  '((emacs-lisp . t)
;;    (sh . t)
    (R . t)
    (perl . t)
    (ruby . t)
    (python . t)
    (js . t)
    (haskell . t)
    (stata . t)
    (shell . t)
    (latex . t)
    (scheme . t)
    ))


;; I am following Ista Zahn here: don't include (stata . t) but do (require 'ob-stata) afterwards. Stil, when I do so, I get the Debugger entered--Lisp error: (void-variable inferior-STA-program-name)
;; eval(inferior-STA-program-name) so the problem comes from =ob-stata=.

(add-to-list 'org-src-lang-modes
             '("r" . ess-mode))

(add-to-list 'org-src-lang-modes
             '("stata" . ess-mode))

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(setq org-confirm-babel-evaluate nil)

(use-package htmlize
  :ensure t)

(unless (boundp 'Info-directory-list)
  (setq Info-directory-list Info-default-directory-list))
(setq Info-directory-list
      (cons (expand-file-name
             "doc"
             (expand-file-name
              "org"
              (expand-file-name "src" dotfiles-dir)))
            Info-directory-list))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq org-M-RET-may-split-line '((default)))

(setq org-ellipsis " […]")

(setq org-adapt-indentation nil)

(setq org-archive-location "/home/dgm/Dropbox/gtd/archive.org::From %s")

(setq org-image-actual-width 550)
(setq org-highlight-latex-and-related '(latex script entities))

(setq org-tags-column 45)

(use-package org-ref
    :ensure t
    :init
    (setq org-ref-completion-library 'org-ref-helm-bibtex)
    (setq org-ref-notes-directory "/media/dgm/blue/documents/elibrary/org/references"
          org-ref-bibliography-notes "/media/dgm/blue/documents/elibrary/org/references/readings.org"
          org-ref-default-bibliography '("/media/dgm/blue/documents/bibs/socbib.bib")
          org-ref-pdf-directory "/media/dgm/blue/documents/elibrary/org/references/pdfs/"))

(require 'org-id)
(require 'org-ref-wos)
(require 'org-ref-scopus)
(require 'org-ref-pubmed)

;; (add-to-list 'org-ref-bibtex-completion-actions '("Edit notes" . helm-bibtex-edit-notes))

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
             '("py" "#+BEGIN_SRC python\n?\n#+END_SRC" ""))
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" ""))
(add-to-list 'org-structure-template-alist
             '("md" "#+BEGIN_SRC markdown\n?\n#+END_SRC" ""))

;;   (setq org-default-notes-file (concat org-directory "/notes.org")) ;; i disable this to see if I can choose between notes and tasks.
;;    this is not working for some reason: (define-key global-map "\C-c c" 'org-capture)
  (define-key global-map (kbd "C-c c") 'org-capture)

;; other bindings from http://orgmode.org/manual/Activation.html
;;     (global-set-key "\C-c l" 'org-store-link)  este binding ya estaba listo
;;     (global-set-key "\C-c a" 'org-agenda) ;; este binding puesto así no funcionaba
;;    (global-set-key "\C-c b" 'org-iswitchb);; este binding puesto así no funcionaba

  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c b") 'org-iswitchb)

(define-key global-map (kbd "S-<left>") 'org-timestamp-down-day)
(define-key global-map (kbd "S-<right>") 'org-timestamp-up-day)
(define-key global-map (kbd "S-<up>") 'org-timestamp-up)
(define-key global-map (kbd "S-<down>") 'org-timestamp-down)

(setq
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 ;; Add keywords.
; org-todo-keywords '((sequence "TODO" "REVIEW" "DONE"))  ;; commented
; out by dgm
 ; Customizations in:
 ; http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html added
 ; by dgm
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w)"  "|" "DONE(d)" "CANCELED(c)"))
 ; comments out
 ;; org-todo-keyword-faces '(("REVIEW" :inherit org-done))
 ;; Priorities.
 org-priority-start-cycle-with-default nil
 org-default-priority 67
 ;; Org-mode aligns text.
 indent-tabs-mode nil)

(setq org-directory "/home/dgm/Dropbox/gtd")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "/media/dgm/blue/documents/dropbox/mobileorg")
;; Set to the files (or directory of files) you want sync'd
;;   (setq org-agenda-files (quote ("/home/dgm/Dropbox/gtd")))  ;; this is the original line by kieran healy.

;; organization by: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;; Org agenda view scans these files and collects all the heading with a TODO (or related) keyword
(setq org-agenda-files (list "/home/dgm/Dropbox/gtd/inbox.org"
                             "/home/dgm/Dropbox/gtd/gtd.org"
                             "/home/dgm/Dropbox/gtd/journal.org"
                             "/media/dgm/blue/documents/proyectos/mtj/mtj_gtd.org" ; i could add it with =C-c [= but that action does not survive across sections
                             "/media/dgm/blue/documents/proyectos/iat_methods/iat_methods.org"
                             "/media/dgm/blue/documents/proyectos/laBussola/laBussola_gtd.org"
                             "/home/dgm/Dropbox/gtd/tickler.org"))

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
        '(("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)))  ;; review upcoming deadlines and appointments
            ;; old code ((org-agenda-fortnight-view)))
            ;; type "l" in the agenda to review logged items
            ;;            (stuck "") ;; review stuck projects as designated by org-stuck-projects
            (todo "STARTED") ;; review  actions that have started
            (todo "NEXT") ;; review next actions
            (todo "TODO") ;; review pending actions waiting for "next actiosn" to be fulfilled
            (todo "WAITING"))) ;; review waiting items
          ("f" "Fortnight Review"
           ((agenda "" ((org-agenda-span 14))) ;; review upcoming deadlines and appointments
            ;; type "l" in the agenda to review logged items
            ;;            (stuck "") ;; review stuck projects as designated by org-stuck-projects
            (todo "STARTED") ;; review  actions that have started
            (todo "NEXT") ;; review next actions
            (todo "TODO") ;; review pending actions waiting for "next actiosn" to be fulfilled
            (todo "WAITING"))) ;; review waiting items
          ("r" "Monthly Review"
           ((agenda "" ((org-agenda-span 31))) ;; review upcoming deadlines and appointments
            ;; type "l" in the agenda to review logged items
            ;;            (stuck "") ;; review stuck projects as designated by org-stuck-projects
            (todo "STARTED") ;; review  actions that have started
            (todo "NEXT") ;; review next actions
            (todo "TODO") ;; review pending actions waiting for "next actiosn" to be fulfilled
            (todo "WAITING"))) ;; review waiting items
          ("y" "Yearly Review"
           ((agenda "" ((org-agenda-span (quote year))))))
          ("p" "Project Review"
           ((tags "project")
            (todo "STARTED") ;; review  actions that have started
            (todo "NEXT") ;; review next actions
            (todo "TODO")))  ;; review pending actions waiting for "next actions" to be fulfilled
          ;;          ("l" "Monthly (Long-Term) Review"
          ;;           ((agenda "" ((org-agenda-span (quote month))))))
          ("o" "Office and home agenda"
           ((agenda "" ((org-agenda-ndays 1))) ;; esto no me funciona... debe ser que (setq org-agenda-span (quote month)) en starter-kit-org.org tiene prioridad
            ;; limits the agenda display to a single day
            (tags-todo "@office|@home")
            (tags-todo "office|home")
            ;;            (tags "project+CATEGORY=\"elephants\"")
            ;;            (tags "review" ((org-agenda-files '("~/org/circuspeanuts.org"))))
            ;; limits the tag search to the file circuspeanuts.org
            ;;            (todo "WAITING")
            )
           ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
          ;; ...other commands here
          ))

(defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

(setq org-mobile-inbox-for-pull "/media/dgm/blue/documents/dropbox/org/fromMobile.org")

(defun my-org-project-list ()
   "Get me a list of projects."
   (interactive)
   (let ((org-tags-match-list-sublevels nil))
   (org-tags-view nil "project")))

(setq org-capture-templates '(
                              ;; ("t" "Todo [inbox]" entry
                              ;;   (file+headline "/home/dgm/Dropbox/gtd/inbox.org" "Tasks")
                              ;;   "* TODO %i%? \nEntry added on: %U
                              ;;                    \nEntry created from this heading or email: %a")
                              ;; ("a"  "Article"  entry  
                              ;;  (file+headline "/home/dgm/Dropbox/gtd/bibliography.org" "Bibliography") 
                              ;;    "* %a %^g
                              ;;     \n:PROPERTIES: 
                              ;;     \n:Created: %U
                              ;;     \n:END:
                              ;;     \n%i
                              ;;     \nBrief description:
                              ;;     \n%?"  
                              ;;  :empty-lines 1    
                              ;;  :created t)        
                                ("T" "Tickler" entry
                                 (file+headline "/home/dgm/Dropbox/gtd/tickler.org" "Tickler")
                                 "* %i%?
                                      \nEntry added on: %U from %a")
                                ("j" "Journal" entry
                                 (file+datetree "/home/dgm/Dropbox/gtd/journal.org")
                                 "* %?
                                      \n Added on: %U")
                                ("n" "Note" entry
                                 (file "~/Dropbox/gtd/notes.org")
                                     "* %?\nCaptured on %U from %a")))

(push `("t" "Todo" entry (file+headline "/home/dgm/Dropbox/gtd/inbox.org" "Tasks")
        ,(string-join
          '("* TODO %^{Description}"
            "  %?"
            "  %a"
            "  :LOGBOOK:"
            "  - Captured on %U from %a"
            "  :END:")
          "\n"))
      org-capture-templates)

(push `("r" "Respond later" entry (file+headline "~/Dropbox/gtd/inbox.org" "Email")
        ,(string-join
          '("* TODO Respond to %:from on %a"
            "  %?"
            "  :LOGBOOK:"
            "  - Captured on %U from %a"
            "  :END:")
          "\n"))
        org-capture-templates)

(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(setq org-tag-alist '(("airbnb" . ?a) ("@computer" . ?b) ("course" . ?c)  ("errands" . ?e) ("@home" . ?h) ("medicos" . ?m) ("@office" . ?o)  ("@phone" . ?p) ("project" . ?q) ("teaching" . ?t) ("uned" . ?u)))

(require 'org-agenda)
  (require 'holidays)
  (setq calendar-holidays holiday-other-holidays)
  (setq org-agenda-include-diary t)

  (setq holiday-other-holidays
        '((holiday-fixed 1 1 "Año Nuevo")
          (holiday-fixed 1 6 "Día de Reyes")
          (holiday-fixed 2 14 "Miércoles de Ceniza")
          (holiday-sexp '(calendar-nth-named-day 1 1 3 year 19) "Día de San José")
          (holiday-easter-etc -7 "Domingo de Ramos")
          (holiday-easter-etc -3 "Jueves Santo")
          (holiday-easter-etc -2 "Viernes Santo")
          (holiday-easter-etc +1 "Lunes de Pascua")
          (holiday-fixed 5 1 "Día Internacional del Trabajo")
          (holiday-fixed 5 2 "Día de la Comunidad de Madrid")
          (holiday-fixed 5 6 "Día de la Madre")
          (holiday-fixed 5 15 "Día de San Isidro")
          (holiday-fixed 5 31 "Corpus Christi (Madrid)")
;;          (holiday-easter-etc +43 "Día de la Ascención")
          (holiday-easter-etc +64 "Corpus Christi")
          (holiday-fixed 10 12 "Día de la Hispanidad")
          (holiday-fixed 11 1  "Todos los santos")
          (holiday-fixed 12 25 "Navidad")
          (holiday-fixed 12 6 "Día de la Constitución")
          (holiday-fixed 12 8 "Inmaculada Concepción")
          ))

(setq calendar-week-start-day 1)

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)

(setq org-refile-targets '(("/home/dgm/Dropbox/gtd/gtd.org" :maxlevel . 3)
                           ("/media/dgm/blue/documents/proyectos/mtj/mtj_gtd.org" :maxlevel . 2)
                           ("/media/dgm/blue/documents/proyectos/laBussola/laBussola_gtd.org" :maxlevel . 2)
                           ("/media/dgm/blue/documents/proyectos/iat_methods/iat_methods.org" :maxlevel . 2)
                           ("/home/dgm/Dropbox/gtd/someday.org" :level . 2)
                           ("/home/dgm/Dropbox/gtd/inbox.org" :level . 2)
                           ("/home/dgm/Dropbox/gtd/notes.org" :level . 2)
                           ("/home/dgm/Dropbox/gtd/tickler.org" :level . 2)))

(setq org-refile-allow-creating-parent-nodes 'confirm)

(autoload 'ambrevar/org-switch-agenda-file "org")
(autoload 'ambrevar/org-switch-agenda-file-other-window "org")

(setq org-imenu-depth 5)

(provide 'starter-kit-org)

(message "Starter Kit Org loaded.")
