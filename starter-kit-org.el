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
  :ensure t)

(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'TeX-mode-hook 'pandoc-mode)  
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
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

(add-to-list
 'file-coding-system-alist '("\\.org" . utf-8-unix))

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

(use-package ox-twbs
  :ensure t)

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

(use-package org-protocol
  :ensure nil)

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
            ;;(local-set-key "<M-up>" 'org-move-item-up)
            ;;(local-set-key "<M-down>" 'org-move-item-down)
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

(setq org-ref-default-citation-link "textcite")

(setq org-latex-prefer-user-labels t)

;;   (setq org-default-notes-file (concat org-directory "/notes.org")) ;; i disable this to see if I can choose between notes and tasks.
;;    this is not working for some reason: (define-key global-map "\C-c c" 'org-capture)
(define-key global-map (kbd "C-c c") 'org-capture)

;; other bindings from http://orgmode.org/manual/Activation.html
;;     (global-set-key "\C-c l" 'org-store-link)  este binding ya estaba listo
;;     (global-set-key "\C-c a" 'org-agenda) ;; este binding puesto así no funcionaba
;;    (global-set-key "\C-c b" 'org-iswitch);; este binding puesto así no funcionaba

(define-key global-map (kbd "C-c a") 'org-agenda)
;;(define-key global-map (kbd "C-c b") 'org-iswitch) ;; I need C-c b for ido-switch-buffer

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
 org-default-priority 67)
;; Org-mode aligns text. But already defined elsewhere (in starter-misc.)
;; indent-tabs-mode nil)

(setq org-todo-keyword-faces
   '(("TODO"     . (:foreground "#d33682" :weight bold))
    ("NEXT"      . (:foreground "#dc322f" :weight bold))
    ("STARTED"   . (:foreground "#859900" :weight bold))
    ("WAITING"   . (:foreground "#b58900" :weight bold))
    ("DONE"      . (:foreground "#268bd2" :weight bold))
    ("CANCELED"  . (:foreground "#2aa198" :weight bold))))

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

(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(setq org-tag-alist '(("econometrics" . ?a) ("web-browsing" . ?b)  ("cooking" . ?c) ("divorcio" . ?d) ("emacs" . ?e) ("lisp" . ?g) ("@home" . ?h) ("kenedy" . ?k) ("leo" . ?l)  ("maths" . ?m) ("@office" . ?o)  ("project" . ?p) ("reading" . ?r) ("salud" . ?s) ("@mail-tel" . ?t) ("uned-admin" . ?u)  ("python" . ?y) ("uned-teaching" . ?v)  ("writing" . ?w)  ("@errands" . ?z)))

(require 'org-agenda)
(require 'holidays)
(setq calendar-holidays holiday-other-holidays)
(setq org-agenda-include-diary t)

;; (setq holiday-other-holidays  ;; with this holidays don't show up in agenda.
;; with the following holidays show up in org-agenda but still it does not show up in orgzly so watch out! 
(setq calendar-holidays          
      '((holiday-fixed 1 1 "Año Nuevo / New Year's Day")
        (holiday-fixed 1 6 "Día de Reyes / Epiphany") 
        (holiday-fixed 2 14 "Valentine's Day")
        ;;          (holiday-fixed 3 5 "Martes de Carnaval")
        ;;          (holiday-fixed 3 6 "Miércoles de Ceniza")
        (holiday-easter-etc -47 "Martes de Carnaval / Shrove Tuesday") 
        (holiday-easter-etc -46 "Miércoles de Ceniza / Ash Friday")
        (holiday-sexp '(calendar-nth-named-day 1 1 3 year 19) "Día de San José")
        (holiday-easter-etc -7 "Domingo de Ramos / Palm Sunday")
        (holiday-easter-etc -3 "Jueves Santo / Maundy Thursday")
        (holiday-easter-etc -2 "Viernes Santo / Good Friday")
        (holiday-easter-etc 0 "Domingo de Resurrección o Pascua / Easter Sunday")
        (holiday-easter-etc +1 "Lunes de Pascua")
        (holiday-fixed 5 1 "Día Internacional del Trabajo")
        (holiday-fixed 5 2 "Día de la Comunidad de Madrid")
        (holiday-fixed 5 6 "Día de la Madre")
        (holiday-fixed 5 15 "Día de San Isidro")
        (holiday-fixed 5 31 "Corpus Christi (Madrid)")
        ;;  (holiday-easter-etc +43 "Día de la Ascención")
        ;;  (holiday-easter-etc +64 "Corpus Christi")
        (holiday-easter-etc 60 "Corpus Christi") ;; en http://lists.gnu.org/archive/html/emacs-devel/2004-07/msg00494.html
        (holiday-fixed 10 12 "Día de la Hispanidad")
        (holiday-fixed 11 1  "Todos los santos")
        (holiday-fixed 11 9  "Día de la Almudena")
        (holiday-fixed 12 25 "Natividad del Señor")
        (holiday-fixed 12 6 "Día de la Constitución")
        (holiday-fixed 12 8 "Inmaculada Concepción") ;; en 2018. En 2019 se trasladará al lunes 9. Ojo! Cambiar con el nuevo agno!!
        ))

'(org-agenda-include-diary t)

(setq calendar-week-start-day 1)

(setq org-refile-targets '(("/home/dgm/Dropbox/gtd/gtd.org" :maxlevel . 3)
                           ("/media/dgm/blue/documents/proyectos/mtj/mtj_gtd.org" :level . 2)
                           ("/media/dgm/blue/documents/proyectos/laBussola/laBussola_gtd.org" :level . 2)
                           ("/media/dgm/blue/documents/proyectos/iat_methods/iat_methods.org" :level . 2)
                           ("/home/dgm/Dropbox/gtd/someday.org" :maxlevel . 2)                    
                           ("/home/dgm/Dropbox/gtd/inbox.org" :level . 2)
                           ("/home/dgm/Dropbox/gtd/notes.org" :maxlevel . 2)
                           ("/home/dgm/Dropbox/gtd/tickler.org" :maxlevel . 2)))

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-refile-use-outline-path t)          ;; show full paths to refiling
(setq org-outline-path-complete-in-steps nil) ;; Change to nil to refile in a single go.

(autoload 'ambrevar/org-switch-agenda-file "org")
(autoload 'ambrevar/org-switch-agenda-file-other-window "org")

(setq org-imenu-depth 5)

;;(setq org-startup-folded 'showeverything)
;;(setq org-inhibit-startup-visibility-stuff t)
;;(setq org-save-outline-visibility t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)

(setq font-lock-maximum-decoration        
      '((org-mode . 1)))

;;  (setq org-goto-interface 'outline
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
;;(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere;; DGM: doesn't work and C-c C-x C-j does the job already
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(defun my/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(defun my/org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
           (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))

(defun my/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
		 (seconds-to-time
		  (+ 86400.0
			 (float-time
			  (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
	(org-insert-heading-after-current)
	(insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defun my/org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))
(eval-after-load 'org-capture
  '(bind-key "C-c C-r" 'my/org-refile-and-jump org-capture-mode-map))

(setq org-reverse-note-order t)
;; (setq org-refile-use-outline-path nil) ;; set to t above
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-refile-use-cache nil)
;; (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3)))) ;; check if this is more efficient than my current code
(setq org-blank-before-new-entry nil)

(require 'org-clock)
(defun my/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
	         (minutes (org-clock-sum-current-item))
	         (wpm (/ words minutes)))
	    (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
	    (kill-new (number-to-string wpm))))))

(setq org-log-done 'time)

(setq org-tags-exclude-from-inheritance '("project"))

(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

(defun my/org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my/org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))
(add-to-list 'org-speed-commands-user '("T" my/org-agenda-for-subtree))

(add-to-list 'org-speed-commands-user '("S" call-interactively 'org-sort))

(use-package org
  :init
  (progn
    (setq org-expiry-inactive-timestamps t)
    (setq org-clock-idle-time nil)
    (setq org-log-done 'time)
    (setq org-clock-continuously nil)
    (setq org-clock-persist t)  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-in-switch-to-state "STARTED")
    (setq org-clock-in-resume nil)  ;; t to Resume clocking task on clock-in if the clock is open
    (setq org-show-notification-handler 'message)
    (setq org-time-stamp-rounding-minutes (quote (0 5)))
    (setq org-clock-report-include-clocking-task t))
  :config
  (org-clock-persistence-insinuate)) ;; Resume clocking task when emacs is restarted

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))

;; global Effort estimate values
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
;;        1    2    3    4    5    6    7    8    9    0
;; These are the hotkeys ^^

;; Set default column view headings: Task Priority Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

;; Show lot of clocking history so it's easy to pick items off the `C-c I` list
(setq org-clock-history-length 23)

(defun eos/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "C-c I") #'eos/org-clock-in)
(global-set-key (kbd "C-c O") #'org-clock-out)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports (already included)
;; (setq org-clock-report-include-clocking-task t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)

(setq org-use-effective-time t)

(defun my/org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
(setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(with-eval-after-load 'org
  ;; (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  ;; (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  ;; (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  ;;   (add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  ;; (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map))

(setq org-modules '(;;org-bbdb
                    ;;org-gnus
                    org-drill
                    ;;org-info
                    ;;org-jsinfo
                    ;;org-irc
                    ;;org-mouse
                    org-protocol
                    org-eww
                    ;;org-annotate-file
                    ;;org-eval
                    ;;org-expiry
                    ;;org-interactive-query
                    ;;org-man
                    ;;org-collector
                    ;;org-panel
                    ;;org-screen
                    ;;org-toc
                    org-habit
                    org-clock))
(eval-after-load 'org
  '(org-load-modules-maybe t))

(setq org-drill-add-random-noise-to-intervals-p t)

;;(setq org-habit-show-habits-only-for-today nil)
(require 'org-habit) ;; yiufung includes this line

(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 75
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)
;;(setq org-habit-show-done-always-green t)

(defvar my/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
(eval-after-load 'org
  '(defadvice org-agenda-finalize-entries (around sacha activate)
     (if my/org-agenda-limit-items
         (progn
           (setq list (mapcar 'org-agenda-highlight-todo list))
           (setq ad-return-value
                 (subseq list 0 my/org-agenda-limit-items))
           (when org-agenda-before-sorting-filter-function
             (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
           (setq ad-return-value
                 (mapconcat 'identity
                            (delq nil
                                  (subseq
                                   (sort list 'org-entries-lessp)
                                   0
                                   my/org-agenda-limit-items))
                            "\n")))
       ad-do-it)))

;; (setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

(defun my/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my/org-agenda-done)

(defun my/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'my/org-agenda-mark-done-and-add-followup)

(defun my/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
;;;;; (define-key org-agenda-mode-map "N" 'my/org-agenda-new)

(defun my/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
	     (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defun my/org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

(defvar my/org-last-refile-marker nil "Marker for last refile")
(defun my/org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((helm-org-headings-actions
         '(("Refile to this heading" . helm-org-heading-refile))))
    (save-excursion
      (helm-org-in-buffer-headings)
      (org-end-of-subtree t)
      (setq my/org-last-refile-marker (point-marker)))))

(defun my/org-refile-to-previous ()
  "Refile subtree to last position from `my/org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (save-excursion
      (let* ((marker my/org-last-refile-marker)
             (target-level
              (with-current-buffer (marker-buffer marker)
                (goto-char (marker-position marker))
                (org-current-level))))
        (helm-org-goto-marker marker)
        (org-end-of-subtree t t)
        (org-paste-subtree target-level)))))

(add-to-list 'org-speed-commands-user '("w" call-interactively 'my/org-refile-in-file))
(add-to-list 'org-speed-commands-user '("." call-interactively 'my/org-refile-to-previous))

(defun my/org-insert-defun (function)
  "Inserts an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if buffer-point        
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (insert "#+begin_src emacs-lisp :tangle yes\n" definition "#+end_src\n")))

(defun my/org-summarize-task-status ()
  "Count number of tasks by status.
Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))

(defun my/org-days-between (start end)
  "Number of days between START and END (exclusive).
  This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))

(run-with-idle-timer 30 t 'org-save-all-org-buffers)

;;  (setq org-agenda-span 2)
(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
;;  (setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;;  (setq org-agenda-time-grid
;;        '((daily today require-timed)
;;         "----------------"
;;         (800 1000 1200 1400 1600 1800)))
;; (setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS") ;; I don't have some of these

(defun my/org-agenda-project-agenda ()
  "Return the project headline and up to `my/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

(defun my/org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (when match
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher)))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

(defun my/org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELED\"" (if (org-before-first-heading-p) 'file 'tree)))

(add-to-list 'org-speed-commands-user '("a" call-interactively 'org-archive-subtree-default))

(defmacro my/org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

(defun my/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my/org-with-current-task
   (org-clock-in)
   (call-interactively 'my/org-quantified-track)
   (when (org-entry-get (point) "AUTO")
     (org-open-link-from-string (org-entry-get (point) "AUTO")))))
;;;;  (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

(defmacro my/with-org-task (&rest body)
  "Run BODY within the current agenda task, clocked task, or cursor task."
  `(cond
    ((derived-mode-p 'org-agenda-mode)
     (let* ((marker (org-get-at-bol 'org-marker))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char pos)
             ,@body)))))
    ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
    ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
    ((derived-mode-p 'org-mode) ,@body)))

(defun my/org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
  Default to the current task in the agenda, the currently-clocked
  entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (my/with-org-task
     (setq category (or category
                        (org-entry-get-with-inheritance "QUANTIFIED")))
     (cond
      ((null category)
       (setq category (read-string "Category: "))
       (org-set-property "QUANTIFIED" category))
      ((string= category "ask")
       (setq category (read-string "Category: "))))
     (setq note
           (concat
            (if (string= (or (org-entry-get-with-inheritance "QUANTIFIEDQUIET") "") "t")
                "!private "
              "")
            (or note (elt (org-heading-components) 4) (read-string "Note: "))))))
  (quantified-track (concat category " | " note)))

(defun my/org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
  If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (my/org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (my/org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (my/org-clock-in-and-track)))))
(bind-key "C-c q" 'my/org-quick-clock-in-task)

(require 'quantified nil t)

(defun my/compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
              (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                 (org-hh:mm-string-to-minutes estimated)))
    ""))

(defun my/org-send-to-bottom-of-list ()
  "Send the current line to the bottom of the list."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (save-excursion
      (kill-line 1)
      (org-end-of-item-list)
      (yank))))

(defun my/org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward my/weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
           "*Plans for next week*\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p 'any)
        (kill-new string)
      string)))

(use-package org-web-tools
  :ensure t)

(require 'org-wiki)
(setq org-wiki-location "/media/dgm/blue/documents/dropbox/notes/org-wiki")

(add-to-list 'load-path "/home/dgm/.emacs.d/src/org-recipes")
(require 'org-recipes)
(setq org-recipes-file-list '("/media/dgm/blue/documents/dropbox/notes/org-recipes/recipes.org" "/media/dgm/blue/documents/dropbox/notes/org-recipes/newPython.org" "/media/dgm/blue/documents/dropbox/notes/org-recipes/helloworld.org"))

(provide 'starter-kit-org)

(message "Starter Kit Org loaded.")
