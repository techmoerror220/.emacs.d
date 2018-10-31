;;; Org mode
;;; Original customization under Mobile Settings // GTD settings heading
;;; in =dgm.org=
;;;   Sync orgmode files with Dropbox and iPhone.

;;; TODO: org-import should be able to parse "|" in CSV files.

; (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)  ;; dgm
; comments out

(setq
 ;; Disable line splitting on M-RET.
 org-M-RET-may-split-line '((default))
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 org-ellipsis " […]"
 org-adapt-indentation nil
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

  ;; Set to the location of your Org files on your local system
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

;;;;;;; commented out by dgm to try and get org-agenda windows in
;; current window
;; (defun ambrevar/org-switch-agenda-file (&optional other-window)
;;   "Switch between org-agenda and the first org-agenda-files."
;;   (interactive "P")
;;   (if (and buffer-file-name
;;            (member (expand-file-name buffer-file-name) (mapcar 'expand-file-name org-agenda-files)))
;;       (org-agenda)
;;     (let ((b (find-buffer-visiting (car org-agenda-files))))
;;       (if b
;;           (if (get-buffer-window b)
;;               (select-window (get-buffer-window b))
;;             (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) b))
;;         (funcall (if other-window 'find-file-other-window 'find-file) (car org-agenda-files))))))

;; (defun ambrevar/org-switch-agenda-file-other-window ()
;;   "Like `ambrevar/org-switch-agenda-file' but use other window if possible."
;;   (interactive)
;;   (ambrevar/org-switch-agenda-file t))

;; https://stackoverflow.com/questions/10635989/emacs-org-agenda-list-destroy-my-windows-splits

(setq org-agenda-window-setup 'current-window)

;; From Caolan at https://caolan.org/dotfiles/emacs.html#orgd96aeb0

;; Provide refile targets as paths, so a level 3 headline will be available as level1/level2/level3. Offer completions in hierarchical steps.
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

;; Create any missing parent nodes during refile (after asking for
;; confirmation). (From Caolan at
;; https://caolan.org/dotfiles/emacs.html#orgd96aeb0)
(setq org-refile-allow-creating-parent-nodes 'confirm)



  ;; Another way of doing the same
  ;;  '(org-refile-targets (quote (("/media/dgm/blue/documents/dropbox/gtd/gtd.org" :maxlevel . 3)
  ;;                              ("/media/dgm/blue/documents/dropbox/gtd/someday.org" :level . 1)
  ;;                              ("/media/dgm/blue/documents/dropbox/gtd/tickler.org" :maxlevel . 2))))


  ;; tip from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html for  using custom agenda commands  to get an overview of actions by context or tag. Here’s an example custom agenda command that will display all actions for the @office context. Following the GTD principle, what I usually want is to only show the first action to be done (or next action) for each project with the @office tag. That can be achieved using a skipping condition. I've tweak it so that I can have it work for entries tagged for the context "work" and for the context "home".

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

  ;; HOWEVER I am not 100% sure how useful is all this  because I have a nice option in C-c a to choose filtering by tag (/), etc.
  ;; anyways, more info on agenda-custom-commands in http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html


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



  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "/media/dgm/blue/documents/dropbox/org/fromMobile.org")

  ;; Finally, as Carsten Dominik says here: http://thread.gmane.org/gmane.emacs.orgmode/523, with the project list You may run into problems with this approach if you have set org-tags-match-list-sublevels to a non-nil value, because then, due to tag inheritance, every headline *inside* each project will also show up in theresulting list.  To work around this, you may define your own special command like this:

   (defun my-org-project-list ()
      "Get me a list of projects."
      (interactive)
      (let ((org-tags-match-list-sublevels nil))
      (org-tags-view nil "project")))

   ;; and guess what?? It works!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; more stuff from Ambrevar's init-org.el file
;;; Agendas.

;;; Set PDF association in Org-mode (original is 'default).
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

;;; Hooks.
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

;; mode for replacing stars with bullets
;; (when (require 'org-bullets nil t)
;;   (add-hook 'org-mode-hook 'org-bullets-mode))



;; Customization in: http://orgmode.org/manual/Capture-templates.html
;; Organization copied from: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html


;; Old code that worked alright but I've changed it to reflect the simpler code in: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

;;   (setq org-capture-templates
;;         '(("n" "Notes" entry (file+headline "/media/dgm/blue/documents/dropbox/org/notes.org" "Notes")
;;            "* Added on: %U\nDescription: %i %?\nEntry created from this heading or email: %a")
;;           ("t" "Todo" entry (file+headline "/media/dgm/blue/documents/dropbox/gtd/inbox.org" "Tasks")
;;            "* TODO %?\nDescription: %i\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\nEntry added on: %T\nEntry created from this heading or email: %a")
;;           ("j" "Journal" entry (file+datetree "/media/dgm/blue/documents/dropbox/org/journal.org")
;;            "* %?\nAdded on: %U\n  %i\n  %a")))

;; Old code for Tickler for when I didn't know that i could schedule with <C-c C-s>

;;                                 ("T" "Tickler" entry
;;                                  (file+headline "/media/dgm/blue/documents/dropbox/gtd/tickler.org" "Tickler")
;;                                  "* %i%?
;;                                   \nScheduled: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
;;                                   \nDeadline:  %(org-insert-time-stamp (org-read-date nil t \"+0d\"))
;;                                   \nEntry added on: %U
;;                                   \nEntry created from this heading or email: %a")))


(setq org-capture-templates '(
                              ;; ("t" "Todo [inbox]" entry
                              ;;   (file+headline "/home/dgm/Dropbox/gtd/inbox.org" "Tasks")
                              ;;   "* TODO %i%? \nEntry added on: %U
                              ;;                    \nEntry created from this heading or email: %a")
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


;; from caolan: https://caolan.org/dotfiles/emacs.html#orgd96aeb0

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

;; As Caolan (https://caolan.org/dotfiles/emacs.html#orgd96aeb0) says,
;; During expansion of the template, %a has been replaced by a link to the location from where you called the capture command. This can be extremely useful for deriving tasks from emails, for example. This tip from the Org-mode manual. The %U will be replaced with the time of the capture, this is an 'inactive' timestamp meaning it won't show up in the agenda view.

;; tip from https://lists.gnu.org/archive/html/emacs-orgmode/2007-08/msg00253.html
;; for having agenda show 30 days
;; (setq org-agenda-span (quote month))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public holidays in Spain. Tip from: https://www.emacswiki.org/emacs/CalendarLocalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


  (require 'org-agenda)
  (require 'holidays)
  (setq calendar-holidays holiday-other-holidays)
  (setq org-agenda-include-diary t)


;; Remove DONE tasks from agenda view. Tip from: https://stackoverflow.com/questions/8281604/remove-done-tasks-from-agenda-view
  (setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; Globally define tags. Tip from: https://orgmode.org/manual/Setting-tags.html
(setq org-tag-alist '(("airbnb" . ?a) ("@computer" . ?b) ("course" . ?c)  ("errands" . ?e) ("@home" . ?h) ("medicos" . ?m) ("@office" . ?o)  ("@phone" . ?p) ("project" . ?q) ("teaching" . ?t) ("uned" . ?u)))

;;; stuff from caolan: https://caolan.org/dotfiles/emacs.html#orgd96aeb0
;;; Mu4e
;; Store a link to a mu4e query or message, setting various properties for use in capture templates. Basic support is provided by 'org-mu4e, but this uses some code from Using org-capture-templates with mu4e to extend the properties available to templates.


(defun org-mu4e-store-link ()
  "Store a link to a mu4e query or message."
  (cond
    ;; storing links to queries
    ((eq major-mode 'mu4e-headers-mode)
     (let* ((query (mu4e-last-query))
             desc link)
       (org-store-link-props :type "mu4e" :query query)
       (setq link (concat "mu4e:query:" query))
       (org-add-link-props :link link :description link)
       link))
    ;; storing links to messages
    ((eq major-mode 'mu4e-view-mode)
     (let* ((msg (mu4e-message-at-point))
            (msgid (or (plist-get msg :message-id) "<none>"))
            (from (car (car (mu4e-message-field msg :from))))
            (to (car (car (mu4e-message-field msg :to))))
            (subject (mu4e-message-field msg :subject))
            link)
       (setq link (concat "mu4e:msgid:" msgid))
       (org-store-link-props
          :type "mu4e" :from from :to to :subject subject
          :message-id msgid)
       (org-add-link-props
          :link link
          :description (funcall org-mu4e-link-desc-func msg))
   link))))

(org-add-link-type "mu4e" 'org-mu4e-open)
(add-hook 'org-store-link-functions 'org-mu4e-store-link)


;; Respond later
;; The 'Respond later' template is a customised TODO which includes some extra email information. This relies on the extended email properties made available in the Org-mode -> Custom Links -> mu4e section of this config.

(push `("r" "Respond later" entry (file+headline "~/Dropbox/gtd/inbox.org" "Email")
        ,(string-join
          '("* TODO Respond to %:from on %a"
            "  %?"
            "  :LOGBOOK:"
            "  - Captured on %U from %a"
            "  :END:")
          "\n"))
        org-capture-templates)

;; Store interesting links... not working

;; (push `("l" "Link" entry (file+headline "~/Dropbox/gtd/inbox.org" "Link")
;;         ,(string-join
;;           '("* TODO Read this %:from on %a"
;;             "  %?"
;;             "  :LOGBOOK:"
;;             "  - Captured on %U from %a"
;;             "  :END:")
;;           "\n"))
;;         org-capture-templates)


;; Org-protocol from https://caolan.org/dotfiles/emacs.html#orgd96aeb0
;; Use org-protocol to trigger org-mode interactions from external programs. Useful for capturing links from Firefox using the org-mode-capture add-on.
(require 'org-protocol)


(provide 'init-org)
