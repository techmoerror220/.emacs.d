(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(setq mu4e-mu-binary (executable-find "/usr/bin/mu"))

(require 'mu4e)
(require 'org-mu4e)        ;;store org-mode links to messages
(require 'mu4e-contrib)

(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  ;; mu4e-update-interval 300  ; 300/60 = 5 minutes.
  ;; mu4e-headers-auto-update t
  ;; mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t)

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp")
;; Commented out by DGM on 4 august because I am trying to be able to choose where to send from.
;;        user-full-name "Daniel Guinea"
;;        user-mail-address "daniel.guinea.uned@gmail.com")

(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-view-scroll-to-next 'nil)

(setq message-kill-buffer-on-exit t)

(setq mu4e-attachment-dir  "~/Downloads")

(use-package visual-fill-column
  :ensure t)

(use-package org-mime
   :ensure t)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

  (defun my/mu4e-choose-signature ()
    "Insert one of a number of signatures"
    (interactive)
    (let ((message-signature
            (mu4e-read-option "Signature:"
              '(("formal" .
                (concat
             "Daniel Guinea\n"
             "Dept. Sociología I\nFacultad de CC.PP. y Sociología\nUniversidad Nacional de Educación a Distancia (UNED)\nCalle Obispo Trejo 2, Madrid 28040\nemail: daniel.guinea@poli.uned.es\nTel. +34 91 398 9441"))
                 ("informal" .
                "Daniel\n")))))
      (message-insert-signature)))

;; Now that I am using org mode somehow when composing emails, this keybind is already in use
(add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "C-c C-w") #'my/mu4e-choose-signature)))

  (setq mu4e-compose-signature-auto-include nil
        mu4e-compose-signature  "Daniel\n")

(setq mu4e-trash-folder nil ;; must be configured later by context
      mu4e-drafts-folder nil ;; must be configured later by context
      mu4e-sent-folder nil ;; must be configured later by context
      mu4e-compose-reply-to-address nil ;; must be configured later by context
      mu4e-compose-signature nil) ;; must be configured later by context

(setq mu4e-refile-folder "/archive")       ;; saved messages. Its location is relative to `mu4e-maildir'

(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-contexts
  `( ,(make-mu4e-context 
    :name "work" 
    :enter-func (lambda () (mu4e-message "Entering work context"))
        :leave-func (lambda () (mu4e-message "Leaving work context"))
    ;; we match based on the contact-fields of the message
    :match-func (lambda (msg)
          (when msg
            (mu4e-message-contact-field-matches msg
              :from "daniel.guinea.uned@gmail.com")))
    :vars '( ( user-mail-address . "daniel.guinea.uned@gmail.com")
         ( user-full-name . "Daniel Guinea")
         ( mu4e-compose-signature . 
           (concat 
             "Daniel Guinea\n Dept. Sociología I\nFacultad de CC.PP. y Sociología\nUniversidad Nacional de Educación a Distancia (UNED)\nCalle Obispo Trejo 2, Madrid\nemail: daniel.guinea@poli.uned.es\nTel. +34 91 398 9441\n"))
        (mu4e-compose-format-flowed . t)
        (mu4e-sent-folder . "/work/[work].Sent Mail")
        (mu4e-drafts-folder . "/work/[work].Drafts")
        (mu4e-trash-folder . "/work/[work].Trash")
        (mu4e-maildir-shortcuts . ( ("/work/INBOX"            . ?i)
                                    ("/work/[work].Sent Mail" . ?s)
                                    ("/work/[work].Drafts"    . ?d)
                                    ("/work/[work].Trash"     . ?t)
                                    ("/work/[work].Spam"      . ?b)))))
     ,(make-mu4e-context
    :name "personal"
    :enter-func (lambda () (mu4e-message "Switch to personal context"))
    ;; no :leave-func 
    ;; we match based on the maildir of the message
    ;; this matches maildir /personal and its sub-directories
    :match-func (lambda (msg)
          (when msg
			(string-match-p "^/personal" (mu4e-message-field msg :maildir))))
    :vars '( ( user-mail-address . "daniel.guinea.martin@gmail.com")
        (user-full-name . "Daniel")
        (mu4e-compose-signature .  "Daniel\n")
        (mu4e-sent-folder . "/personal/[personal].Sent Mail")
        (mu4e-drafts-folder . "/personal/[personal].Drafts")
        (mu4e-trash-folder . "/personal/[personal].Trash")
        (mu4e-compose-format-flowed . t)
        (mu4e-maildir-shortcuts . ( ("/personal/INBOX"                . ?i)
                                    ("/personal/[personal].Sent Mail" . ?s)
                                    ("/personal/[personal].Drafts"    . ?d)
                                    ("/personal/[personal].Trash"     . ?t)
                                    ("/personal/[personal].Spam"      . ?b)))))
     ))

  (setq mu4e-user-mail-address-list
    (delq nil
      (mapcar (lambda (context)
		(when (mu4e-context-vars context)
		  (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
	mu4e-contexts)))

;; Don't bother me with context on startup.
;; (setq mu4e-context-policy nil)

;; alternative from DGM on 4 august 2019, https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
;; start with the first (default) context;
(setq mu4e-context-policy 'pick-first) 
;; (setq mu4e-context-policy nil) 
;; (setq mu4e-context-policy 'ask-if-none) 
;; whether to compose with the current context if no context matched (nil option. In this case I say "ask always")
;; (setq mu4e-compose-context-policy 'always-ask)
;; (setq mu4e-compose-context-policy 'ask)
(setq mu4e-compose-context-policy nil)

;; (setq mu4e-user-mail-address-list (list "daniel.guinea.uned@gmail.com" "daniel.guinea.martin@gmail.com"))

;; (setq mail-user-agent 'mu4e-user-agent)

;; these are actually the defaults
;;    (setq
;;      ;; mu4e-maildir       "~/Maildir/work"      ;; top-level Maildir
;;      mu4e-sent-folder   "[work].Sent Mail"   ;; folder for sent messages
;;      mu4e-drafts-folder "[work].Drafts"      ;; unfinished messages
;;      mu4e-trash-folder  "[work].Trash")      ;; trashed messages

  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

   (setq gnus-dired-mail-mode 'mu4e-user-agent)
   (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq mu4e-attachment-dir "~/Downloads"
      mu4e-save-multiple-attachments-without-asking t)

(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Messages with attachments"
    :query "flag:attach"
    :key ?a) t)

(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Archived messages"
    :query "maildir:/archive" 
    :key ?r) t)

(setq org-capture-templates `(
                              ("a"  "Article"  entry  
                               (file+headline "/home/dgm/Dropbox/gtd/bibliography.org" "Bibliography") 
                                  "* %a %^g
                                  \n:PROPERTIES: 
                                  \n:Created: %U
                                  \n:END:
                                  \n%i
                                  \nBrief description:
                                  \n%?"  
                                :immediate-finish t 
                                :prepend t  
                                :empty-lines 0 
                                :created t)
                              ("j" "Journal" entry
                                 (file+datetree "/home/dgm/Dropbox/gtd/journal.org")
                                 "* %? \n Added on: %U")
                              ("l" "Life-related Idea" entry
                                (file+headline "~/Dropbox/gtd/notes.org" "Life-related Ideas")
                                 "* %?\nCaptured on %U from %a\n")
                              ("n" "Note" entry
                                (file+headline "~/Dropbox/gtd/notes.org" "Notes")
                                 "* %?\nCaptured on %U from %a\n")
                              ("p" "Project-related Idea" entry
                                (file+headline "~/Dropbox/gtd/notes.org" "Project-related Ideas")
                                 "* %?\nCaptured on %U from %a\n")
                              ("r" "Respond later" entry 
                                (file+headline "~/Dropbox/gtd/inbox.org" "Email")
                                 "* TODO Respond to %a, email by %:from \nEntry added on: %U \n"
                                 :empty-lines 0
                                 :immediate-finish t)
                              ("t" "Todo [inbox]" entry
                                (file+headline "/home/dgm/Dropbox/gtd/inbox.org" "Tasks")
                                 "* TODO %i%? \nEntry added on: %U from %a\n")
                              ("T" "Tickler" entry
                                (file+headline "/home/dgm/Dropbox/gtd/tickler.org" "Tickler")
                                 "* %i%? \nEntry added on: %U from %a\n")
                              ("u" "URLs to remember" entry
                                (file+headline  "/home/dgm/Dropbox/gtd/URLs.org" "URLs")
                                 ,(concat "* TODO Read this URL: '%:description'\nURL: %l\nDate:%U\n\n")
                                 :empty-lines 0
                                 :immediate-finish t)
                              ("w" "Capture web snippet" entry
                                (file+headline "~/Dropbox/gtd/notes.org" "Webs")
                                 ,(concat "* Web: '%:description'\n\nURL: %l\nTime:%U\n\nContents:\n\n %i\n")
                                 :empty-lines 0
                                 :immediate-finish t)))

(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
	(interactive)
	(setq mu4e-headers-fields
	      `((:human-date . 25) ;; alternatively, use :date
		(:flags . 6)
		(:from . 22)
        (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		(:size . 7)
        ))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

(setq 
 ;; mu4e-headers-date-format "%F %R" ;; already set
 mu4e-headers-time-format "%R"
 mu4e-hide-index-messages t)

(setq mu4e-headers-include-related 'nil)
;;(setq mu4e-headers-include-related t)

(set-face-foreground 'mu4e-unread-face "#8b8b00")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)

(when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

;; (setq mu4e-view-prefer-html t)  ;; trying this off as https://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html recommends

;; (setq mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true")

;;(setq mu4e-html2text-command 'mu4e-shr2text)
(setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")

;; If I use shr, it is convinient to make =shr/eww= readable with dark themes, i.e., if you're using a dark theme, and the messages are hard to read, it can help to change the luminosity, e.g.:
(setq shr-color-visible-luminance-min 80)

(defun jcs-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

;; Arrange to view messages in either the default browser or EWW
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-completing-read-function 'completing-read)

(setq mu4e-decryption-policy t)

(setq  mml-secure-openpgp-encrypt-to-self t)

(setq mu4e-split-view 'nil)

(global-set-key (kbd "C-*") #'mu4e)

(setq mu4e-confirm-quit nil)

(setq mu4e-compose-dont-reply-to-self t)

(add-hook 'message-mode-hook 'turn-on-orgtbl)
;; (add-hook 'message-mode-hook 'turn-on-orgstruct++) ;; gives error when composing

(defun ed/preview-some-mail-at (path)
  (interactive "fPath: ")
  (call-process
   "mu" nil
   (switch-to-buffer (generate-new-buffer "*mail preview*") t)
   t "view" (expand-file-name path))
  (with-current-buffer "*mail preview*"
    (goto-char (point-min))
    (mu4e~fontify-cited)
    (mu4e~fontify-signature)
    (while (re-search-forward "^\\(\\w+:\\) \\(.*\\)$" nil t)
      (let ((key (match-string 1))
            (value (match-string 2)))
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (insert (concat (propertize key 'face 'mu4e-header-key-face) " "))
        (if (or (string= key "From:")
                (string= key "To:"))
            (insert (propertize value 'face 'mu4e-special-header-value-face))
          (insert (propertize value 'face 'mu4e-header-value-face)))))
    (forward-line)
    (beginning-of-line)
    (insert "\n")
    (read-only-mode)
    (local-set-key (kbd "q") #'kill-this-buffer)))

(use-package helm-mu
  :ensure t)

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "m" 'helm-mu)))

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 0)
            (visual-fill-column-mode)
            (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
            (visual-line-mode)))


  ;; I want to see full From header, not only name
  (setq mu4e-view-show-addresses t)

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

(provide 'starter-kit-mu4e)

(message "Starter Kit User Mu4e File loaded.")
