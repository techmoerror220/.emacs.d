(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(setq mu4e-mu-binary (executable-find "/usr/bin/mu"))

(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)

(defun ambrevar/mu4e-headers ()
  "Like `mu4e' but show the header view.
Default to unread messages if the header buffer does not already exist."
  (interactive)
  (mu4e~start)
  (if (get-buffer "*mu4e-headers*" )
      (switch-to-buffer "*mu4e-headers*")
    (mu4e-headers-search "flag:unread AND NOT flag:trashed")))

(with-eval-after-load 'mu4e
  ;; mu4e-conversation must be enabled here.
  ;; REVIEW: https://github.com/djcb/mu/issues/1258
  (when (require 'mu4e-conversation nil t)
    (global-mu4e-conversation-mode)
    (add-hook
     'mu4e-conversation-after-send-hook
     (lambda ()
       (let ((mu4e-get-mail-command "offlineimap"))
         (mu4e-update-mail-and-index 'run-in-background))))
    (add-hook 'mu4e-view-mode-hook 'auto-fill-mode)))
(autoload 'ambrevar/mu4e-headers "mu4e")

(setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)

(setq  mu4e-headers-auto-update nil)
;; (add-hook 'mu4e-index-updated-hook 'mu4e-headers-do-auto-update) ;; updated it says...

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "s" 'helm-mu)))

(when (require 'org-mu4e nil t)
  (dolist (map (list mu4e-view-mode-map mu4e-headers-mode-map))
    ;; Org mode has "C-c C-t" for 'org-todo.
    (define-key map (kbd "C-c C-t") 'org-mu4e-store-and-capture))
  (setq org-mu4e-link-query-in-headers-mode nil))

(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

(when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

(setq mu4e-view-prefer-html t)

;; (setq mu4e-html2text-command 'mu4e-shr2text)
(setq mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true")

;; Don't bother me with context on startup.
(setq mu4e-context-policy nil)

(setq mu4e-user-mail-address-list (list "daniel.guinea.uned@gmail.com" "daniel.guinea.martin@gmail.com"))

(setq mail-user-agent 'mu4e-user-agent)

;; these are actually the defaults
    (setq
      mu4e-maildir       "~/Maildir"               ;; top-level Maildir
      mu4e-sent-folder   "/[Gmail].Enviados"       ;; folder for sent messages
      mu4e-drafts-folder "/[Gmail].Borradores"     ;; unfinished messages
      mu4e-trash-folder  "/[Gmail].Papelera"       ;; trashed messages
      mu4e-refile-folder "/[Gmail].Destacados")    ;; saved messages

(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-view-scroll-to-next 'nil)

(setq message-kill-buffer-on-exit t)

(setq message-kill-buffer-on-exit t)

(setq mu4e-attachment-dir  "~/Downloads")

(defun ambrevar/mu4e-mark-execute-all-no-confirm ()
  (interactive)
  (mu4e-mark-execute-all t))
(define-key mu4e-headers-mode-map "x" 'ambrevar/mu4e-mark-execute-all-no-confirm)

(setq mu4e-completing-read-function 'completing-read)

(setq 
 mu4e-headers-date-format "%F %R"
 mu4e-headers-fields '((:human-date . 16)
                       (:flags . 6)
                       (:size . 6)
                       (:mailing-list . 10)
                       (:from . 22)
                       (:subject))
 mu4e-headers-time-format "%R"
 mu4e-view-show-addresses t
 ;; mu4e-view-show-images t
 ;; mu4e-view-image-max-width 800
 mu4e-hide-index-messages t

 ;; Make =shr/eww= readable with dark themes, i.e., if you're using a dark theme, and the messages are hard to read, it
 ;; can help to change the luminosity, e.g.:
 shr-color-visible-luminance-min 80

 ;; Gmail-style threading.
 mu4e-headers-include-related t)

(set-face-foreground 'mu4e-unread-face "#8b8b00")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

(add-to-list 'mu4e-view-actions 
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-decryption-policy t)

(setq  mml-secure-openpgp-encrypt-to-self t)

  (setq mu4e-maildir-shortcuts
      '( ("/INBOX"                     . ?i)
         ("/[Gmail].Enviados"          . ?s)
         ("/[Gmail].Papelera"          . ?t)
         ("/[Gmail].Todos"             . ?a)
         ("/personal/INBOX"            . ?k)
         ("/personal/[Gmail].Enviados" . ?x)
         ("/personal/[Gmail].Papelera" . ?f)))

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        user-full-name "Daniel Guinea"
        user-mail-address "daniel.guinea.uned@gmail.com")

  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "daniel.guinea.uned@gmail.com" from) "work")
                 ((string-match "daniel.guinea.martin@gmail.com" from) "personal"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)
  (add-to-list 'mu4e-bookmarks
               '("maildir:/INBOX OR maildir:/personal/INBOX flag:unread" "Today's news" ?z))

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

(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-cache-maildir-list t)

(setq mu4e-maildir-list nil)

(setq
  mu4e-index-cleanup nil      ;; don't do a full cleanup check
  mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
        (setq user-mail-address
          (cond
            ((mu4e-message-contact-field-matches msg :to "daniel.guinea.uned@gmail.com")
              "daniel.guinea.uned@gmail.com")
            ((mu4e-message-contact-field-matches msg :to "daniel.guinea@poli.uned.es")
              "daniel.guinea.uned@gmail.com")
            ((mu4e-message-contact-field-matches msg :to "daniel.guinea.martin@gmail.com")
              "daniel.guinea.martin@gmail.com")
            (t "daniel.guinea.uned@gmail.com")))))))

(setq mu4e-compose-dont-reply-to-self t)

(use-package visual-fill-column
  :ensure t)

  (defun my-mu4e-choose-signature ()
    "Insert one of a number of signatures"
    (interactive)
    (let ((message-signature
            (mu4e-read-option "Signature:"
              '(("formal" .
                (concat
             "Daniel Guinea\n"
             "Dept. Sociología I\nFacultad de CC.PP. y Sociología\nUniversidad Nacional de Educación a Distancia (UNED)\nCalle Obispo Trejo 2, Madrid\nemail: daniel.guinea@poli.uned.es\nTel. +34 91 398 9441"))
                 ("informal" .
                "Daniel\n")))))
      (message-insert-signature)))

  (add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "C-c C-w") #'my-mu4e-choose-signature)))

  (setq mu4e-compose-signature-auto-include nil
        mu4e-compose-signature (concat
                                "Daniel Guinea\n"
                                "Dept. Sociología I\nFacultad de CC.PP. y Sociología\nUniversidad Nacional de Educación a Distancia (UNED)\nCalle Obispo Trejo 2, Madrid\nemail: daniel.guinea@poli.uned.es\nTel. +34 91 398 9441"))

(setq mu4e-attachment-dir "~/Downloads"
      mu4e-save-multiple-attachments-without-asking t)

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

(defun ambrevar/message-fetch-addresses ()
  "Return a list of (NAME EMAIL) from the message header.
The \"From\", \"To\", \"Cc\" and \"Bcc\" fields are looked up.
Addresses in `mu4e-user-mail-address-list' are filtered out.
Duplicates are removed."
;;  (require 'cl) ;; already in starter-kit.org
  (cl-delete-duplicates
   (seq-remove
    (lambda (contact) (member (cadr contact) mu4e-user-mail-address-list))
    (apply 'append
           (if (eq major-mode 'mu4e-compose-mode)
               (save-restriction
                 (message-narrow-to-headers)
                 (mapcar
                  (lambda (addr) (mail-extract-address-components (message-fetch-field addr) t))
                  (seq-filter 'message-fetch-field
                              '("From" "To" "Cc" "Bcc"))))
             (unless (buffer-live-p (mu4e-get-headers-buffer))
               (mu4e-error "no headers buffer connected"))
             (let ((msg (or (mu4e-message-at-point 'noerror)
                            (with-current-buffer (mu4e-get-headers-buffer)
                              ;; When loading messages, point might
                              ;; not be over a message yet.
                              (mu4e-message-at-point 'noerror)))))
               (when msg
                 (delq nil
                       (mapcar (lambda (field)
                                 ;; `mu4e-message-field' returns a list of (NAME . EMAIL).
                                 (mapcar (lambda (addr) (list (car addr) (cdr addr)))
                                         (mu4e-message-field msg field)))
                               '(:from :to :cc :bcc))))))))))

(defvar ambrevar/mu4e-move-to-trash-patterns nil
  "List of regexps to match for moving to trash instead of deleting them.
Matches are done against the :maildir field of the e-mail at
point.  See `ambrevar/mu4e-headers-move-to-trash' and
`ambrevar/mu4e-view-move-to-trash'.")

(defun ambrevar/mu4e-headers-move-to-trash ()
  (interactive)
  (let ((msg-dir (mu4e-message-field (mu4e-message-at-point) :maildir)))
    (if (not (seq-filter (lambda (re)
                           (string-match re msg-dir))
                         ambrevar/mu4e-move-to-trash-patterns))
        (mu4e-headers-mark-for-delete)
      (mu4e-mark-set 'move (funcall mu4e-trash-folder (mu4e-message-at-point)))
      (mu4e-headers-next))))

(defun ambrevar/mu4e-view-move-to-trash ()
  (interactive)
  (mu4e~view-in-headers-context
   (ambrevar/mu4e-headers-move-to-trash)
   (mu4e~headers-move (or n 1))))

;;; Don't display trashed messages in bookmarks.  This is useful for Gmail where
;;; the "delete" flag is not used.
(defvar ambrevar/mu4e-trash-folders nil
  "List of trash folders to filter out from bookmarks.")

;; Do this after setting `ambrevar/mu4e-trash-folders'.
(dolist (bookmark mu4e-bookmarks)
  ;; TODO: Why mu4e-bookmark-query does not work here?
  (setf (car bookmark) (concat  (mapconcat (lambda (s) (format "NOT maildir:\"%s\" and " s))
                                           ambrevar/mu4e-trash-folders "")
                                (car bookmark))))

(defun ambrevar/message-github ()
  "When replying to a github message, clean up all bogus recipients.
This function could be useful in `mu4e-compose-mode-hook'."
  (interactive)
  (let ((to (message-fetch-field "To")))
    (when (and to
               (string-match (rx "@reply.github.com" string-end) (cadr (mail-extract-address-components to))))
      (dolist (hdr '("To" "Cc" "Bcc"))
        (let ((addr (message-fetch-field hdr))
              recipients
              bogus-recipients
              clean-recipients)
          (when (stringp addr)
            (setq recipients (mail-extract-address-components addr t)
                  bogus-recipients (message-bogus-recipient-p addr))
            (when bogus-recipients
              (setq clean-recipients (seq-difference recipients bogus-recipients
                                                     (lambda (addrcomp addr)
                                                       (string= (cadr addrcomp) addr))))
              ;; See `message-simplify-recipients'.
              (message-replace-header
               hdr
               (mapconcat
                (lambda (addrcomp)
                  (if (and message-recipients-without-full-name
                           (string-match
                            (regexp-opt message-recipients-without-full-name)
                            (cadr addrcomp)))
                      (cadr addrcomp)
                    (if (car addrcomp)
                        (message-make-from (car addrcomp) (cadr addrcomp))
                      (cadr addrcomp))))
                clean-recipients
                ", "))))))
      (message-sort-headers)
      ;; Delete signature if any.
      (delete-region (save-excursion
                       (message-goto-signature)
                       (unless (eobp)
                         (forward-line -1))
                       (point))
                     (point-max))
      ;; Deleting trailing blank lines.
      (save-excursion
        (goto-char (point-max))
        (delete-blank-lines)
        (delete-blank-lines)))))
(add-hook 'mu4e-compose-mode-hook 'ambrevar/message-github)

  (defun ambrevar/mu4e-contact-dwim ()
    "Return a list of (NAME . ADDRESS).
If point has an `email' property, move it to the front of the list.
Addresses in `mu4e-user-mail-address-list' are skipped."
    (let ((result (ambrevar/message-fetch-addresses))
          (message org-store-link-plist))
      ;; Move contact at point to front.
      (let ((email-at-point (get-text-property (point) 'email))
            (contacts result))
        (when email-at-point
          (while contacts
            (if (not (string= (cadr (car contacts)) email-at-point))
                (setq contacts (cdr contacts))
              (setq result (delete (car contacts) result))
              (push (car contacts) result)
              (setq contacts nil)))))
      result))

  (defun ambrevar/org-contacts-template-name (&optional return-value)
    "Like `org-contacts-template-name' for mu4e."
    (or (car (car (ambrevar/mu4e-contact-dwim)))
        return-value
        "%^{Name}"))
  ;; commented by dgm until i learn how to use contacts with org
  ;; (defun ambrevar/org-contacts-template-email (&optional return-value)
  ;;   "Like `org-contacts-template-name' for mu4e."
  ;;   (or (cadr (car (ambrevar/mu4e-contact-dwim)))
  ;;       return-value
  ;;       (concat "%^{" org-contacts-email-property "}p")))
  ;; (add-to-list 'org-capture-templates
  ;;              `("c" "Add e-mail address to contacts" entry (file+headline ,(car org-contacts-files) "Contacts")
  ;;                "* %(ambrevar/org-contacts-template-name)
  ;; :PROPERTIES:
  ;; :EMAIL: %(ambrevar/org-contacts-template-email)
  ;; :END:")))

(defun ambrevar/mu4e-kill-ring-save-message-id (&optional msg)
  "Save MSG's \"message-id\" field to the kill-ring.
If MSG is nil, use message at point."
  (interactive)
  (kill-new (mu4e-message-field (or msg (mu4e-message-at-point)) :message-id)))

(setq mu4e-split-view 'nil)

(provide 'starter-kit-mu4e)

(message "Starter Kit User Mu4e File loaded.")
