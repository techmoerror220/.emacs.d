;;; DGM's mu4e customization

;; dgm's customizations of mu4e
;; Setting up the MU mail server
;; copied from http://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html#Gmail-configuration


;; commented out because in office PC it returns "wrong type argument", stringp nil
;;  (mu4e-update-index)

;; commented out because I get this message: error in process filter: run-hooks: Symbol’s function definition is void: mu4e~headers-do-auto-update
(add-hook 'mu4e-index-updated-hook 'mu4e~headers-do-auto-update)

(setq mu4e-user-mail-address-list (list "daniel.guinea.uned@gmail.com" "daniel.guinea.martin@gmail.com"))

(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

(when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

(setq mu4e-view-prefer-html t)

(setq mail-user-agent 'mu4e-user-agent)

  ;; ;; these are actually the defaults
    (setq
      mu4e-maildir       "~/Maildir"               ;; top-level Maildir
      mu4e-sent-folder   "/[Gmail].Enviados"       ;; folder for sent messages
      mu4e-drafts-folder "/[Gmail].Borradores"     ;; unfinished messages
      mu4e-trash-folder  "/[Gmail].Papelera"       ;; trashed messages
      mu4e-refile-folder "/[Gmail].Destacados")    ;; saved messages

  ;; Note, mu4e-maildir takes an actual filesystem-path, the other folder names are all relative to mu4e-maildir. Also note that this must not be a symbolic link.

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; don't move to next message when you reach the end of a message
  (setq mu4e-view-scroll-to-next 'nil)

  ;; save attachments in the Downloads folder
  (setq mu4e-attachment-dir  "~/Downloads")

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
      '( ("/INBOX"                     . ?i)
         ("/[Gmail].Enviados"          . ?s)
         ("/[Gmail].Papelera"          . ?t)
         ("/[Gmail].Todos"             . ?a)
         ("/personal/INBOX"            . ?k)
         ("/personal/[Gmail].Enviados" . ?x)
         ("/personal/[Gmail].Papelera" . ?f)))


  ;; something about ourselves
  ;; ;; general emacs mail settings; used when composing e-mail
  ;; ;; the non-mu4e-* stuff is inherited from emacs/message-mode

  ;; commented out on 23rd dic 2016 to test whether hook on replies works.
  ;; (setq mu4e-compose-reply-to-address "daniel.guinea.uned@gmail.com"
  ;;       user-mail-address "daniel.guinea.uned@gmail.com"
  ;;       user-full-name  "Daniel Guinea")
  ;; (setq mu4e-compose-signature
  ;;    "Daniel Guinea\nDept. Sociología I\nFacultad de CC.PP. y Sociología\nUNED\n")

  ;; sending mail -- replace USERNAME with your gmail username
  ;; (require 'smtpmail)
  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;     smtpmail-stream-type 'starttls
  ;;     smtpmail-default-smtp-server "smtp.gmail.com"
  ;;     smtpmail-smtp-server "smtp.gmail.com"
  ;;     smtpmail-smtp-service 587

      ;; sending mail with msmtp rather than smtp, copied from http://zmalltalker.com/linux/mu.html#
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        user-full-name "Daniel Guinea")

     ;; if you need offline mode, set these -- and create the queue dir
     ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
     ;; smtpmail-queue-mail  nil
  ;; smtpmail-queue-dir  "/home/user/Maildir/queue/cur")


  ;; Borrowed from http://ionrock.org/emacs-email-and-mu.html (and
  ;; included in http://zmalltalker.com/linux/mu.html#)
  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.
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
  ;; (add-to-list 'mu4e-bookmarks
  ;;              '("maildir:/Gmail/gitorious-ml flag:unread" "Unread on
  ;; the mailing list" ?m))

  ;; Wouldn't it be awesome to be able to send files from dired using your mail client?
  ;; I'll need a special version of the gnus-dired-mail-buffers function
  ;; so it understands mu4e buffers as well:
  ;; copied from http://zmalltalker.com/linux/mu.html#

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

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; copied from http://zmalltalker.com/linux/mu.html#
  ;; Use fancy chars
  ;; (setq mu4e-use-fancy-chars t)

  ;; When using ’fancy characters’ (mu4e-use-fancy-chars) with the Inconsolata-font (and likely others as well), the display may be slightly off; the reason for this issue is that Inconsolata does not contain the glyphs for the ’fancy’ arrows and the glyphs that are used as replacements are too high.

  ;; To fix this, you can use something like the following workaround (in your .emacs-file):

  ;; (if (equal window-system 'x)
  ;;    (progn
  ;;      (set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")
  ;;      (set-face-font 'default "Sans")))

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults: (set now in Ambrevar's init-mu4e.el)
;;  (setq mu4e-headers-fields
;;      '( (:human-date    .  25)    ;; alternatively, use :date
;;         (:flags         .   6)
;;         (:from          .  22)
;;         (:subject       .  nil))) ;; alternatively, use :thread-subject

  (setq mu4e-get-mail-command "offlineimap")   ;; or fetchmail, or ...
  ;; (setq mu4e-update-mail-and-index 600)        ;; update every 60*10 = 600 (10 minutes)
  ;; parece que da problemas esta última línea, así que la comento. Originalmente era:
  ;; mu4e-update-interval 600

  ;; tip from
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html#Compose-hooks
  ;; set the From:-address for a reply message based on the receiver of
  ;; the original

  ;; 1) messages to "daniel.guinea.uned@gmail.com should be replied with From:"daniel.guinea.uned@gmail.com
  ;; 2) messages to danie.guinea.martin@gmail.com should be replied with From:danie.guinea.martin@gmail.com
  ;; 3) all other mail should use From: daniel.guinea.uned@gmail.com
  ;;  http://zmalltalker.com/linux/mu.html# has an example of this code
  ;; When replying to an email I want to use the address I received this message to as the sender of the reply. This is fairly trivial:


;; dec 14, 2017: reemplazo esto por nuevo código encontrado en https://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html#Compose-hooks
;; porque la dirección en las nuevas composiciones no funciona


;; old code that worked except for new compositions.

;;  (add-hook 'mu4e-compose-pre-hook
;;            (defun my-set-from-address ()
;;              "Set the From address based on the To address of the original."
;;              (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
;;                (if msg
;;                    (setq user-mail-address
;;                          (cond
;;                           ((mu4e-message-contact-field-matches msg :to "daniel.guinea.uned@gmail.com")
;;                            "daniel.guinea.uned@gmail.com")
;;                           ((mu4e-message-contact-field-matches msg :to "daniel.guinea.martin@gmail.com")
;;                           "daniel.guinea.martin@gmail.com")
;;                           ;; ((mu4e-message-contact-field-matches msg :to "marius.mathiesen@gmail.com")
;;                           ;;  "zmalltalker@zmalltalker.com")
;;                           ;; ((mu4e-message-contact-field-matches msg :to "zmalltalker@zmalltalker.com")
;;                           ;;  "zmalltalker@zmalltalker.com")
;;                           (t "daniel.guinea.uned@gmail.com")))))))



;; 1) messages to me@foo.example.com should be replied with From:me@foo.example.com
;; 2) messages to me@bar.example.com should be replied with From:me@bar.example.com
;; 3) all other mail should use From:me@cuux.example.com
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


  ;; stop mu4e from inserting line breaks: http://emacs.stackexchange.com/questions/3061/how-to-stop-mu4e-from-inserting-line-breaks
  (defun no-auto-fill ()
    "Turn off auto-fill-mode."
    (auto-fill-mode -1))

  (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)

  ;; I want to see full From header, not only name

;;  (setq mu4e-view-show-addresses t)

  ;; set default signature to nil
  (setq mu4e-compose-signature-auto-include nil
        mu4e-compose-signature "")

  ;; tip from http://www.macs.hw.ac.uk/~rs46/posts/2014-11-16-mu4e-signatures.html
  ;; for inserting different signatures

  ;; try this solution by djcb: https://github.com/djcb/mu/issues/706
  ;; (defun insert-mu4e-sig-here ()
  ;;  "Insert the mu4e signature here, assuming it is a string."
  ;;  (interactive)
  ;;  (when (stringp mu4e-compose-signature)
  ;;    (mu4e-compose-signature)))

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

  ;; but this inserts the signature not where cursor is but at the very bottom of the message.



  ;; solution to multiple addresses and signatures
  ;; from: http://danielsz.github.io/2015/06/27/mu4e-account-switching

  ;; (cl-defstruct account full-name address signature)
  ;; (setq my-accounts (list
  ;;                    (make-account :full-name "Daniel Guinea"
  ;;                                  :address "daniel.guinea.uned@gmail.com"
  ;;                                  :signature "Daniel Guinea\n Dept. Sociología I\nFacultad de CC.PP. y Sociología\nUNED, Madrid\n")
  ;;                    (make-account :full-name "Daniel Guinea"
  ;;                                  :address "daniel.guinea.martin@gmail.com"
  ;;                                  :signature "Daniel")))
  ;;
  ;; (add-hook 'mu4e-compose-pre-hook
  ;;           (lambda ()
  ;;             "Set the From address based on the To address of the original."
  ;;             (let* ((msg mu4e-compose-parent-message)
  ;;                    (default-account (car my-accounts))
  ;;                    (current-account (if msg
  ;;                                         (loop for account in my-accounts
  ;;                                               when (mu4e-message-contact-field-matches msg :to (account-address account))
  ;;                                               return account)
  ;;                                       default-account)))
  ;;               (setq user-mail-address (account-address current-account)
  ;;                     user-full-name (account-full-name current-account)
  ;;                     mu4e-compose-signature (account-signature current-account)))))


  ;; Updating email takes ages because I have many Maildir folders, so I follow the advice here: https://www.djcbsoftware.nl/code/mu/mu4e/General.html
  (setq mu4e-cache-maildir-list t)

  ;; notes by https://github.com/djcb/mu/blob/master/mu4e/mu4e-utils.el
  ;; mu4e-cache-maildir-list "Whether to cache the list of maildirs; set it to t if you find
  ;; that generating the list on the fly is too slow. If you do, you
  ;; can set `mu4e-maildir-list' to nil to force regenerating the
  ;; cache the next time `mu4e-get-maildirs' gets called.")

     (setq mu4e-maildir-list nil)   ;;  "Cached list of maildirs."


;; From https://github.com/djcb/mu/blob/master/NEWS.org, indexing
;;
;;    (1) Allow for indexing in the background; see `mu4e-index-update-in-background`.
;;    (2) Better handle mbsync output in the update buffer
;;    (3) Add variables mu4e-index-cleanup and mu4e-index-lazy to enable lazy checking from mu4e; you can sit from mu4e using something like:

(setq
  mu4e-index-cleanup nil      ;; don't do a full cleanup check
  mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

;;; mu4e by Ambrevar

;;; REVIEW: Reply to all by default.
;;; https://github.com/djcb/mu/issues/1135
;;; TODO: Is it possible to mbsync without attachments?
;;; REVIEW: Do not cite when replying: https://github.com/djcb/mu/issues/1110.
;;; TODO: Face of `message-cited-text' does not work.
;;; REVIEW: Handle attachments in attached e-mails.
;;; See https://github.com/djcb/mu/issues/454#issuecomment-320616279.
;;; TODO: <tab> should go to next link in text e-mails too.

;; ;; We need 'main' to setup pinentry-emacs for GPG.
;; (add-to-list 'load-path "~/.emacs.d/src/ambrevar/")
;; (require 'main)

;; (when (require 'mu4e-maildirs-extension nil t)
;;   (mu4e-maildirs-extension))

(defun ambrevar/mu4e-headers ()
  "Like `mu4e' but show the header view.
Default to unread messages if the header buffer does not already exist."
  (interactive)
  (mu4e~start)
  (if (get-buffer "*mu4e-headers*" )
      (switch-to-buffer "*mu4e-headers*")
    (mu4e-headers-search "flag:unread AND NOT flag:trashed")))

(setq
 ;; Attachments
 ;; mu4e-attachment-dir "~/temp"
 mu4e-save-multiple-attachments-without-asking t

 ;; IMAP sync.
 ;; mu4e-maildir "~/.cache/mail"       ;; commmented out by dgm
 ;; mu4e-get-mail-command "mbsync -a"  ;; commented out by dgm
 ;; mu4e-update-interval 90            ;;
 mu4e-headers-auto-update nil        ; Don't refresh so that we don't lose the current filter upon, e.g. reading e-mails.
 ;; mu4e-change-filenames-when-moving t ; Preferred for mbsync according to the man page.

 ;; SMTP
 ;;  message-send-mail-function 'smtpmail-send-it ;; commented out by dgm

 ;; Don't bother me with context on startup.
 mu4e-context-policy nil

 ;; Don't keep sent e-mail buffer.
 ;; message-kill-buffer-on-exit t

 ;; For reporting bugs, "C-x m", etc.
 ;; mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t

  ;; Display
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

  ;; If you're using a dark theme, and the messages are hard to read, it
 ;; can help to change the luminosity, e.g.:
 shr-color-visible-luminance-min 80

 ;; Gmail-style threading.
 mu4e-headers-include-related t

 ;; Gmail likes format=flowed(?)
 ;; mu4e-compose-format-flowed

  ;; Also crypt to self so that we can read sent e-mails.
 mml-secure-openpgp-encrypt-to-self t

 ;; Because default completion can be extended (e.g. Helm, Ivy).
 mu4e-completing-read-function 'completing-read)

;;; Press "aV" to view in browser.
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Unicode chars for decoration might cause issues with some fonts or in terminals.
;;; https://github.com/djcb/mu/issues/733
;;; https://github.com/djcb/mu/issues/1062
;; (setq mu4e-use-fancy-chars t)

;;; REVIEW: Sorting in ascending order is impeded by
;;; `mu4e-search-results-limit': the 500 oldest e-mails will be displayed first.
;;; https://github.com/djcb/mu/issues/809
;; (setq mu4e-headers-sort-direction 'ascending)
;;; Since we sort in ascending direction, we default to the end of buffer.
;; (add-hook 'mu4e-headers-found-hook 'end-of-buffer)

;; (defvar ambrevar/mu4e-compose-fortune-p nil
;;   "Whether or not to include a fortune in the signature.")
;; (defun ambrevar/mu4e-add-fortune-signature ()
;;   (require 'functions) ; For `call-process-to-string'.
;;   (setq mu4e-compose-signature
;;         (concat
;;          user-full-name
;;          "\n"
;;          "https://ambrevar.xyz/"
;;          (when (and ambrevar/mu4e-compose-fortune-p   ;; this function
;;                     ;; is not defined here
;;                     (executable-find "fortune"))
;;            (concat "\n\n"
;;                    (ambrevar/call-process-to-string "fortune" "-s"))))))
;; (add-hook 'mu4e-compose-pre-hook 'ambrevar/mu4e-add-fortune-signature)

;; (defun ambrevar/mu4e-select-dictionary ()
;;   "Set dictionary according to the LANGUAGE property of the first
;; \"To:\" recipient found in the Org contacts file."
;;   (interactive)
;;   (let ((addresses (mapcar 'cadr (ambrevar/message-fetch-addresses)))
;;         address-lang-map)
;;     (setq address-lang-map
;;           (cl-loop for contact in (org-contacts-filter)
;;                    ;; The contact name is always the car of the assoc-list
;;                    ;; returned by `org-contacts-filter'.
;;                    for language = (cdr (assoc-string "LANGUAGE" (nth 2 contact)))
;;                    ;; Build the list of the user email addresses.
;;                    for email-list = (org-contacts-split-property
;;                                      (or (cdr (assoc-string org-contacts-email-property
;;                                                             (nth 2 contact))) ""))
;;                    if (and email-list language)
;;                    ;; Build an alist of (EMAIL . LANGUAGE).
;;                    nconc (cl-loop for email in email-list
;;                                   collect (cons email language))))
;;     (while addresses
;;       (if (not (assoc (car addresses) address-lang-map))
;;           (setq addresses (cdr addresses))
;;         (ispell-change-dictionary (cdr (assoc (car addresses) address-lang-map)))
;;         (setq addresses nil)))))
;; (add-hook 'mu4e-compose-pre-hook 'ambrevar/mu4e-select-dictionary)
;; (add-hook 'mu4e-conversation-hook 'ambrevar/mu4e-select-dictionary)

;;; Make some e-mails stand out a bit.
(set-face-foreground 'mu4e-unread-face "#8b8b00")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

;;; Confirmation on every mark execution is too slow to my taste.
(defun ambrevar/mu4e-mark-execute-all-no-confirm ()
  (interactive)
  (mu4e-mark-execute-all t))
(define-key mu4e-headers-mode-map "x" 'ambrevar/mu4e-mark-execute-all-no-confirm)

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "s" 'helm-mu)))

;;;;;;;;;;;;;;;;;;;; DGM: enabling the following produces Message:
;; mml-secure-epg-sign: GPG error: "Sign failed", "Exit"
;; and the sending of the email is aborted
;; (defvar ambrevar/mu4e-compose-signed-p t)
;; (defvar ambrevar/mu4e-compose-signed-and-crypted-p nil)
;; (defun ambrevar/mu4e-compose-maybe-signed-and-crypted ()
;;   "Maybe sign or encrypt+sign message.
;; Message is signed or encrypted+signed when replying to a signed or encrypted
;; message, respectively.

;; Alternatively, message is signed or encrypted+signed if
;; `ambrevar/mu4e-compose-signed-p' or `ambrevar/mu4e-compose-signed-and-crypted-p' is
;; non-nil, respectively.

;; This function is suitable for `mu4e-compose-mode-hook'."
;;   (let ((msg mu4e-compose-parent-message))
;;     (cond
;;      ((or ambrevar/mu4e-compose-signed-and-crypted-p
;;           (and msg (member 'encrypted (mu4e-message-field msg :flags))))
;;       (mml-secure-message-sign-encrypt))
;;      ((or ambrevar/mu4e-compose-signed-p
;;           (and msg (member 'signed (mu4e-message-field msg :flags))))
;;       (mml-secure-message-sign-pgpmime)))))
;; (add-hook 'mu4e-compose-mode-hook 'ambrevar/mu4e-compose-maybe-signed-and-crypted)

(defun ambrevar/message-fetch-addresses ()
  "Return a list of (NAME EMAIL) from the message header.
The \"From\", \"To\", \"Cc\" and \"Bcc\" fields are looked up.
Addresses in `mu4e-user-mail-address-list' are filtered out.
Duplicates are removed."
  (require 'cl)
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

;; (defun ambrevar/message-send-maybe-crypted ()
;;   "Crypt message if all recipients have a trusted key.
;; This will prompt the user if only some recipients have a suitable public key.
;; Suitable for `message-send-hook'."
;;   (let ((recipients (mapcar 'cadr (ambrevar/message-fetch-addresses)))
;;         valid-addresses untrusted-recipients)
;;     (dolist (key (epg-list-keys (epg-make-context epa-protocol)))
;;       (dolist (user-id (epg-key-user-id-list key))
;;         (when (memq (epg-user-id-validity user-id) '(marginal full ultimate))
;;           (push (cadr (mail-extract-address-components (epg-user-id-string user-id))) valid-addresses))))
;;     (setq untrusted-recipients
;;           (seq-difference recipients valid-addresses))
;;     (when (/= (length untrusted-recipients)
;;               (length recipients))
;;       ;; Some recipients have valid keys.
;;       (mml-secure-message-sign-encrypt)
;;       (when (and untrusted-recipients
;;                  (yes-or-no-p
;;                   (format "Some recipients don't have a trusted key %S.
;; Sending unencrypted? "
;;                           untrusted-recipients)))
;;         (mml-secure-message-sign)
;;         (mu4e-message "Sending unencrypted"))))
;;   t)
;; (add-hook 'message-send-hook 'ambrevar/message-send-maybe-crypted)

;; Because it's to tempting to send an e-mail riddled with typos... Yes,
;; but too slow
;; (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;;; Org capture
(when (require 'org-mu4e nil t)
  (dolist (map (list mu4e-view-mode-map mu4e-headers-mode-map))
    ;; Org mode has "C-c C-t" for 'org-todo.
    (define-key map (kbd "C-c C-t") 'org-mu4e-store-and-capture))
  (setq org-mu4e-link-query-in-headers-mode nil))

;;; Gmail trash fix.
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

;;;;;;;;;;;;;;;;;;;;  (load "~/personal/mail/mu4e.el" t)  ;;;; dgm
;;;;;;;;;;;;;;;;;;;;  commmented this out

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

;;; Org captures
;;; commented by dgm... I don't see what it adds to my setup
;; (when (require 'org-mu4e nil t)
;;   (require 'init-org)                   ; For org-agenda-files... dgm
;; ;;  comments this out because I have my org-agenda set up done in dgm,
;;   (add-to-list 'org-capture-templates
;;                `("e" "Mark e-mail in agenda" entry (file+headline ,(car org-agenda-files) "E-mails")
;;                  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))

  ;; TODO: Don't duplicate contacts.
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


;; make tree view the default
;; tip in "mu4e-conversation: Single buffer full-thread display to make
;; e-mails great again" on Reddit

(setq mu4e-conversation-print-function 'mu4e-conversation-print-tree)


(provide 'init-mu4e)
