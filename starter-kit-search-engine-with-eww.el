;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;;(setq browse-url-browser-function 'browse-url-generic
;;      browse-url-generic-program "chromium")

;;;; (setq browse-url-browser-function 'browse-url-generic
;;;;      browse-url-generic-program "eww")

;;;;;;;;;;;;;;;;;  has the code that worked prior to modification based on sacha chua's format
;;;;;;;;;;;;;;;;;  (setq browse-url-browser-function 'eww-browse-url)  ;; to make eww the default browser (see https://emacs.stackexchange.com/posts/7332/revisions)
;;(setq shr-external-browser 'chromium-browser)               ;; to make chromium default when I do =&= from within eww to open external browser
;;(setq eww-browse-with-external-browser 'shr-external-browser)
;;;;;;;;;;;;;;;;;  (setq browse-url-generic-program "chromium")

(setq browse-url-browser-function 'eww-browse-url)

;; (setq browse-url-browser-function #'eww-browse-url)

(defun my-eww-scale-adjust ()
  "Slightly bigger font but text shorter than text."
  (interactive)
  (text-scale-adjust 0)
  (text-scale-adjust 1)
  (eww-toggle-fonts)
  (split-window-right)
  (eww-toggle-fonts)
  (other-window 1)
  (sleep-for 1)
  (delete-window))

(use-package engine-mode
  :config (engine-mode t))

; The default keymap prefix for engine-mode is C-x /. If you'd like to bind the keymap to an additional prefix (say, C-c s), you totally can:

;; Todo: activate when something available
;; (engine/set-keymap-prefix (kbd "s-b"))

(defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
  :keybinding "a"
  :browser 'browse-url-chromium)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine google-maps
  "http://maps.google.com/maps?q=%s"
  :keybinding "m"
  :browser 'browse-url-chromium
  :docstring "Mappin' it up.")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w"
  :docstring "Searchin' the wikis.")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine wolfram-alpha
  "http://www.wolframalpha.com/input/?i=%s"
  :keybinding "z"
  :browser 'browse-url-chromium)

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "y"
  :browser 'browse-url-chromium)

(require 'json)

(defun my-get-stackoverflow-answers (query)
  (interactive "sQuestion: ")
  (let* ((question_ids
          (with-current-buffer
              (url-retrieve-synchronously
               (concat "https://google.com/search?ie=utf-8&oe=utf-8&hl=en&as_qdr=all&q="
                       (url-hexify-string (concat query " site:stackoverflow.com"))))
            (let (ids)
              (while (re-search-forward "https://stackoverflow.com/questions/\\([0-9]+\\)" nil t)
                (push (match-string-no-properties 1) ids))
              (setq ids (reverse ids))
              (if (> (length ids) 5)
                  (subseq ids 0 5)
                ids))))

         (url_template (format "https://api.stackexchange.com/2.2/questions/%s%%s?site=stackoverflow.com"
                               (string-join question_ids ";")))

         (questions (with-current-buffer                      
                        (url-retrieve-synchronously
                         (format url_template ""))
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (append (assoc-default 'items (json-read)) nil)))

         (answers (with-current-buffer
                      (url-retrieve-synchronously
                       (concat (format url_template "/answers")
                               "&order=desc&sort=activity&filter=withbody"))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (sort (append (assoc-default 'items (json-read)) nil)
                          (lambda (x y)
                            (> (assoc-default 'score x)
                               (assoc-default 'score y)))))))

    (switch-to-buffer "*stackexchange*")
    (erase-buffer)

    (dolist (question_id (mapcar 'string-to-number question_ids))
      (let ((question (some (lambda (question)
                              (if (equal (assoc-default 'question_id question)
                                         question_id)
                                  question))
                            questions)))
(insert "<hr><h2 style='background-color:paleturquoise'>Question: "
                (format "<a href='%s'>%s</a>"
                        (assoc-default 'link question)
                        (assoc-default 'title question))
                "</h2>"
                "\n"
                (mapconcat
                 'identity
                 (let ((rendered
                        (remove-if
                         'null
                         (mapcar (lambda (answer)
                                   (if (and (equal question_id
                                                   (assoc-default 'question_id answer))
                                            (>= (assoc-default 'score answer) 0))
                                       (concat "<hr><h2 style='background-color:"
                                               "#c1ffc1'>Answer - score: "
                                               (number-to-string (assoc-default 'score answer))
                                               "</h2>"
                                               (assoc-default 'body answer))))
                                 answers))))
                   (if (> (length rendered) 5)
                       (append (subseq rendered 0 5)
                               (list (format "<br><br><a href='%s'>%s</a>"
                                             (assoc-default 'link question)
                                             "More answers...")))
                     rendered))
                 "\n")
                )))
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "^M" nil t)
        (replace-match "")))))

;; install helm via packages and then:

(require 'helm-net)

(defun my-helm-stackoverflow-lookup ()
  (interactive)
  ;; set debug-on-error to swallow potential network errors
  ;; idea taken from: https://blog.johnregner.com/post/78877988910/fixing-helm-spotify#_=_
  (let ((debug-on-error t)
        (helm-google-suggest-actions '(("Stackoverflow" . my-get-stackoverflow-answers))))
    (helm-google-suggest)))

(use-package sos
  :ensure t)

(global-set-key (kbd "s-b") 'helm-surfraw)

(provide 'starter-kit-search-engine-with-eww.org)

  (message "Starter Kit Search Engine with EWW loaded.")
