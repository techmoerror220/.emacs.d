(setq ispell-local-dictionary-alist
      `((nil
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         t
         ("-d" "en_US")
         nil
         utf-8)
        ("american"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         t
         ("-d" "en_US")
         nil
         utf-8)
 	    ("castellano"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         t
         ("-d" "es_ES")
         nil
         utf-8)
	    ))

;; (setq ispell-program-name "hunspell")
;; It works!  It works!  After two hours of slogging, it works!
(if (file-exists-p "/usr/bin/hunspell")
    (progn
      (setq-default ispell-program-name "hunspell")
      (setq ispell-program-name         "hunspell"
            ispell-dictionary           "american"
            ispell-local-dictionary     "american")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(setq ispell-really-hunspell t)

(global-set-key (kbd "C-M-$") #'ispell-change-dictionary)

(provide 'starter-kit-aspell)

(message "Starter Kit Aspell loaded.")
