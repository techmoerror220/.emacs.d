(require 'flyspell)
  (setq ispell-program-name "aspell"
        ispell-dictionary "english"
        ispell-dictionary-alist
        (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                         ("-B" "-d" "english")
                         nil iso-8859-1)))
          `((nil ,@default)
            ("english" ,@default))))
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (setq flyspell-issue-message-flag nil)
  
 
;; More efforts to speed up flyspell
;; See http://www.brool.com/index.php/speeding-up-flyspell-region 
  (defadvice flyspell-region (around fast-flyspell-region)
    (cl-flet ( (sit-for (x) t) ) 
      ad-do-it))
  (ad-activate 'flyspell-region)

(message "Starter Kit Aspell loaded.")
