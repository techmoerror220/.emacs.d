;; (use-package ess
;;  :ensure t
;;  :init (require 'ess-site))

(use-package ess
  :ensure t)

(use-package ess-smart-underscore
  :ensure t
  :after ess)

(use-package ess-view
  :ensure t
  :after ess)

(use-package ess-R-data-view
  :ensure t
  :after ess)

(require 'ob-stata)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (stata . t)
   (shell . t)
   (latex . t)
   ))

;; (sh . t)
;; (scheme . t)


;; I am following Ista Zahn here: don't include (stata . t) but do (require 'ob-stata) afterwards. Stil, when I do so, I get the Debugger entered--Lisp error: (void-variable inferior-STA-program-name)
;; eval(inferior-STA-program-name) so the problem comes from =ob-stata=.
;;(require 'ess)
;;(require 'ess-site)

(provide 'starter-kit-stats)

  (message "Starter Kit STATS file loaded.")
