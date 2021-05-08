(require 'solarized)
(deftheme solarized-wombat-dark "The solarized-wombat-dark colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'solarized-wombat-dark
  '((base03 . "#2a2a29")
    (base02 . "#2f2f2e")
    (base01 . "#6a6a65")
    (base00 . "#74736f")
    (base0 . "#8d8b86")
    (base1 . "#999891")
    (base2 . "#e8e5db")
    (base3 . "#f6f3e8")
    (yellow . "#e5c06d")
    (orange . "#ddaa6f")
    (red . "#ffb4ac")
    (magenta . "#e5786d")
    (violet . "#834c98")
    (blue . "#a4b5e6")
    (cyan . "#7ec98f")
    (green . "#8ac6f2")
    (yellow-d . "#4c4536")
    (yellow-l . "#f4e9cf")
    (orange-d . "#4b4136")
    (orange-l . "#f3e4cf")
    (red-d . "#504341")
    (red-l . "#f9e7dc")
    (magenta-d . "#4d3936")
    (magenta-l . "#f6dbce")
    (violet-d . "#3b313d")
    (violet-l . "#dfd0d8")
    (blue-d . "#41434a")
    (blue-l . "#e6e6e8")
    (cyan-d . "#3b473c")
    (cyan-l . "#dfebd6")
    (green-d . "#3d464c")
    (green-l . "#e3eaea")
    (yellow-1bg . "#433e33")
    (orange-1bg . "#423b33")
    (red-1bg . "#463d3b")
    (magenta-1bg . "#443633")
    (blue-1bg . "#3b3d42")
    (cyan-1bg . "#363f37")
    (green-1bg . "#393f43")
    (violet-1bg . "#373038")
    (yellow-1fg . "#eccf92")
    (orange-1fg . "#e7bf92")
    (red-1fg . "#fec7be")
    (magenta-1fg . "#ee9e90")
    (violet-1fg . "#a67db0")
    (blue-1fg . "#bec7e7")
    (cyan-1fg . "#a4d6a9")
    (green-1fg . "#aed3ef")
    (yellow-2bg . "#706144")
    (orange-2bg . "#6d5a44")
    (red-2bg . "#785d5a")
    (magenta-2bg . "#714943")
    (violet-2bg . "#4d3853")
    (blue-2bg . "#585d6e")
    (cyan-2bg . "#4b654f")
    (green-2bg . "#516472")
    (yellow-2fg . "#efd7a4")
    (orange-2fg . "#ebcaa4")
    (red-2fg . "#fdd1c7")
    (magenta-2fg . "#f2b0a2")
    (violet-2fg . "#b895bc")
    (blue-2fg . "#cad0e7")
    (cyan-2fg . "#b6dcb6")
    (green-2fg . "#bfdaee"))
  '((custom-theme-set-faces theme-name
			    `(default
			       ((,class
				 (:foreground ,(solarized-color-blend base03 base3 0.15 2)
					      :background ,base03))))
			    `(highlight
			      ((,class
				(:background ,violet))))
			    `(font-lock-builtin-face
			      ((,class
				(:foreground ,magenta))))
			    `(font-lock-constant-face
			      ((,class
				(:foreground ,blue))))
			    `(font-lock-comment-face
			      ((,class
				(:foreground ,base00))))
			    `(mode-line
			      ((,class
				(:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
			    `(mode-line-inactive
			      ((,class
				(:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
			    `(mode-line-buffer-id
			      ((,class
				(:foreground ,base3 :weight bold))))
			    `(minibuffer-prompt
			      ((,class
				(:foreground ,base1))))
			    `(vertical-border
			      ((,class
				(:foreground ,base03)))))))
(provide-theme 'solarized-wombat-dark)
(provide 'solarized-wombat-dark-theme)
