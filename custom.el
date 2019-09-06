(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
 ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(atomic-chrome-url-major-mode-alist
 '(("reddit\\.com" . markdown-mode)
   ("uned\\.es" . text-mode)))
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
 (--map
  (solarized-color-blend it "#002b36" 0.25)
  '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
 '(("#073642" . 0)
   ("#546E00" . 20)
   ("#00736F" . 30)
   ("#00629D" . 50)
   ("#7B6000" . 60)
   ("#8B2C02" . 70)
   ("#93115C" . 85)
   ("#073642" . 100)))
 '(hl-bg-colors
 '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
 '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
 '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
 '(org-wiki helm-ack ivy-yasnippet ivy-yasnippe worf moody minions cheatsheet helm-org-rifle sos emacs-sos page-break-lines stripe-buffer helm-mu twittering-mode mu4e-conversation hl-sexp org-eww org-drill try ipython dired-narrow typing org-fstree wttrin edit-list guide-key miniedit atomic-chrome vi-tilde-fringe ztree zenburn-theme yasnippet-classic-snippets yaml-mode wordnut whole-line-or-region wgrep-helm volatile-highlights visible-mark unicode-fonts undo-tree typit transpose-frame textmate tango-2-theme symon switch-window sudo-edit spacemacs-theme spaceline soothe-theme smooth-scrolling smex smartscan slime-company shell-pop recentf-ext rebox2 rainbow-mode rainbow-delimiters r-autoyas python-x py-autopep8 popup-kill-ring pass ox-twbs ox-tufte ox-pandoc ox-gfm org-ac ob-ipython nyan-mode monokai-theme moe-theme maxframe material-theme mark-multiple linum-relative latex-pretty-symbols labburn-theme kurecolor jedi interleave inf-ruby ido-vertical-mode idle-highlight-mode ibuffer-vc hungry-delete highlight-symbol highlight-numbers helm-swoop helm-gtags helm-exwm helm-ag helm-R hc-zenburn-theme gruvbox-theme golden-ratio free-keys flycheck-tip flx-ido eyebrowse eval-in-repl emmet-mode eink-theme ein edit-server ebib duplicate-thing discover-my-major diminish dashboard darktooth-theme darkokai-theme csv-mode company-web company-statistics company-shell company-quickhelp company-math company-jedi company-auctex color-theme-solarized color-theme-sanityinc-tomorrow color-theme-modern clues-theme clean-aindent-mode browse-kill-ring anti-zenburn-theme anaphora ag adaptive-wrap ace-jump-mode))
 '(pdf-misc-print-programm "/usr/bin/gtklp")
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
 '((20 . "#dc322f")
   (40 . "#c8805d801780")
   (60 . "#bec073400bc0")
   (80 . "#b58900")
   (100 . "#a5008e550000")
   (120 . "#9d0091000000")
   (140 . "#950093aa0000")
   (160 . "#8d0096550000")
   (180 . "#859900")
   (200 . "#66aa9baa32aa")
   (220 . "#57809d004c00")
   (240 . "#48559e556555")
   (260 . "#392a9faa7eaa")
   (280 . "#2aa198")
   (300 . "#28669833af33")
   (320 . "#279993ccbacc")
   (340 . "#26cc8f66c666")
   (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
 '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
 ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
 ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "#4682b4" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue")))))
