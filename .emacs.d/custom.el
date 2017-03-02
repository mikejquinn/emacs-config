; Emac's "customize" system is a way of declaring settings which can be edited via a UI. Some plugins use
; this, like elscreen. See here for more:
; http://ergoemacs.org/emacs/emacs_custom_system.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(custom-safe-themes
   (quote
    ("ecb9fe1d5b165a35499191a909b2b5710a52935614058b327a39bfbbb07c7dc8" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "cf205b711e61963020e2d1561e87cdbe7727679b58af25dcabfe5073572b16f0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fdae9182be8e27809fd10af95941f13d1ca1324d2595db29f1d9f1ab14cbe3f0" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/tasks.org")))
 '(package-selected-packages
   (quote
    (tango-plus-theme twilight-bright-theme gandalf-theme evil-org workgroups2 deft csharp-mode undo-tree lua-mode yasnippet projectile org markdown-mode go-mode evil zoom-frm yaml-mode wrap-region with-editor tangotango-theme sublime-themes spinner solarized-theme smartparens seq scss-mode rust-mode rainbow-delimiters queue protobuf-mode outline-magic less-css-mode ido-vertical-mode ido-ubiquitous ham-mode golint go-stacktracer go-snippets go-projectile flx-ido f evil-surround evil-nerd-commenter evil-leader escreen dired-details+ diminish company color-identifiers-mode coffee-mode autopair auto-complete ag ace-jump-mode)))
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (emacs-lisp-docstring-fill-column . 75)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cider-error-highlight-face ((t (:inherit nil :underline "brown4"))))
 '(mu4e-cited-1-face ((t (:inherit font-lock-builtin-face :weight normal))) t)
 '(mu4e-cited-2-face ((t (:inherit font-lock-type-face :weight normal))) t)
 '(mu4e-cited-3-face ((t (:inherit font-lock-variable-name-face :weight normal))) t)
 '(mu4e-cited-4-face ((t (:inherit font-lock-keyword-face :weight normal))) t)
 '(mu4e-cited-5-face ((t (:inherit font-lock-comment-face :weight normal))) t)
 '(mu4e-cited-6-face ((t (:inherit font-lock-comment-delimiter-face :weight normal))) t)
 '(mu4e-cited-7-face ((t (:inherit font-lock-preprocessor-face :weight normal))) t)
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#3A8CD7"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#44D7BC"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#C9D736"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#D79841"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#D7604A"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#D777A9"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#5D7CD7"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "#FF0000" :foreground "#FFFFFF"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "#FF0000" :foreground "#FFFFFF")))))
