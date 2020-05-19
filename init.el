;;
;; Package management
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add all directories under vendor to the load path.
(let ((basedir "~/.emacs.d/vendor/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
      (add-to-list 'load-path (concat basedir f)))))

(eval-when-compile (require 'use-package))

;; Some lisp helpers
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'lisp-helpers-personal)
(require 'emacs-utils)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.

;; Use the same PATH variable as your shell does. From http://clojure-doc.org/articles/tutorials/emacs.html
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
;; Prevent emacs from passing certain key combos (subh as M-h) directly to macOS
;; so we can choose our own bindings.
(setq mac-pass-command-to-system nil)
;; Save way more files in the recentf history list (the default is only 20).
(setq recentf-max-menu-items 300)
(setq recentf-max-saved-items 300)

;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Store ~ and # files outside of the working directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

;; Include path information in duplicate buffer names (e.g. a/foo.txt b/foo.txt)
(setq uniquify-buffer-name-style 'forward)

;; Start scrolling the window when the cursor reaches its edge.
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      ;; Make touchpad scrolling on OSX less jerky
      mouse-wheel-scroll-amount '(0.01))

;; The preference file for Emac's "Customize" system. `M-x customize` to access it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file t)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace-mode by default highlights all whitespace. Show only tabs and trailing spaces.
     (setq whitespace-style '(face trailing lines-tail))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
;; Some modes have their own tab-width variables which need to be overridden.
(setq-default css-indent-offset 2)
;; No indenting with tabs by default
(setq-default indent-tabs-mode nil)

(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(setq column-number-mode t) ; Show column number in the mode line
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
(global-hl-line-mode)

;;
;; Incremental search (isearch)
;;
;; Make highlighting during incremental search feel snappier.
(setq case-fold-search t) ; Make searches case insensitive.
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-max-at-a-time nil)
;; Hitting esacpe aborts the search, restoring your cursor to the original position, as it does in Vim.
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
;; Make C-h act the same as backspace, as it does in readline.
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; Make M-v paste the clipboard's text into the search ring.
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-w") 'isearch-del-word)

;; Enable the common Bash text-editing shortcuts in the minibuffer.
(define-key minibuffer-local-map (kbd "C-k") 'kill-line)
(define-key minibuffer-local-map (kbd "C-e") 'end-of-line)
(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-line)
(define-key minibuffer-local-map (kbd "C-d") 'delete-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

;; Mac OS X keybindings minor mode.
;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
(defvar mq/osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key mq/osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
(define-key mq/osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key mq/osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
; Commenting this out because currently I like to use M-h in org mode for
; org-promote-subtree, and I very rarely care about hiding emacs.
;(define-key mq/osx-keys-minor-mode-map (kbd "M-h") 'ns-do-hide-emacs)
(define-key mq/osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)
(define-key mq/osx-keys-minor-mode-map (kbd "M-n") 'new-frame)
(define-key mq/osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key mq/osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
;; (require 'zoom-frm)
(define-key mq/osx-keys-minor-mode-map (kbd "M--") 'text-scale-decrease)
(define-key mq/osx-keys-minor-mode-map (kbd "M-=") 'text-scale-increase)
(define-key mq/osx-keys-minor-mode-map (kbd "M-0") (lambda () (interactive) (text-scale-adjust 0)))

;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I want them to take
;; precedence over everything else.
;; Note that I have Ctrl-Space mapped to Alt, which makes these shortcuts easy to hit.
(define-key mq/osx-keys-minor-mode-map (kbd "A-e") 'switch-to-upper-left)
(define-key mq/osx-keys-minor-mode-map (kbd "A-d") 'switch-to-lower-left)
(define-key mq/osx-keys-minor-mode-map (kbd "A-r") 'switch-to-upper-right)
(define-key mq/osx-keys-minor-mode-map (kbd "A-f") 'switch-to-lower-right)
(define-key mq/osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key mq/osx-keys-minor-mode-map (kbd "M-w")
  (lambda ()
    (interactive)
    (when (not (one-window-p))
      (delete-window)
      (balance-windows)
      nil)))

(define-minor-mode mq/osx-keys-minor-mode
    "A minor-mode for emulating osx keyboard shortcuts."
    t " osx" mq/osx-keys-minor-mode-map)
  (mq/osx-keys-minor-mode t)

(use-package delight
  :ensure t
  :config
  (delight '((mq/osx-keys-minor-mode nil t)
             (undo-tree-mode nil undo-tree)
             (visual-line-mode nil t)
             (global-whitespace-mode nil t)
             (auto-fill-function nil t))))

(use-package which-key
  :ensure t
  :delight ""
  :init
  (setq which-key-allow-evil-operators t)
  :config
  (which-key-mode t))

;;
;; Evil mode
;;

(use-package evil
  :ensure t
  :init
  ;; When opening new lines, indent according to the previous line.
  (setq evil-auto-indent t)
  (setq evil-want-C-u-scroll t)
  (require 'undo-tree)
  :config
  (evil-mode t)
  )

(defun mq/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat evil-leader/leader " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix (cons name long-name))))

(use-package evil-leader
  :ensure t
  :after (evil)
  :config
  (evil-leader/set-leader ";")
  (evil-leader/set-key
		"TAB" 'mode-line-other-buffer
    "bb" 'helm-mini
    "bd" 'kill-this-buffer
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fj" 'projectile-dired
    "fd" 'dired-jump
    "pb" 'helm-projectile-switch-to-buffer
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file
    "pF" 'helm-projectile-find-file-dwim
    "ph" 'helm-projectile
    "pp" 'helm-projectile-switch-project
    "pr" 'helm-projectile-recentf
    "p/" 'helm-projectile-grep)
  (mq/declare-prefix "b" "buffers")
  (mq/declare-prefix "f" "files")
  (mq/declare-prefix "p" "projectile")
  (global-evil-leader-mode)

  ;; elisp mode
  (evil-define-key 'normal emacs-lisp-mode-map "K"
    (lambda () (interactive) (preserve-selected-window
                              ;; Skip the prompt.
                              (lambda () (describe-symbol (or (symbol-at-point)
                                                              (error "No symbol at point")))))))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "gg" 'xref-find-definitions
    "ef" 'eval-defun)

  (evil-define-key 'normal dired-mode "r" 'revert-buffer))

(use-package window-management-personal
  :after (evil-leader)
  :config
  (evil-leader/set-key
    "wn" 'create-window-in-next-logical-spot
    "wh" 'split-window-vertically-and-focus
    "wv" 'split-window-horizontally-and-focus
    "we A-e" 'swap-window-with-upper-left
    "we A-d" 'swap-window-with-lower-left
    "we A-r" 'swap-window-with-upper-right
    "we A-f" 'swap-window-with-lower-right)
  (mq/declare-prefix "w" "windows")
  (mq/declare-prefix "we" "swap"))

(use-package company
  :ensure t
	:config (progn
						(setq company-idle-delay 0)
						(setq company-minimum-prefix-length 1)
						(setq company-tooltip-align-annotations t)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package flycheck
  :ensure t
  :delight "FlyC"
  :config
  (global-flycheck-mode)
  (evil-define-key 'normal flycheck-mode-map (kbd "C-n") 'flycheck-next-error)
  (evil-define-key 'normal flycheck-mode-map (kbd "C-p") 'flycheck-previous-error))

(use-package lsp-mode
  :ensure t
  :config
  ;; use flycheck, not flymake
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :ensure t
	:commands company-lsp)

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (evil-leader/set-key-for-mode 'go-mode
    "gg" 'lsp-find-definition
    "rn" 'lsp-rename))

(use-package doom-themes
	:ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  ;; Set the default mode for the *scratch* buffer
  (setq initial-major-mode 'markdown-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package org
  :ensure t
  :init
  (require 'mq-org-mode)

  ;; I only use priorities A-C.
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?C)

  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file "refile.org")
  (setq org-outline-path-complete-in-steps nil)

  (setq mq-org-files '("~/Dropbox/org/tasks.org"
                       "~/Dropbox/org/personal.org"))

  (setq org-agenda-files (cons "~/Dropbox/org/refile.org" mq-org-files))

  (setq org-completion-use-ido t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; I have some deeply nested trees, so only use the top three levels as
  ;; refle targets.
  (setq org-refile-targets '(
                             ;; (nil :maxlevel . 2)
                             (mq-org-files :maxlevel . 3)
                             ))

  ;; Log task state changes into the LOGBOOK drawer
  (setq org-log-into-drawer t)

  ;; Don't allow parent TODOs to be marked complete untill all TODO children are
  (setq org-enforce-todo-dependencies t)

  ;; C-ret starts inserts a new line instead of breaking the current one.
  (setq org-M-RET-may-split-line nil)

  ;; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  ;; Align tags at column 90.
  (setq org-tags-column 100)

  ;; This enables "clean mode", such that sublists use whitespace for
  ;; indentation (ala markdown) instead of many stars.
  (setq org-startup-indented t)

  ;; Always use the fast todo selection rather than cycling through
  ;; TODO states.
  (setq org-use-fast-todo-selection t)

  (setq org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

  (setq org-todo-keyword-faces
    (quote (("TODO" :foreground "red" :weight bold)
            ("NEXT" :foreground "yellow" :weight bold)
            ("DONE" :foreground "forest green" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)
            ("MEETING" :foreground "forest green" :weight bold))))

  ;; Add/remove tags when moving tasks between states.
  (setq org-todo-state-tags-triggers
    (quote (("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol.
  (setq org-capture-templates
    (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
             "* TODO %?")
            ("n" "note" entry (file "~/Dropbox/org/refile.org")
             "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
             "* MEETING %? :MEETING:\n%U" :clock-in t :clock-resume t))))

  (setq org-agenda-custom-commands
    '(("c" "Simple agenda view"
       ((tags "PRIORITY=\"A\""
              ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
               (org-agenda-overriding-header "High-priority unfinished tasks:")))
        (agenda "")
        (alltodo ""
                 ((org-agenda-skip-function '(or (mq/org-skip-subtree-if-priority ?A)
                                                 (org-agenda-skip-if nil '(scheduled deadline)))))))
       ((org-agenda-files '("~/Dropbox/org/refile.org"
                            "~/Dropbox/org/tasks.org"))))))

  :config
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-mode-hook #'visual-line-mode)
  ;; When capturing, automatically enter insert mode.
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package helm
  :ensure t
  :init
  (setq helm-always-two-windows nil)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  ;; Fuzzy matching everywhere.
  (setq helm-M-x-fuzzy-match        t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map [escape] 'helm-keyboard-quit)

  (use-package helm-projectile :ensure t)
  (use-package helm-files
    :config
    (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level))

  (helm-mode t))

(use-package helm-org
  :ensure t
  :after (org helm)
  :config
  (setq helm-org-format-outline-path t))

(use-package helm-lsp
  :commands
  helm-lsp-workspace-symbol)

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode)
  (evil-leader/set-key "hb" 'describe-bindings))

(use-package evil-org
  :ensure t
  :after (org evil evil-leader)
  :init
  ;; Keep selection when shifting with >, <.
  (setq evil-org-retain-visual-state-on-shift t)
  ;; Make keybindings available in insert mode as well.
  (setq evil-org-use-additional-insert t)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)

  (evil-define-key 'normal evil-org-mode-map
    "t" 'org-todo
    (kbd "A-h") 'outline-up-heading
    (kbd "A-j") 'org-forward-heading-same-level
    (kbd "A-k") 'org-backward-heading-same-level)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  ;; Bind hlkj keys to navigate the calendar (e.g. when using org-deadline).
  (define-key org-read-date-minibuffer-local-map (kbd "M-h") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-H") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-L") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-K") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-J") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda
    "oo" (lambda () (interactive) (find-file "~/Dropbox/org/tasks.org")))
  (mq/declare-prefix "o" "org-mode")

  (evil-leader/set-key-for-mode 'org-mode
    "/" 'org-sparse-tree
    "n" 'org-narrow-to-subtree
    "r" 'org-refile
    "d" 'org-deadline
    "a" 'org-archive-subtree-default
    "N" 'widen
    "ci" 'org-clock-in
    "co" 'org-clock-out)
  )

(use-package evil-commentary
  :ensure t
  :after (evil)
  :config
  (evil-commentary-mode))

(defun mq/cider-show-cider-buffer ()
  "Shows the nrepl buffer, but does not focus it."
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(use-package clojure-mode
  :ensure t
  :init
  ; Don't prompt for commands like cider-doc and cider-find-var
  (setq cider-prompt-for-symbol nil)
  ;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
  (setq nrepl-hide-special-buffers t)
  ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :config
  (add-hook 'clojure-mode-hook (lambda () (setq fill-column 80)))
  (load "$REPOS/liftoff/exp/emacs/cljfmt.el")
  (add-hook 'before-save-hook 'cljfmt-before-save))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package evil-cleverparens
  :ensure t
  :init
  ;; evil-cleverparens overrides a lot of my M- bindings.
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (evil-cleverparens-mode)
              (evil-define-key 'normal evil-cleverparens-mode-map
                "M-T" 'sp-transpose-sexp
                "M-k" 'evil-cp-drag-backward
                "M-j" 'evil-cp-drag-forward
                "M-J" 'sp-join-sexp
                "M-y" 'evil-cp-yank-sexp
                "M-d" 'evil-cp-delete-sexp
                "M-c" 'evil-cp-change-sexp
                "M-Y" 'evil-cp-yank-enclosing
                "M-D" 'evil-cp-delete-enclosing
                "M-C" 'evil-cp-change-enclosing
                "M-(" 'evil-cp-wrap-next-round
                "M-)" 'evil-cp-wrap-previous-round
                "M-[" 'evil-cp-wrap-next-square
                "M-]" 'evil-cp-wrap-previous-square
                "M-{" 'evil-cp-wrap-next-curly
                "M-}" 'evil-cp-wrap-previous-curly))))

;; Not sure if I need this yet.
(use-package evil-surround
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'evil-surround-mode))

(require 'evil-surround)

(use-package cider
  :ensure t
  :init
  (setq cider-clojure-cli-global-options "-A:liftoff:dev")
  ; Don't prompt for commands like cider-doc and cider-find-var
  (setq cider-prompt-for-symbol nil)
  :config
  (evil-define-key 'normal clojure-mode-map "K"
    (lambda () (interactive)
      (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))
  (evil-leader/set-key-for-mode 'clojure-mode
    ;; TODO: It would be nice if this left the cursor in the same place.
    "gg" (lambda () (interactive) (cider-find-var))
    "gb" (lambda () (interactive) (cider-pop-back))
    "eb" (lambda ()
           (interactive)
           (save-buffer)
           (cider-eval-buffer))
    "es" 'cider-eval-sexp-at-point
    "ef" 'cider-eval-defun-at-point
    "ee" 'mq/cider-show-cider-buffer
    "ec" 'cider-find-and-clear-repl-output
    "tt" (lambda ()
           (interactive)
           (cider-eval-defun-at-point)
           ;; This doesn't work.
           (preserve-selected-window (lambda () (call-interactively 'cider-test-run-test))))
    "tn" (lambda ()
           (interactive)
           (save-buffer)
           (cider-eval-buffer)
           ;; This call never runs for some reason.
           (cider-test-run-ns-tests))))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-new-workspace t)
  :config
  ;; TODO:
  ;; - Automatically set the name of the window when opening a projectile project.
  ;; - Support closing a window through repeated uses of M-w
  ;; - Move the tab selection out of the modeline if possible
  (eyebrowse-mode t)
  ;; These key bindings reflect macOS default keybindings for managing tabs.
  (define-key eyebrowse-mode-map (kbd "M-t") 'eyebrowse-create-window-config)
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
  (define-key eyebrowse-mode-map (kbd "M-{") 'eyebrowse-prev-window-config)
  (define-key eyebrowse-mode-map (kbd "M-}") 'eyebrowse-next-window-config)
  (define-key eyebrowse-mode-map (kbd "M-r") 'eyebrowse-rename-window-config))

;; LSP mode generates a lot of garbage.
(setq gc-cons-threshold 100000000)
;; Some LSP responses are in the 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(provide 'init)
