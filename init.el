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
(define-key mq/osx-keys-minor-mode-map (kbd "M-h") 'ns-do-hide-emacs)
(define-key mq/osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)
(define-key mq/osx-keys-minor-mode-map (kbd "M-n") 'new-frame)
(define-key mq/osx-keys-minor-mode-map (kbd "M-s") 'save-buffer)
(require 'zoom-frm)
(define-key mq/osx-keys-minor-mode-map (kbd "M--") 'zoom-out)
(define-key mq/osx-keys-minor-mode-map (kbd "M-=") 'zoom-in)
(define-key mq/osx-keys-minor-mode-map (kbd "M-0") 'zoom-frm-unzoom)

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

;;
;; Evil mode
;;

(defun mq/config-evil ()
  "Configure evil mode.")

(defun mq/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ";")

  (evil-leader/set-key
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fj" 'projectile-dired
    "pb" 'helm-projectile-switch-to-buffer
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file
    "pF" 'helm-projectile-find-file-dwim
    "ph" 'helm-projectile
    "pp" 'helm-projectile-switch-project
    "pr" 'helm-projectile-recentf
    "sgp" 'helm-projectile-grep))

(use-package evil
  :ensure t
  :init
  ;; When opening new lines, indent according to the previous line.
  (setq evil-auto-indent t)
  (setq evil-want-C-u-scroll t)
  (require 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'mq/config-evil)
  (evil-mode t)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (mq/config-evil-leader)))

(use-package window-management-personal
  :config
  (evil-leader/set-key
    "wn" 'create-window-in-next-logical-spot
    "wh" 'split-window-vertically-and-focus
    "wv" 'split-window-horizontally-and-focus
    "we A-e" 'swap-window-with-upper-left
    "we A-d" 'swap-window-with-lower-left
    "we A-r" 'swap-window-with-upper-right
    "we A-f" 'swap-window-with-lower-right))

(use-package company
  :ensure t
	:config (progn
						(setq company-idle-delay 0)
						(setq company-minimum-prefix-length 1)
						(setq company-tooltip-align-annotations t)))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

(use-package helm
  :ensure t
  :init
  (setq helm-always-two-windows nil)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  :config
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map [escape] 'helm-keyboard-quit)

  (use-package helm-projectile
    :ensure t)
  (use-package helm-files
    :config
    (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil)))

(use-package company-lsp
  :ensure t
	:commands company-lsp)

(use-package helm-lsp
  :commands
  helm-lsp-workspace-symbol)

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

;; LSP mode generates a lot of garbage.
(setq gc-cons-threshold 100000000)
;; Some LSP responses are in the 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb
