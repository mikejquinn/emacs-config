;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode ; Jump to any text on screen in a few keystrokes. Like Vim's EasyMotion.
                      ;ag ; Silver searcher integration for Emacs
                      ;autopair ; Insert matching delimiters, e.g. insert closing braces.
                      auto-complete
                      spinner
                      queue
                      company
                      coffee-mode
                      deft ; Note taking
                      dired-details+ ; Hides all of the unnecessary file details in dired mode.
                      diminish ; For hiding and shortening minor modes in the modeline
                      evil
                      evil-leader
                      evil-nerd-commenter
                      flx-ido ; Fuzzy matching for ido, which improves the UX of Projectile.
                      go-mode
                      less-css-mode
                      ido-ubiquitous ; make ido completions work everywhere.
                      ido-vertical-mode ; show ido results vertically.
                      markdown-mode
                      org ; For outlining. This is bundled with Emacs, but I'm using the latest version.
                      outline-magic ; Extensions to ouline mode, which I use heavily in markdown mode.
                      projectile ; Find file in project (ala CTRL-P).
                      protobuf-mode
                      rainbow-delimiters ; Highlight parentheses in rainbow colors.
                      scss-mode
                      smartparens
                      tangotango-theme
                      workgroups2
                      yaml-mode
                      yasnippet
                      zoom-frm))

(dolist (p my-packages)
  (unless (package-installed-p p))
    (package-install p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/org-mode/lisp"))
(require 'org)

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'lisp-helpers-personal)
(require 'emacs-utils)

;; Anecdotally, this reduces the amount of display flicker on some Emacs startup.
(setq redisplay-dont-pause t)

;; Turn off graphical toolbars.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode) (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode) (scroll-bar-mode -1))

(setq initial-scratch-message "") ; When opening a new buffer, don't show the scratch message.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

(global-auto-revert-mode 1) ; Reload an open file from disk if it is changed outside of Emacs.

;; Require typing only "y" or"n" instead of the full "yes" to confirm destructive actions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Store ~ and # files outside of the working directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

(setq vc-follow-symlinks t) ; Don't ask confirmation to follow symlinks to edit files.
(setq text-scale-mode-step 1.1) ;; When changing font size, change in small increments.

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)

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

;; Colorscheme
(load-theme 'tangotango t)
;; (load-theme 'solarized-light t)
; A font face size of 140 can show 110 chars before wrapping on a 1920x1200 resolution.
(set-face-attribute 'default nil :height 140)

;; Whitespace & line wrapping.
(global-whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-line-column 110) ; When text flows past 110 chars, highlight it.
     ; whitespace-mode by default highlights all whitespace. Show only tabs and trailing spaces.
     (setq whitespace-style '(face trailing lines-tail))))
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 2)
(setq-default fill-column 110) ; When wrapping with the Emacs fill commands, wrap at 110 chars.
(setq column-number-mode t) ; Show column number in the mode line
(auto-fill-mode t) ; When typing across the fill-column, hard-wrap the line as you type.
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Some modes, like markdown, turn off autofill. Force it!

(global-visual-line-mode t)

;; Highlight the line the cursor is on. This is mostly to make it easier to tell which split is active.
(global-hl-line-mode)

;; Don't use tabs by default. Modes that really need tabs should enable indent-tabs-mode explicitly.
;; Makefile-mode already does that, for example. If indent-tabs-mode is off, untabify before saving.
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks
          (lambda ()
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
            nil))

;; Enable the common Bash text-editing shortcuts in the minibuffer.
(define-key minibuffer-local-map (kbd "C-k") 'kill-line)
(define-key minibuffer-local-map (kbd "C-e") 'end-of-line)
(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-line)
(define-key minibuffer-local-map (kbd "C-d") 'delete-char)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

;; RecentF mode is the Emacs minor mode used when opening files via C-x C-f.
(require 'recentf)
(define-key recentf-mode-map (kbd "C-w") 'backward-kill-word)
(define-key recentf-mode-map (kbd "C-h") 'backward-delete-char)

;; Emacs general autocompletion
(add-hook 'prog-mode-hook 'auto-complete-mode)
(eval-after-load 'auto-complete
  '(progn
     (define-key ac-complete-mode-map "\C-n" 'ac-next)
     (define-key ac-complete-mode-map "\C-p" 'ac-previous)
     (setq ac-auto-start nil)
     (ac-set-trigger-key "TAB")
     (ac-linum-workaround)))

;; Some help keybindings which conflict with nothing else, so you can pull up help in any context.
(global-set-key (kbd "C-A-M-h") 'help)
(global-set-key (kbd "C-A-M-b") 'describe-bindings)

;; TODO: Do this on save, but fix the modes that auto-save.
(global-set-key (kbd "M-d") 'delete-trailing-whitespace)

;; The poorly-named winner mode saves the history of your window splits, so you can undo and redo changes to
;; your window configuration.
(winner-mode t)

;; Disable creation of lock files. I nearly always edit through emacs, and emacs seems to get confused
;; about file ownership if my mac hostname changes.
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode -- Vim keybindings for Emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-want-C-u-scroll t)
(setq evil-shift-width 2)
(require 'evil-leader) ; Provide configuration functions for assigning actions to a Vim leader key.
(require 'evil)
(global-evil-leader-mode t)
(evil-mode t)
;; Note that there is a bug where Evil-leader isn't properly bound to the initial buffers Emacs opens
;; with. We work around this by killing them. See https://github.com/cofi/evil-leader/issues/10.
(kill-buffer "*Messages*")

;; When opening new lines, indent according to the previous line.
(setq evil-auto-indent t)

;; Unbind "q" so it doesn't record macros. I activate this mistakenly all the time and wreak havoc.
(define-key evil-normal-state-map (kbd "q") nil)
(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "M-s") 'save-buffer)

;; Move up and down through long, wrapped lines one visual line at a time.
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "K") 'info-lookup-symbol)
;; I use this shortcut for manually splitting lines. Note that it does not put you in insert mode.
(define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)

;; By default, Emacs will not indent when you hit enter/return within a comment.
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

;; When jumping back and forth between marks, recenter the screen on the cursor.
(define-key evil-normal-state-map (kbd "C-o")
  (lambda () (interactive) (evil-jump-backward) (recenter-no-redraw)))
(define-key evil-normal-state-map (kbd "C-i")
  (lambda () (interactive) (evil-jump-forward) (recenter-no-redraw)))

;; gq is normally bound to evil-fill-and-move, but when I reflow a paragraph, I like the cursor to remain
;; where it was.
(define-key evil-normal-state-map "gq" 'evil-fill)
(define-key evil-normal-state-map "-" 'evil-indent-without-move)

;; Enable the typical Bash/readline keybindings when in insert mode.
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(global-set-key (kbd "<A-backspace>") 'backward-kill-word)
(global-set-key (kbd "C-h") 'backward-delete-char) ; Here we clobber C-h, which accesses Emacs's help.

(evil-leader/set-key
 "h" 'help
 "B" 'ido-switch-buffer
 ;; "SPC" 'evil-fill-inside-paragraph ; Shortcut for Vim's gqip
;;   "i" 'evil-indent-inside-paragraph ; Shortcut to Vim's =ip
  "vo" (lambda () (interactive) (find-file "~/Dropbox/org/tasks.org"))
  "ve" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "vh" (lambda () (interactive) (find-file "~/work/src/liftoff/ops/ansible/hosts"))
 )

(setq evil-leader/leader ";")

(defun backward-kill-line (arg)
  "Delete backward (Ctrl-u) as in Bash."
  (interactive "p")
  (kill-line (- 1 arg)))

;; Commenting via NERD commentor.
(require 'evil-nerd-commenter)
(define-key evil-normal-state-map "," 'evilnc-comment-operator)
(define-key evil-visual-state-map "," 'evilnc-comment-operator)

;; package menu mode
(evil-define-key 'normal package-menu-mode-map "i" 'package-menu-mark-install)
(evil-define-key 'normal package-menu-mode-map "u" 'package-menu-mark-unmark)
(evil-define-key 'normal package-menu-mode-map "U" 'package-menu-mark-upgrades)
(evil-define-key 'normal package-menu-mode-map "r" 'package-menu-refresh)
(evil-define-key 'normal package-menu-mode-map "x" 'package-menu-execute)
(evil-define-key 'normal package-menu-mode-map "?" 'package-menu-describe-package)
(setq evil-emacs-state-modes (delq 'package-menu-mode evil-emacs-state-modes))

;;
;; Mac OS X keybindings minor mode.
;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
(defvar osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")
(define-key osx-keys-minor-mode-map (kbd "M-`") 'other-frame)
(define-key osx-keys-minor-mode-map (kbd "M-~")
  '(lambda () (interactive) (other-frame -1)))
(define-key osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
(define-key osx-keys-minor-mode-map (kbd "M-h") 'ns-do-hide-emacs)
(define-key osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
(define-key osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
(define-key osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)
(define-key osx-keys-minor-mode-map (kbd "M-w")
  (lambda ()
    (interactive)
    (when (not (one-window-p))
      (delete-window)
      (balance-windows)
      nil)))

(define-minor-mode osx-keys-minor-mode
  "A minor-mode for emulating osx keyboard shortcuts."
  t " osx" osx-keys-minor-mode-map)

(osx-keys-minor-mode t)

(defadvice load (after give-osx-keybindings-priority)
  "Try to ensure that osx keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'osx-keys-minor-mode))
      (let ((osx-keys (assq 'osx-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'osx-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist osx-keys))))
(ad-activate 'load)

;; Make it so Esc means quit, no matter the context.
;; http://stackoverflow.com/a/10166400/46237
;; Note that when Emacs becomes unresponsive (e.g. because I accidentally grepped my home directory), I might
;; still need to hold C-g (the Emacs esc/cancel key) to bring it back.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq project-folders '("~/work/src/liftoff" "~/work/src/liftoff/ops" "~/dev/personal" "~/dev/go/src/mikeq"))
(setq ansible-role-folders '("~/work/src/liftoff/ops/ansible/roles"))

;; This is set to 600 by default. It shouldn't be the case, but for some reason, the filter-files-in-directory
;; function hits this limit.
(setq max-lisp-eval-depth 1200)

(defun filter-files-in-directory (directory filter-fn include-subdirectories)
  "Filters the files in the given directory and subdirectories using filter-fn. Excludes .git subdirectories."
  (->> (directory-files directory t)
       (remove-if (lambda (path)
                    (or (string/ends-with path ".")
                        (string/ends-with path "..")
                        (string/ends-with path ".git"))))
       (mapcar (lambda (file)
                 (if (and include-subdirectories (file-directory-p file))
                     (filter-files-in-directory file filter-fn include-subdirectories)
                   file)))
       flatten
       (remove-if-not filter-fn)))

(defun open-root-of-project-in-dired (prompt folders)
  "Prompts for the name of a project which exists in your common project folders and opens a dired window in
   the root of the project folder. This is a fast way to open a new project and be able to run
   projectile-file-file."
  (let ((all-project-folders (->> folders
                                  (mapcar (lambda (file)
                                            (filter-files-in-directory file 'file-directory-p nil)))
                                  flatten)))
    (let ((project-to-open (ido-completing-read prompt
                                                (mapcar 'file-name-nondirectory all-project-folders)
                                                nil t)))
      (->> all-project-folders
           (remove-if-not (lambda (project) (string/ends-with project (concat "/" project-to-open))))
           first
           ((lambda (project)
              (dired project)
              ;; If we invoke this inside of a split, don't set the workgroup's title.
              (when (= 1 (length (window-list)))
                (wg-rename-workgroup (file-name-nondirectory project)))))))))

(evil-leader/set-key
  "vp" (lambda () (interactive) (open-root-of-project-in-dired "Project name: " project-folders))
  "vr" (lambda () (interactive) (open-root-of-project-in-dired "Ansible role: " ansible-role-folders)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window manipulation, switching, & management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'window-management-personal)

;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I want them to take
;; precedence over everything else.
;; Note that I have Ctrl-Space mapped to Alt, which makes these shortcuts easy to hit.
(define-key osx-keys-minor-mode-map (kbd "A-e") 'switch-to-upper-left)
(define-key osx-keys-minor-mode-map (kbd "A-d") 'switch-to-lower-left)
(define-key osx-keys-minor-mode-map (kbd "A-r") 'switch-to-upper-right)
(define-key osx-keys-minor-mode-map (kbd "A-f") 'switch-to-lower-right)

;; Window-management keybindings. "w" is the namespace I use.
(evil-leader/set-key "wn" 'create-window-in-next-logical-spot)
(evil-leader/set-key "wv" 'split-window-horizontally-and-focus)
(evil-leader/set-key "wh" 'split-window-vertically-and-focus)
(evil-leader/set-key "ws A-e" 'swap-window-with-upper-left)
(evil-leader/set-key "ws A-d" 'swap-window-with-lower-left)
(evil-leader/set-key "ws A-r" 'swap-window-with-upper-right)
(evil-leader/set-key "ws A-f" 'swap-window-with-lower-right)
(evil-leader/set-key "wk" (lambda () (interactive) (kill-buffer (current-buffer))))
(evil-leader/set-key "wm" 'toggle-window-maximize)
(evil-leader/set-key "wr" 'evil-window-rotate-downwards)
(evil-leader/set-key "wR" 'evil-window-rotate-upwards)
;; Undo the last change you made to your window configuration.
(evil-leader/set-key "wb" 'winner-undo)
(evil-leader/set-key "we" 'toggle-maximize-lower-right-window)
(evil-leader/set-key "q" 'dismiss-ephemeral-windows)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode - using the Emacs file browser.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired-details+)
(setq dired-use-ls-dired nil)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(put 'dired-find-alternate-file 'disabled nil) ; By default, the dired-find-alternative-file fn is disabled.

;; Use the same buffer for going into and up directories.
(evil-define-key 'normal dired-mode-map (kbd "gu") (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map "H" (lambda () (interactive) (find-alternate-file "..")))
(evil-define-key 'normal dired-mode-map (kbd "<return>")
  'dired-find-alternate-file) ; was dired-advertised-find-file

;; dired overrides my global "other window" shorcut.
(evil-define-key 'normal dired-mode-map ";" nil) ; Ensure my evil-leader key works unhindered.
(evil-define-key 'normal dired-mode-map "cd" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "cf" 'dired-create-file)
(evil-define-key 'normal dired-mode-map "x" 'dired-mark)
(evil-define-key 'normal dired-mode-map "v" 'dired-details-toggle)
(evil-define-key 'normal dired-mode-map "r" 'revert-buffer)
;; The "e" prefix is for execute.
(evil-define-key 'normal dired-mode-map "ed" 'dired-do-flagged-delete)
(evil-define-key 'normal dired-mode-map "em" 'dired-do-rename)

;; Taken from http://stackoverflow.com/a/18885461/46237.
(defun dired-create-file (file)
  "Create a file called FILE, and recursively create any parent directories.
  If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file))
         (try expanded)
         (dir (directory-file-name (file-name-directory expanded)))
         new)
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (write-region "" nil expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp (elisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))
(evil-define-key 'normal emacs-lisp-mode-map
  "gf" 'find-function-at-point
  "K"'(lambda ()
        (interactive)
        ;; Run `describe-function` and show its output in a help
        ;; window. Inspired from help-fns.el.
        (with-help-window "*Help*"
          (describe-function (intern (current-word))))))

(defun current-sexp ()
  "Returns the text content of the sexp list around the cursor."
  (let ((position (bounds-of-thing-at-point 'list)))
    (buffer-substring-no-properties (car position) (cdr position))))

(defun elisp-eval-current-sexp ()
  (interactive)
  (message "%s" (eval (read (current-sexp)))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  ; Note that I'm saving the buffer before each eval because otherwise, the buffer gets saved after the eval,
  ; (due to save-when-switching-windows setup) and the output from the buffer save overwrites the eval results
  ; in the minibuffer.
  "eb" (lambda() (interactive) (save-buffer-if-dirty) (eval-buffer))
  "es" (lambda () (interactive) (save-buffer-if-dirty) (elisp-eval-current-sexp))
  "ex" (lambda () (interactive) (save-buffer-if-dirty) (call-interactively 'eval-defun))
  "ee" 'view-echo-area-messages)

;; Indentation rules.
(put '-> 'lisp-indent-function nil)
(put '->> 'lisp-indent-function nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental search (isearch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun trim-last-word-of-string (string)
  "Removes the last word from the given string. Word separators are -, _ and spaces. This is designed to
  perform the same function as kill-word, but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on isearch-del-char, from isearch.el."
  (interactive "p")
  (if (= 0 (length isearch-string))
    (ding)
    (setq isearch-string (trim-last-word-of-string isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; When pressing enter to confirm a search, or jumping to the next result, scroll the result to the center of
;; the window. This solves the UX problem of the result appearing at the bottom of the screen, with little
;; context.
(defadvice evil-search-next (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice evil-search-previous (after isearch-recenter activate)
  (recenter-no-redraw))

(defadvice isearch-exit (before isearch-recenter activate)
  (recenter-no-redraw))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go mode, for writing Go code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'go-mode)
(evil-define-key 'normal go-mode-map
  "gf" 'godef-jump
  "K" 'godef-describe)

;; goimports formats your code and also adds or removes imports as needed.
;; goimports needs to be on your path. See https://godoc.org/code.google.com/p/go.tools/cmd/goimports
(setq gofmt-command "goimports")
(setq gofmt-args '("-local" "liftoff/"))

(defun init-go-buffer-settings ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Make it so comments are line-wrapped properly when filling. It's an oversight that this is missing from
  ;; go-mode.
  (setq-local fill-prefix "// ")
  (setq tab-width 4)
  (setq indent-tabs-mode 1))

(add-hook 'go-mode-hook 'init-go-buffer-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO Mode config (auto-complete menus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(ido-mode (quote both))
(eval-after-load 'ido
  '(progn
     (setq ido-enable-flex-matching t)
     (setq ido-use-virtual-buffers t)
     (setq ido-everywhere t)
     ;; Use the current window when visiting files and buffers with ido.
     (setq ido-default-file-method 'selected-window)
     (setq ido-default-buffer-method 'selected-window)
     ;; kill the highlighted buffer in the matches list.
     (define-key ido-buffer-completion-map (kbd "M-d") 'ido-kill-buffer-at-head)))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode, for TODOs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-mode-personal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various package configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Ace jump - for quickly jumping to a precise character in view. Similar to Vim's EasyMotion.
;;
(require 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
;; Note that Evil mode's ace-jump integration is supposed to add this motion keybinding automatically for you,
;; but it doesn't work. So I've defined it here explicitly.
(define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-word-mode)

;; Deft - Note taking
(require 'deft)
(evil-leader/set-key "vn" 'deft)
(setq deft-extensions '("org" "md" "txt"))
(setq deft-use-filename-as-title nil)
(setq deft-use-filter-string-for-filename t)
(setq deft-directory "~/Dropbox (Personal)/notes")
(evil-define-key 'normal deft-mode-map "r" 'deft-refresh)
(evil-define-key 'normal deft-mode-map "N" 'deft-new-file-named)
(evil-define-key 'normal deft-mode-map (kbd "C-h") 'deft-filter-decrement)
(evil-define-key 'insert deft-mode-map (kbd "C-h") 'deft-filter-decrement)
(evil-define-key 'normal deft-mode-map (kbd "C-w") 'deft-filter-decrement-word)
(evil-define-key 'insert deft-mode-map (kbd "C-w") 'deft-filter-decrement-word)
(evil-define-key 'insert deft-mode-map (kbd "C-n") 'widget-forward)
(evil-define-key 'insert deft-mode-map (kbd "C-p") 'widget-backward)
(evil-define-key 'insert deft-mode-map (kbd "<C-return>") 'deft-open-file-other-window)
(evil-define-key 'normal deft-mode-map (kbd "RET") 'deft-complete)

;; Diminish - hide or shorten the names of minor modes in your modeline.
;; To see which minor modes you have loaded and what their modeline strings are: (message minor-mode-alist)
(require 'diminish)
(diminish 'visual-line-mode "")
(diminish 'global-whitespace-mode "")
(diminish 'global-visual-line-mode "")
(diminish 'auto-fill-function "")
(diminish 'projectile-mode " p")
(diminish 'yas-minor-mode "yas")
(diminish 'osx-keys-minor-mode "")
(diminish 'undo-tree-mode "")

;; Projectile (find file from the root of the current project).
(require 'projectile)
(evil-leader/set-key
  "t" 'projectile-find-file
  "a" 'projectile-ack
  "b" 'projectile-switch-to-buffer
  "d" 'projectile-dired
  "D" (lambda () (interactive) (-> (buffer-file-name) file-name-directory dired)))
(projectile-global-mode)

;; Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Spell checking
;; http://www.emacswiki.org/emacs/SpeckMode
;;
;; FlySpell is the default choice for spellchecking, but I found it slow, even using every flyspell perf
;; improvement I could find. Speck doesn't slow down your typing.
;;
;; You may need to install aspell (e.g. `brew install aspell`).
;; (add-to-list 'load-path "~/.emacs.d/plugins/speck-mode")
;; (require 'speck)
;; ;; This apparently needs to be a fully-qualified path.
;; (setq speck-personal-dictionary-file (concat (getenv "HOME") "/.personal_dict.txt"))
;; (setq speck-engine (quote Aspell))
;; (add-hook 'text-mode-hook 'speck-mode)
;;  ;; Triggers a spell-correction menu. I use this to add words to my dictionary (hit "i").
;; (define-key evil-normal-state-map (kbd "zg") 'speck-popup-menu-at-point)

;; Workgroups2
(require 'workgroups2)
(evil-leader/set-key "p" 'wg-switch-to-workgroup)
(evil-leader/set-key "np" 'wg-create-workgroup)
(evil-leader/set-key "kp" 'wg-kill-workgroup)
(evil-leader/set-key "rp" 'wg-rename-workgroup)
(define-key osx-keys-minor-mode-map (kbd "M-n") 'wg-create-workgroup)
(workgroups-mode 1)

;; Zoom frm - change font sizes
(require 'zoom-frm)
(define-key osx-keys-minor-mode-map (kbd "M--") 'zoom-out)
(define-key osx-keys-minor-mode-map (kbd "M-=") 'zoom-in)
(define-key osx-keys-minor-mode-map (kbd "M-0") 'zoom-frm-unzoom)

;; Clojure
(add-to-list 'load-path "~/.emacs.d/vendor/cider/")
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode/")
(require 'clojure-mode)
(require 'clojure-mode-personal)
(require 'cider)
(require 'cider-test)

;; ;; Ensure we evil-leader works in non-editing modes like magit. This is referenced from evil-leader's README.
;; (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))

;; (defun evil-fill-inside-paragraph ()
;;   "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
;;   (interactive)
;;   (let ((region (if (use-region-p)
;;                   (list (region-beginning) (region-end))
;;                   (evil-inner-paragraph))))
;;     (evil-fill (first region) (second region))))

;; (defun evil-indent-inside-paragraph ()
;;   "Fills (reflows/linewraps) the current paragraph. Equivalent to gqap in vim."
;;   (interactive)
;;   (let ((region (if (use-region-p)
;;                   (list (region-beginning) (region-end))
;;                   (evil-inner-paragraph))))
;;     (evil-indent-without-move (first region) (second region))))

;; (defun evil-shift-paragraph-left (beg end)
;;   "Shifts a paragraph left."
;;   (interactive "r")
;;   (let ((region (evil-inner-paragraph)))
;;     (save-excursion
;;       (evil-shift-left (first region) (second region)))))

;; (defun evil-shift-paragraph-right (beg end)
;;   "Shifts a paragraph right."
;;   (interactive "r")
;;   (let ((region (evil-inner-paragraph)))
;;     (save-excursion
;;       (evil-shift-right (first region) (second region)))))

;; (defun eval-surrounding-sexp (levels)
;;   (interactive "p")
;;   (save-excursion
;;     (up-list (abs levels))
;;     (eval-last-sexp nil)))

;; (evil-define-operator evil-indent-without-move (beg end)
;;   "Indent text."
;;   :move-point nil
;;   :type line
;;   (save-excursion
;;     (evil-indent beg end)))

;; ;; Evil uses the current file's mode's definition of a paragraph, which is often surprising. For instance, in
;; ;; Markdown mode, a single item in a bullet list consistutes a paragraph. Instead, I've defined a paragraph to
;; ;; be hunks of text separated by newlines. That's typically what I would expect of a paragraph. You can still
;; ;; use Evil's paragraph definition using the text object "P" instead of "p".
;; (evil-define-text-object evil-paragraph-from-newlines (count &optional beg end type)
;;   "Select a paragraph separated by newlines."
;;   :type line
;;   ;; These two vars are set by the current programming mode. Set them to their default text mode values
;;   ;; temporarily while we select the paragraph. The implementation of evil-select-an-object will invoke
;;   ;; `forward-paragraph`, which uses these variables.
;;   (let ((paragraph-start "\f\\|[     ]*$")
;;         (paragraph-separate "[  ]*$"))
;;     (evil-select-an-object 'evil-paragraph beg end type count)))

;; (define-key evil-outer-text-objects-map "p" 'evil-paragraph-from-newlines)
;; (define-key evil-outer-text-objects-map "P" 'evil-a-paragraph)

;; (defun open-folder-in-finder ()
;;   "Opens the folder of the current file in OSX's Finder."
;;   (interactive)
;;   (call-process-region nil nil "/usr/bin/open" nil nil nil "."))

;; ;; Closes the current escreen, or if there's only one screen, use the ":q" Evil
;; ;; command. This simulates the ":q" behavior of Vim when used with tabs.
;; ;; http://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
;; (defun vimlike-quit ()
;;   "Vimlike ':q' behavior: close current window if there are split windows;
;;    otherwise, close current tab (elscreen)."
;;   (interactive)
;;   (let ((one-escreen (= 1 (length (escreen-get-active-screen-numbers))))
;;         (one-window (one-window-p)))
;;     (cond
;;      ; if current tab has split windows in it, close the current live window
;;      ((not one-window)
;;       (delete-window) ; delete the current window
;;       (balance-windows) ; balance remaining windows
;;       nil)
;;      ; if there are multiple escreens (tabs), close the current escreen
;;      ((not one-escreen)
;;       (escreen-kill-screen)
;;       nil)
;;      ; if there is only one elscreen, just try to quit (calling elscreen-kill
;;      ; will not work, because elscreen-kill fails if there is only one
;;      ; elscreen)
;;      (one-escreen
;;       (evil-quit)
;;       nil))))


;; ;;
;; ;; escreen (tabs)
;; ;;
;; ;; I use one tab per "workspace" (i.e. open project). All of my tab-related config is geared towards that use
;; ;; case. I was previously using elscreen, but it has two major bugs: tab bars would get rendered on
;; ;; random windows, and Emacs's redraws would begin flashing if you changed monitors or font size.
;; (require 'escreen)
;; (escreen-install)

;; ;; KeyRemap4Macbook translates M-j and M-k to these keys.
;; (global-set-key (kbd "<A-M-left>") 'escreen-goto-prev-screen)
;; (global-set-key (kbd "<A-M-right>") 'escreen-goto-next-screen)

;; ;; I alias/nickname each of my tabs (escreen's numbered screens).
;; (setq escreen-number->alias (make-hash-table))

;; (defun escreen-set-tab-alias (alias)
;;   "Give the current tab an alias. This alias is shown by escreen-tab-switcher."
;;   (interactive "sTab alias: ")
;;   (when (> (length alias) 0)
;;     (puthash (escreen-get-current-screen-number) alias escreen-number->alias)))

;; (defun escreen-tab-switcher ()
;;   "Shows a menu in the minibuffer of tab names and numbers. Type the tab number to switch to it."
;;   (interactive)
;;   (lexical-let* ((get-display-name (lambda (i)
;;                                      (let ((template (if (= i (escreen-get-current-screen-number))
;;                                                          "*%d.%s*"
;;                                                        "%d.%s")))
;;                                        (->> (or (gethash i escreen-number->alias) "unnamed")
;;                                             (format template (+ i 1))))))
;;                  (tab-names (mapcar get-display-name (escreen-get-active-screen-numbers))))
;;     (message (string/join tab-names "  "))
;;     (lexical-let* ((input (string (read-char)))
;;                    (is-digit (and (string= (number-to-string (string-to-number input)) input))))
;;       (when is-digit
;;         (escreen-goto-screen (- (string-to-number input) 1))))))

;; (defun open-current-buffer-in-new-tab ()
;;   (interactive)
;;   ;; Exit out of insert mode when opening a new tab.
;;   (evil-change-to-initial-state)
;;   ;; I'm using the current buffer in the new tab so that the current directory is set as it was previously,
;;   ;; which lets me begin using projectile immediately.
;;   (let ((buffer (current-buffer)))
;;     (escreen-create-screen)
;;     (set-window-buffer (get-buffer-window) buffer)))

;; ;;
;; ;; Snippets - yassnippet
;; ;;
;; ;; Ignore the default snippets that come with yasnippet. I only need my own, and don't want any conflicts.
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (define-key yas-keymap (kbd "ESC") 'yas-abort-snippet)
;; ;; By default, you can't delete selected text using backspace when tabbing through a snippet.
;; ;; Filed as a bug here: https://github.com/capitaomorte/yasnippet/issues/408
;; (define-key yas-keymap (kbd "C-h") 'yas-skip-and-clear-or-delete-backward-char)
;; ;; (define-key yas-keymap (kbd "backspace") 'yas-skip-and-clear-or-delete-backward-char)

;; ;; This function is based on yas-skip-and-clear-or-delete-char from yassnippet.el.
;; (defun yas-skip-and-clear-or-delete-backward-char (&optional field)
;;   "Clears unmodified field if at field start, skips to next tab. Otherwise deletes backward."
;;   (interactive)
;;   (let ((field (or field
;;                    (and yas--active-field-overlay
;;                         (overlay-buffer yas--active-field-overlay)
;;                         (overlay-get yas--active-field-overlay 'yas--field)))))
;;     (cond ((and field
;;                 (not (yas--field-modified-p field))
;;                 (eq (point) (marker-position (yas--field-start field))))
;;            (yas--skip-and-clear field)
;;            (yas-next-field 1))
;;           (t
;;            (call-interactively 'delete-backward-char)))))

;; ;;
;; ;; Markdown
;; ;;
;; (require 'markdown-mode-personal)

;; ;;
;; ;; CSS
;; ;;
;; (add-hook 'css-mode-hook (lambda ()
;;                            (autopair-mode 1) ; Auto-insert matching delimiters.
;;                            ;; Properly unindent a closing brace after you type it and hit enter.
;;                            (eletric-indent-mode)))

;; ;;
;; ;; Coffeescript
;; ;;
;; (setq coffee-tab-width 2)
;; (evil-leader/set-key-for-mode 'coffee-mode
;;   "c" nil ; Establishes "c" as a "prefix key". I found this trick here: http://www.emacswiki.org/emacs/Evil
;;   ;; This compiles the file and jumps to the first error, if there is one.
;;   "cc" (lambda ()
;;          (interactive)
;;          (save-buffer)
;;          (coffee-compile-without-side-effect))
;;   ;; The mnemonic for this is "compile & preview". It shows the javascript output in a new buffer.
;;   "cp" 'coffee-compile-buffer)

;; (defun coffee-compile-without-side-effect ()
;;   ;; coffee-compile-file annoyingly creates a file on disk.
;;   (let* ((js-file (concat (file-name-sans-extension (buffer-file-name)) ".js"))
;;          (js-file-existed (file-exists-p js-file)))
;;     (coffee-compile-file)
;;     (when (and (not js-file-existed) (file-exists-p js-file))
;;       (delete-file js-file))))

;; ;; Make return and open-line indent the cursor properly.
;; (evil-define-key 'insert coffee-mode-map (kbd "RET") 'coffee-newline-and-indent)
;; (evil-define-key 'normal coffee-mode-map "o" '(lambda ()
;;                                                 (interactive)
;;                                                 (end-of-line)
;;                                                 (evil-append nil)
;;                                                 (coffee-newline-and-indent)))

;; ;;
;; ;; Ruby
;; ;;
;; (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; (eval-after-load 'ruby-mode
;;   '(progn
;;      ;; Ruby mode defines this as "next block". I define it globally as "next window".
;;      (define-key ruby-mode-map (kbd "C-M-n") nil)))

;; ;;
;; ;; HTML mode
;; ;;
;; (add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))

;; (defun preview-html ()
;;   "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
;;   (interactive)
;;   (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c" "bcat"))

;; (evil-leader/set-key-for-mode 'html-mode
;;   "vv" 'preview-html)

;; ;;
;; ;; SCSS mode, for editing SCSS files.
;; ;;
;; (setq scss-compile-at-save nil)
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; ;;
;; ;; YAML mode, for editing YAML files
;; ;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; (defun open-root-of-ansible-role-in-dired ()
;;   "Prompts for the name of an ansible role which exists in the ansible role directory and opens a dired window
;;    in the root of the role folder.
;;    Once a role is chosen, the current elscreen-tab is set to be the name of that role."
;;   (interactive)
;;   (let ((all-project-folders (->> ansible-role-folers
;;                                   (mapcar (lambda (file)
;;                                             (filter-files-in-directory file 'file-directory-p nil)))
;;                                   flatten)))
;;     (let ((project-to-open (ido-completing-read "Role folder: "
;;                                                 (mapcar 'file-name-nondirectory all-project-folders)
;;                                                 nil t)))
;;       (->> all-project-folders
;;            (remove-if-not (lambda (project) (string/ends-with project (concat "/" project-to-open))))
;;            first
;;            ((lambda (project)
;;               (dired project)
;;               ;; If we invoke this inside of a split, don't set the tab's title.
;;               (when (= 1 (length (window-list)))
;;                 (escreen-set-tab-alias (file-name-nondirectory project)))))))))

;;
;; JSON
;;
(defun json-format ()
  "Pipe the current buffer into `jq .`, and replace the current buffer's contents."
  (interactive)
  (save-excursion
    (call-process-region (point-min) (point-max) "jq" t (buffer-name) t ".")))

;;
;; Java
;;
;; (add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))

;; ;; TODO(philc): It would be nice to parameterize this further and combine it with go-save-and-compile-fn.
;; (defun java-save-and-compile-fn (command-name)
;;   "Returns a function for the purpose of binding to a key which saves the current buffer and then
;;    runs the given command in the root of the go project."
;;   (lexical-let ((command-name command-name))
;;     #'(lambda ()
;;         (interactive)
;;         (save-buffer)
;;         (message command-name)
;;         (without-confirmation
;;          (lambda ()
;;            (compile (concat "cd " (locate-dominating-file (buffer-file-name) "build.xml")
;;                             " && " command-name)))))))

;; (evil-leader/set-key-for-mode 'java-mode
;;   ;; ant -find searches up the directory tree and finds the closest build file.
;;   ;; "cc" (java-save-and-compile-fn "ant debug -silent")
;;   "cn" 'next-error
;;   "cp" 'previous-error)

;;
;; Javascript
;;
;; (setq js-indent-level 2)

;;
;; Misc
;;

;; I just invoke this by name using M-x.
;; (defun prompt-to-open-info-page ()
;;   "Prompts you for the name of an info page to view. It's the same as calling info with a prefix argument
;;    ala C-u C-h i using the regular Emacs key bindings."
;;   (interactive)
;;   (setq current-prefix-arg '(4)) ; C-u
;;   (call-interactively 'info))

;; (setq tramp-default-method "ssh")
;; (setq require-final-newline t)

;; (byte-recompile-directory (expand-file-name "/usr/local/Cellar/emacs-mac/emacs-24.5-z-mac-5.11/share/emacs/24.5/") 0)

;; (require 'protobuf-mode)
;; (defconst my-protobuf-style
;;   '((c-basic-offset . 4)
;;     (indent-tabs-mode . nil)))

;; (add-hook 'protobuf-mode-hook
;;           (lambda () (c-add-style "my-protobuf-style" my-protobuf-style t)))

;; Deprecated?

;;(setq font-lock-support-mode 'fast-lock-mode ; lazy-lock-mode
;;      fast-lock-cache-directories '("~/.emacs-flc"))
;; Some modes have their own tab-width variables which need to be overridden.
;; (setq-default css-indent-offset 2)
;; Visually wrap long lines on word boundaries. By default, Emacs will wrap mid-word. Note that Evil doesn't
;; have good support for moving between visual lines versus logical lines. Here's the start of a solution:
;; https://lists.ourproject.org/pipermail/implementations-list/2011-December/001430.html
;; Disable the prompt we get when killing a buffer with a process. This affects clojure mode in particular,
;; when we want to restart the nrepl process.
;; (setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; ; The version of ruby-electric in melpa wasn't compiling for me as of 9/14
;; (add-to-list 'load-path "~/.emacs.d/vendor/ruby-electric/")
;; (require 'ruby-electric)

;; Smart parens
;; (require 'smartparens-config)
(put 'narrow-to-region 'disabled nil)
