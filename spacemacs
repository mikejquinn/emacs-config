;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ansible
     auto-completion
     clojure
     deft
     emacs-lisp
     ;; evil-cleverparens
     (go :variables
         go-tab-width 4
         go-format-before-save t
         godoc-at-point-function 'godoc-gogetdoc)
     ;; helm
     ;; html
     ;; javascript
     ;; lua
     markdown
     org
     python
     ruby
     rust
     sql
     syntax-checking
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(protobuf-mode visual-fill-column flymd)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key ";"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ";"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Silence warnings on startup due to the fact that my .zshrc sets some
  ;; extra PATH directories on interactive shells (e.g. fzf, rbenv).
  (setq exec-path-from-shell-check-startup-files nil)
  )

; Exclude DONE state tasks from refile targets
(defun mq/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun mq/backward-kill-line (arg)
  "Delete backward (Ctrl-u) as in Bash."
  (interactive "p")
  (kill-line (- 1 arg)))

(defvar mq/osx-keys-minor-mode-map (make-keymap) "osx-keys-minor-mode-keymap")

(defun mq/trim-last-word-of-string (string)
  "Removes the last word from the given string. Word separators are -, _ and spaces. This is designed to
  perform the same function as kill-word, but on a string argument."
  (lexical-let ((i 0))
    (while (and (< i (length string))
                (string-match "[-_ ]+" string i))
      (setq i (second (match-data))))
    (if (= i 0)
      ""
      (substring string 0 (dec i)))))

(defun mq/isearch-del-word (&optional arg)
  "Delete word from end of search string and search again. If search string is empty, just beep.
  This function definition is based on isearch-del-char, from isearch.el."
  (interactive "p")
  (if (= 0 (length isearch-string))
    (ding)
    (setq isearch-string (mq/trim-last-word-of-string isearch-string)
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")))
  ;; Use the isearch-other-end as new starting point to be able
  ;; to find the remaining part of the search string again.
  (when isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

;; Taken from https://groups.google.com/forum/#!topic/gnu.emacs.help/vASrP0P-tXM
(defun mq/recenter-no-redraw (&optional arg)
  "Centers the viewport around the cursor."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

;; Taken from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun mq/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun preview-org (beg end)
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max))))
    (call-process-region beg end "/bin/bash" nil nil nil "-c"
                         "/Users/mikeq/bin/convert_org_to_markdown.rb | markdown_page.rb --css gmail | browser")))
    ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
    ;; (call-process-region beg end "/bin/bash" nil nil nil "-c"
    ;;                      "/Users/mikeq/bin/convert_org_to_markdown.rb | /Users/mikeq/bin/markdown_page.rb --css gmail > /Users/mikeq/out")))
;;"convert_org_to_markdown.rb | markdown_page.rb --css gmail | browser")))

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (global-auto-revert-mode nil)

  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)

  (setq mac-pass-command-to-system nil)

  ;; Include custom elisp files.
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
  (require 'lisp-helpers-personal)

  (require 'liftoff-pivotal)

  ;; Store ~ and # files outside of the working directory
  (setq backup-directory-alist `(("." . "~/.emacs_saves")))

  ;; TOML
  (add-hook 'toml-mode-hook
            (lambda () (setq require-final-newline t)))

  ;; Clojure
  (require 'cljfmt)
  (add-hook 'before-save-hook 'cljfmt-before-save)
  ;(spacemacs/toggle-evil-cleverparens-on)
  ;(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  ;(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)

  ;; Deft (note taking)
  (setq deft-directory "~/Dropbox/notes")
  (setq deft-auto-save-interval 60.0)

  ;; Go
  (setq gofmt-command "goimports")
  (setq gofmt-args '("-local" "liftoff/"))
  (setq company-go-gocode-args '("-builtin" "-unimported-packages"))

  ;; Mac OS X keybindings minor mode.
  ;; Make it so the OSX keybindings you're used to always work in every mode in Emacs.
  ;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
  ;;
  (define-key mq/osx-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)
  (define-key mq/osx-keys-minor-mode-map (kbd "M-v") 'clipboard-yank)
  (define-key mq/osx-keys-minor-mode-map (kbd "M-c") 'clipboard-kill-ring-save)
  (define-key mq/osx-keys-minor-mode-map (kbd "M-m") 'iconify-or-deiconify-frame)

  ;; Enable the common Bash text-editing shortcuts in the minibuffer.
  (define-key minibuffer-local-map (kbd "C-k") 'kill-line)
  (define-key minibuffer-local-map (kbd "C-e") 'end-of-line)
  (define-key minibuffer-local-map (kbd "C-u") 'mq/backward-kill-line)
  (define-key minibuffer-local-map (kbd "C-d") 'delete-char)
  (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
  (define-key minibuffer-local-map (kbd "C-h") 'backward-delete-char)

  ;; Enable the typical Bash/readline keybindings when in insert mode.
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)

  ;; Move up and down through long, wrapped lines one visual line at a time.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Enable normal file save shortcut.
  (define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "M-s") 'save-buffer)

  ;; Incremental search (isearch)
  (setq case-fold-search t) ; Make searches case insensitive.
  ;; Make highlighting during incremental search feel snappier.
  (setq lazy-highlight-initial-delay 0)
  (setq lazy-highlight-max-at-a-time nil)
  ;; Make C-h act the same as backspace, as it does in readline.
  (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
  ;; Make M-v paste the clipboard's text into the search ring.
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "C-w") 'mq/isearch-del-word)

  ;; Use firefox for markdown rendering.
  (setq flymd-browser-open-function 'my-flymd-browser-function)

  ;; When pressing enter to confirm a search, or jumping to the next result, scroll the result to the center of
  ;; the window. This solves the UX problem of the result appearing at the bottom of the screen, with little
  ;; context.
  (defadvice evil-search-next (after isearch-recenter activate)
    (mq/recenter-no-redraw))
  (defadvice evil-search-previous (after isearch-recenter activate)
    (mq/recenter-no-redraw))
  (defadvice isearch-exit (before isearch-recenter activate)
    (mq/recenter-no-redraw))

  (require 'window-management-personal)
  (spacemacs/declare-prefix "we" "window-swaps")
  (spacemacs/set-leader-keys
    "wn" 'create-window-in-next-logical-spot
    "we A-e" 'swap-window-with-upper-left
    "we A-d" 'swap-window-with-lower-left
    "we A-r" 'swap-window-with-upper-right
    "we A-f" 'swap-window-with-lower-right)

  ;; These aren't specifically replicating OSX shortcuts, but they manipulate the window, so I want them to take
  ;; precedence over everything else.
  ;; Note that I have Ctrl-Space mapped to Alt, which makes these shortcuts easy to hit.
  (define-key mq/osx-keys-minor-mode-map (kbd "A-e") 'switch-to-upper-left)
  (define-key mq/osx-keys-minor-mode-map (kbd "A-d") 'switch-to-lower-left)
  (define-key mq/osx-keys-minor-mode-map (kbd "A-r") 'switch-to-upper-right)
  (define-key mq/osx-keys-minor-mode-map (kbd "A-f") 'switch-to-lower-right)
  (define-key mq/osx-keys-minor-mode-map (kbd "M-q") 'save-buffers-kill-terminal)
  (define-key mq/osx-keys-minor-mode-map (kbd "M-h") 'ns-do-hide-emacs)
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

  ;; Shortcuts to specific files.
  (spacemacs/declare-prefix "o" "open-files")
  (spacemacs/set-leader-keys
    "oo" (lambda () (interactive) (find-file "~/Dropbox/org/tasks.org"))
    "oh" (lambda () (interactive) (find-file "~/work/src/liftoff/ops/ansible/hosts")))

  ;; Wrap lines at fill-column instead of the right side of the window when
  ;; using visual-line-mode.
  (global-visual-fill-column-mode)

  ;; Force popup windows (such as the org agenda) to split vertically.
  ;; (setq split-width-threshold 0)

  ;; Workaround for https://github.com/bbatsov/projectile/issues/1302
  (setq projectile-git-submodule-command nil)

  (eval-after-load 'org
    '(progn
       (add-to-list 'org-modules 'org-habit)
       (require 'ox-odt)

       (add-hook 'org-mode-hook #'visual-line-mode)
       (add-hook 'org-mode-hook (lambda () (setq fill-column 120)))

       ;; (setq org-highest-priority "A")
       ;; (setq org-lowest-priority "C")
       ;; (setq org-default-priority "B")

       (setq org-directory "~/Dropbox/org")
       (setq org-default-notes-file "refile.org")
       (setq org-agenda-files '("~/Dropbox/org/personal.org"
                                "~/Dropbox/org/refile.org"
                                "~/Dropbox/org/tasks.org"))

       (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

       ;; This enables "clean mode", such that sublists use whitespace for
       ;; indentation (ala markdown) instead of many stars.
       (setq org-startup-indented t)

       ;; C-ret starts inserts a new line instead of breaking the current one.
       (setq org-M-RET-may-split-line nil)

       ;; Use the current window for indirect buffer display
       (setq org-indirect-buffer-display 'current-window)

       ;; Align tags at column 90.
       (setq org-tags-column 100)

       ; Exclude DONE state tasks from refile targets
       ;; (setq org-refile-target-verify-function 'mq/verify-refile-target)

       (setq org-enforce-todo-dependencies t)

       ;; Log task state changes into the LOGBOOK drawer
       (setq org-log-into-drawer t)

       ;; Normal state shortcuts
       (evil-define-key 'normal evil-org-mode-map
         "H" 'org-beginning-of-line
         "L" 'org-end-of-line
         "$" 'org-end-of-line
         "gl" 'org-goto-top-level-heading
         "gu" 'outline-up-heading
         (kbd "A-h") 'outline-up-heading
         (kbd "A-j") 'org-forward-heading-same-level
         (kbd "A-k") 'org-backward-heading-same-level)

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

       (setq org-use-fast-todo-selection t)

       ;; Don't allow parent TODOs to be marked complete untill all TODO children are
       (setq org-enforce-todo-dependencies t)

       ;; Add/remove tags when moving tasks between states.
       (setq org-todo-state-tags-triggers
             (quote (("CANCELLED" ("CANCELLED" . t))
                     ("WAITING" ("WAITING" . t))
                     ("HOLD" ("WAITING") ("HOLD" . t))
                     (done ("WAITING") ("HOLD"))
                     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                     ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

       ;; When capturing, automatically enter insert mode.
       (add-hook 'org-capture-mode-hook 'evil-insert-state)

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
                ((tags "PRIORITY=\"A\"|PRIORITY=\"B\""
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "High-priority unfinished tasks:")))
                 (agenda "")
                 (alltodo ""
                          ((org-agenda-skip-function
                            '(or (mq/org-skip-subtree-if-priority ?A)
                                 (mq/org-skip-subtree-if-priority ?B)
                                 (org-agenda-skip-if nil '(scheduled deadline)))))))
                ((org-agenda-files '("~/Dropbox/org/tasks.org" "~/Dropbox/org/refile.org")))
                )))

       ))

  ;; Rust
  (setq rust-format-on-save t)

  ;;
  ;; JSON
  ;;
  (defun json-format ()
    "Pipe the current buffer into `jq .`, and replace the current buffer's contents."
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max) "jq" t (buffer-name) t "."))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify yaml-mode ws-butler winum which-key web-mode web-beautify volatile-highlights visual-fill-column vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit sql-indent spaceline slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode protobuf-mode popwin pip-requirements persp-mode paradox org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode minitest markdown-toc macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint json-mode js2-refactor js-doc jinja2-mode indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-guru go-eldoc gnuplot gh-md fuzzy flymd flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu emmet-mode elisp-slime-nav dumb-jump diminish deft define-word cython-mode company-web company-tern company-statistics company-go company-ansible company-anaconda column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile ansible-doc ansible aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
