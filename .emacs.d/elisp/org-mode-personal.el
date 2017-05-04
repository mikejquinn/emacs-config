;;; My additions and customizations to org-mode.
;;; Based loosely on evil-org-mode as a starting point.
;;; Provides an evil-org-mode minor mode.
;;; https://github.com/edwtjo/evil-org-mode

(require 'org)
(require 'ox-odt)
(provide 'org-mode-personal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Dropbox (Personal)/org")
(setq org-default-notes-file "~/Dropbox (Personal)/org/refile.org")

(setq org-agenda-files (quote ("~/Dropbox (Personal)/org")))

;; Use IDO for both buffer and file completion and ido-everywhere to t.
(setq org-completion-use-ido t)
;; C-ret starts inserts a new line instead of breaking the current one.
(setq org-M-RET-may-split-line nil)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Align tags at column 80.
(setq org-tags-column 80)

(defun init-org-mode-personal ()
  ;; This enables "clean mode", such that sublists use whitespace for indentation (ala markdown) instead of
  ;; many stars.
  (setq org-startup-indented t))

(eval-after-load 'org '(init-org-mode-personal))

(defun init-org-mode-buffer ()
  ;; Since I don't use fill mode on Org Mode headers, don't highlight long lines.
  (setq-local whitespace-style '(face trailing)))

(add-hook 'org-mode-hook 'init-org-mode-buffer)

(setq org-acii-text-width 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mq/evil-org-eol-call (fun)
  (end-of-line)
  (funcall fun)
  (evil-append nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(evil-leader/set-key
  "ol" 'org-store-link
  "oc" 'org-capture
  "oj" 'org-clock-goto
  "oa" 'org-agenda
  "ob" 'org-iswitchb)

(evil-leader/set-key-for-mode 'org-mode
  "a" (lambda () (interactive)
         (org-archive-subtree)
         ;; For some reason org-archive-subtree aggressively scrolls the window down. Re-center the window on
         ;; the cursor.
         (call-interactively 'evil-scroll-line-to-center))
  "ci" 'org-clock-in
  "co" 'org-clock-out
  "s" 'org-schedule
  )

;; Normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "t" 'org-todo
  "T" '(lambda () (interactive) (mq/evil-org-eol-call '(org-insert-todo-heading)))
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "$" 'org-end-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "^" 'org-beginning-of-line
  ; I use "gl" for this because it behaves similarly to "goto label" in gmail and elsewhere
  "gl" 'org-goto-top-level-heading
  "gu" 'outline-up-heading
  )
  ;; "o" '(lambda () (interactive) (mq/evil-org-eol-call 'always-insert-item))
  ;; ;; "O" '(lambda () (interactive) (mq/evil-org-eol-call 'org-insert-heading))
  ;; ";g" 'org-set-tags-command
  ;; "yc" 'org-table-copy-region
  ;; "-" 'org-cycle-list-bullet
  ;; ; Normally these go backwards-and-forward by paragraphs but skipping between headings is more useful.
  ;; "{" 'org-backward-heading-same-level
  ;; "}" 'org-forward-heading-same-level
  ;; (kbd "<C-tab>") 'org-expand-top-level-parent
  ;; (kbd "TAB") 'org-cycle)
  ;; )

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "C-S-L") 'org-metaright
          (kbd "C-S-H") 'org-metaleft
          (kbd "C-S-K") 'org-metaup
          (kbd "C-S-J") 'org-metadown))
      '(normal insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4)))

; Exclude DONE state tasks from refile targets
(defun mq/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'mq/verify-refile-target)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task and Clock settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
;; TODO: This doesn't work since I rebind L/R shift to ( and ).
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Persist clock history across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 10)

;; Don't allow parent TODOs to be marked complete untill all TODO children are
(setq org-enforce-todo-dependencies t)

;; Dim blocked tasks in agenda views
(setq org-agenda-dim-blocked-tasks t)

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
      (quote (("t" "todo" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish n)
              ("t" "note" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Dropbox (Personal)/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Dropbox (Personal)/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Remove empty LOGBOOK drawers on clock out
(defun mq/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK")))

;; (add-hook 'org-clock-out-hook 'mq/remove-empty-drawer-on-clock-out 'append)

;; (defun preview-org ()
;;   "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
;;   (interactive)
;;   ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
;;   (call-process-region (point-min) (point-max) "/bin/bash" nil nil nil "-c"
;;                        "convert_org_to_markdown.rb | markdown_page.rb | bcat"))

;; (evil-leader/set-key-for-mode 'org-mode
;;   "vv" 'preview-org)

;; (defun always-insert-item ()
;;   (if (not (org-in-item-p))
;;       (insert "\n")
;;     (org-insert-item)))

;; ;; Moves the current heading (and all of its children) into the matching parent note in the archive file.
;; ;; I think this is the most sensible way to archive TODOs in org mode files.
;; ;; http://orgmode.org/worg/org-hacks.html
;; (defadvice org-archive-subtree (around my-org-archive-subtree activate)
;;   (let ((org-archive-location
;;          (if (save-excursion (org-back-to-heading)
;;                              (> (org-outline-level) 1))
;;              (concat (car (split-string org-archive-location "::"))
;;                      "::* "
;;                      (car (org-get-outline-path)))
;;            org-archive-location)))
;;     ad-do-it))

;; (defun text-of-current-line ()
;;   (buffer-substring-no-properties (line-beginning-position)
;;                                   (line-beginning-position 2)))

;; (defun org-get-current-heading ()
;;   "Assumes the cursor is currently on a heading. TODO: return nil if the cursor isn't on a heading."
;;   (-> (text-of-current-line) chomp (split-string "* ") second))


;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (clojure . t)
;;    (ruby . t)
;;    (sh . t)
;;    ))
