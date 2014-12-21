;;
;; Clojure
;;
;; Docs:
;; https://github.com/clojure-emacs/cider
;; http://clojure-doc.org/articles/tutorials/emacs.html

(provide 'clojure-mode-personal)

;; Count hyphens, etc. as word characters in lisps
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w" clojure-mode-syntax-table)))
(add-hook 'clojure-mode-hook (lambda ()
                               (setq indent-line-function 'lisp-indent-line-single-semicolon-fix)
                               ;; Comment lines using only one semi-colon instead of two.
                               (setq comment-add 0)))

(evil-define-key 'normal clojure-mode-map "K"
  (lambda () (interactive) (preserve-selected-window (lambda () (call-interactively 'cider-doc)))))

(evil-define-key 'normal clojure-mode-map "gf" 'cider-jump)
(evil-define-key 'normal clojure-mode-map "gb" 'cider-jump-back)

;; Hide the uninteresting nrepl-connection and nrepl-server buffers from the buffer list.
(setq nrepl-hide-special-buffers t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established.
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Don't ask confirmation for closing any open nrepl connections when exiting Emacs.
;; http://stackoverflow.com/q/2706527/46237
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(evil-define-operator evil-cider-eval (beg end)
  "Evaluate the text region moved over by an evil motion."
  (cider-eval-region beg end))

;; Eval a paragraph. This is different from eval-surrounding-sexp in that it will eval multiple adjacent
;; s-expressions which are not separated by a new line. It's equivalent to wrapping the expressions in a do.
(defun cider-eval-paragraph (beg end)
  (interactive "r")
  (let ((region (evil-a-paragraph)))
    (evil-cider-eval (first region) (second region))))

(defun cider-show-cider-buffer ()
  "Shows the nrepl buffer, but does not focus it."
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(defun cider-clear-buffer-inside-cider-buffer ()
  "Switches to the cider buffer, clears it, and refocused to the original buffer."
  (interactive)
  (command-execute 'cider-switch-to-repl-buffer)
  (cider-clear-buffer)
  (command-execute 'cider-switch-to-last-clojure-buffer))

(defun my-cider-defun-name-at-point ()
  "Returns the name of the function at point, and nil if it can't be parsed."
  ;; This should work with both defn and deftest.
  (let* ((form (cider-defun-at-point))
         (result (string-match "(def[^ ]* \\([^ ]*\\)" form)))
    (when result
      (match-string 1 form))))

(defun my-cider-run-test-at-point ()
  "Runs the clojure.test under the cursor by invoking the function defined by the test in the cider repl."
  (interactive)
  ;; Note that prior to running the test, we must eval its code in case its definition has been changed
  ;; recently. We use cider-load-current-buffer instead of cider-eval-defun-at-point because
  ;; load-current-buffer properly sets the file metadata for the function definition, so that test failure
  ;; output has the source file and line number of the failing test, as expected.
  (save-buffer)
  (cider-load-current-buffer)
  (sleep-for 0.1) ; cider-load-current-buffer is asynchronous unfortunately.
  ;; If there was a compile error, halt. Otherwise we'll mask the compile error which was printed to the
  ;; minibuffer.
  (if (my-cider-buffer-has-compile-errors)
      nil
    (let ((fn-name (my-cider-defun-name-at-point)))
      ;; TODO(philc): It would be nice if we showed whether the test passed or failed in the minibuffer.
      ;; Currently we just show "nil", and one must look to the repl to see the test output.
      (when fn-name
        (cider-interactive-eval (concat "(" fn-name ")"))))))

(defun my-cider-buffer-has-compile-errors ()
  "Returns true if the current buffer has been evaled previously and has a compile error."
  (interactive)
  ;; cider doesn't expose this information directly. cider-highlight-compilation-errors will set an overlay on
  ;; the buffer if there is a compile error. This fn checks for that font face.
  (let ((overlays ))
    (->> (overlays-in (point-min) (point-max))
         (remove-if-not (lambda (o) (overlay-get o 'cider-note-p)))
         length
         (< 0))))

(defun my-cider-run-tests-in-ns ()
  "Runs any clojure.test tests defined in the current namespace."
  (interactive)
  (cider-interactive-eval "(clojure.test/run-tests)"))

(defun my-cider-restart-nrepl ()
  "Restarts or starts afresh the nrepl."
  (interactive)
  (let ((repl-buffer (nrepl-connection-for-buffer (current-buffer))))
    (without-confirmation (lambda ()
                            (when (not (stringp repl-buffer))
                              (nrepl-close repl-buffer))
                            (cider-jack-in nil)))))

(defun my-cider-make-connection-buffer-the-current-connection (connection-buffer)
  (cons connection-buffer (delq connection-buffer nrepl-connection-list)))

(defun with-nrepl-connection-of-current-buffer (f)
  (let ((result (nrepl-connection-for-buffer (current-buffer))))
    (if (stringp result)
        (message result)
      (progn
        (my-cider-make-connection-buffer-the-current-connection result)
        (funcall f)))))

;; Based on `cider-switch-to-relevant-repl-buffer` in cider.el.
;; NOTE(philc): I think this needs further tuning. It doesn't work in all circumstances.
(defun nrepl-connection-for-buffer (buffer)
  "Returns either the corresponding nrepl buffer for the given buffer, or a string error message."
  (if (not (cider-connected-p))
      "No active nREPL connection."
    (let ((project-directory (nrepl-project-directory-for (nrepl-current-dir))))
      (if (not project-directory)
          "No project directory found."
        (let ((buf (cond
                    ;; I'm special casing shared_lib so that I can eval files from that project against the
                    ;; most recent repl.
                    ((or (search "shared_lib" project-directory)
                         (search "ml_lib" project-directory))
                     (car nrepl-connection-list))
                    (project-directory
                     (car (-filter
                           (lambda (conn)
                             (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                                    nrepl-project-dir)))
                               (when conn-proj-dir
                                 (equal (file-truename project-directory)
                                        (file-truename conn-proj-dir)))))
                           nrepl-connection-list))))))
          (if buf
              (get-buffer buf)
            "No relevant nREPL connection found."))))))

(defun my-cider-eval-and-print-to-repl (form)
  "Wraps the form in 'print' and evaluates the expression."
  ;; Cider has cider-interactive-eval-to-repl, but it prints the results of expressions to random places in
  ;; the repl buffer.
  (let* (;; NOTE(philc): I have pprint aliased into clojure.core as >pprint for all of my lein projects. I've
         ;; done this through ~/.lein/profiles.clj. Assuming you don't, you can just use println as the
         ;; print-fn.
         (print-fn ">pprint")
         (form (concat "(" print-fn form ")")))
    (cider-interactive-eval form)))

(defun my-cider-eval-and-print-defun-at-point ()
  ;; Based on cider-eval-defun-at-point.
  (my-cider-eval-and-print-to-repl (cider-defun-at-point)))

(defun my-cider-eval-current-sexp (&optional print-result)
  "Eval the sexp the cursor is currently in. In Emacs' syntax table, this is called a list of expressions."
  (interactive)
  (let ((form (current-sexp)))
    (if print-result
        (my-cider-eval-and-print-to-repl form)
      (cider-interactive-eval form))))

(evil-leader/set-key-for-mode 'clojure-mode
  "eap" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-paragraph))
  "ee" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-show-cider-buffer))
  "ek" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-find-and-clear-repl-buffer))
  ;; Note that I actually use cider-load-file here, not cider-eval-buffer, because it gives useful line numbers
  ;; on exceptions.
  "eb" (lambda ()
         (interactive)
         (save-buffer)
         (with-nrepl-connection-of-current-buffer 'cider-load-buffer))
  ;; cider-restart-nrepl is more handy than cider-jack-in, because it doesn't leave existing repls running.
  "en" 'my-cider-restart-nrepl
  "es" 'my-cider-eval-current-sexp
  "ex" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-defun-at-point))
  "er" (lambda () (interactive) (with-nrepl-connection-of-current-buffer 'cider-eval-region))
  ;; Shortcuts for printing the results of expressions. These eval functions take a second param which prints
  ;; result of the expression.
  "eps" (lambda () (interactive) (with-nrepl-connection-of-current-buffer
                                  (lambda () (my-cider-eval-current-sexp t))))
  "epx" (lambda () (interactive) (with-nrepl-connection-of-current-buffer
                                  'my-cider-eval-and-print-defun-at-point))
  "rt" (lambda ()
         (interactive)
         (with-nrepl-connection-of-current-buffer 'my-cider-run-test-at-point))
  "rT" (lambda ()
         (interactive)
         (save-buffer)
         (with-nrepl-connection-of-current-buffer 'cider-load-buffer)
         (with-nrepl-connection-of-current-buffer 'my-cider-run-tests-in-ns)))

;; Highlight parentheses in rainbow colors.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Clojure indentation rules
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (send-off 1) (cli 1) (go-loop 1)                                  ; Core
     (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2) (context 2) ; Compojure
     (select 1) (insert 1) (update 1) (where 1) (set-fields 1)         ; Korma
     (values 1) (delete 1) (upsert 1) (subselect 1)
     (clone-for 1)                                                     ; Enlive
     (up 1) (down 1) (alter 1) (table 1) (create 1)                    ; Lobos
     (checker 1)                                                         ; Midje
     (with-eligible-values 1) (when-eligible 1) (check 4)              ; Personal
     (url-of-form 1)                                                   ; Personal
     ))

(defun lisp-indent-line-single-semicolon-fix (&optional whole-exp)
  "Identical to the built-in function lisp-indent-line,
but doesn't treat single semicolons as right-hand-side comments."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
        (goto-char (- (point-max) pos))
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))
