;;; mq-org-mode --- Helpers for configuring org mode.
;;; Commentary:
;;;   This file contains some helpers for configuring org mode.

;;; Code:
(provide 'mq-org-mode)

(defun mq/org-configure ()
  (evil-leader/set-key
    "oo" (lambda () (interactive) (find-file "~/Dropbox/org/tasks.org"))))

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

(defun mq/preview-org (beg end)
  "Pipes the buffer's contents into a script which renders the markdown as HTML and opens in a browser."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max))))
    (call-process-region beg end "/bin/bash" nil nil nil "-c"
                         "tee /Users/mikeq/md.out | convert_org_to_markdown.rb | markdown_page.rb --css gmail | browser")))

    ;; This convert_org_to_markdown.rb is a primitive script I've written which fits my needs.
    ;; (call-process-region beg end "/bin/bash" nil nil nil "-c"
    ;;                      "/Users/mikeq/bin/convert_org_to_markdown.rb | /Users/mikeq/bin/markdown_page.rb --css gmail > /Users/mikeq/out")))
;;"convert_org_to_markdown.rb | markdown_page.rb --css gmail | browser")))
