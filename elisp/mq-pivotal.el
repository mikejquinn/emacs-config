;;; A few functions that leverage the pivotal API -*- lexical-binding: t; -*-
;;; https://www.pivotaltracker.com/help/api/

(require 'json)
(require 'subr-x)
(require 'org-table)

(defconst pivotal-api-key "c0d80a1ecd236cc8b376c687a8ae8ca8")

(defun pivotal-request (url &optional params)
  (let ((url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-key)))
        (url (if params
                 (concat url "?" (url-build-query-string params))
               url))
        (json-array-type 'list))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (+ url-http-end-of-headers 1))
      (json-read))))

(defun pivotal-project-id (project-name)
  (let* ((projects (pivotal-request "https://www.pivotaltracker.com/services/v5/projects"))
         (matching-projects (seq-filter (lambda (p)
                                          (string= (cdr (assq 'name p)) project-name))
                                        projects)))
    (if (null matching-projects)
        (signal 'pivotal "Unknown project")
      (cdr (assq 'id (car matching-projects))))))

(defun pivotal-sprint-notes (project)
  "Insert the sprint notes summary as markdown into the current buffer."
  (interactive (list (read-string "Project: " "Data Infra")))
  (let* ((project-id (pivotal-project-id project))
         (me (pivotal-request "https://www.pivotaltracker.com/services/v5/me"))
         (others (let ((resp (pivotal-request "https://www.pivotaltracker.com/services/v5/my/people"
                                              `((project_id ,project-id)))))
                   (mapcar (lambda (kp) (cdr (assq 'person kp))) resp)))
         (id->initials (cl-reduce (lambda (m p)
                                    (cons (cons (cdr (assq 'id p)) (upcase (cdr (assq 'initials p)))) m))
                                  (cons me others)
                                  :initial-value nil)))
    (cl-flet* ((list-stories-in-iteration
                (scope offset)
                (let* ((url (format "https://www.pivotaltracker.com/services/v5/projects/%d/iterations"
                                    project-id))
                       (resp (pivotal-request url `((limit 1) (offset ,offset) (scope ,scope)))))
                  (cdr (assq 'stories (car resp)))))
               (format-story
                (story)
                (format "  - %s (%s)"
                        (cdr (assq 'name story))
                        (string-join (mapcar (lambda (id)
                                               (cdr (assq id id->initials)))
                                             (cdr (assq 'owner_ids story)))
                                     ",")))
               (insert-section
                (header stories)
                (insert "__" header "__")
                (newline)
                (dolist (st stories)
                  (insert (format-story st))
                  (newline))
                (newline)))
      (insert-section "Select completed stories"
                      (list-stories-in-iteration "done" -1))
      (insert-section "Select ongoing stories"
                      (list-stories-in-iteration "current" 0)))))

(defun pivotal-velocity (project user since)
  "Return an org-mode table in list form summarizing velocity by month for
the user on the given project. The `since` arg should be in the
format 2017-01-01. This is intended to be called from an org-mode code block like this:

#+BEGIN_SRC emacs-lisp
  (pivotal-velocity 'ML' 'Sean' '2017-01-01')
#+END_SRC
"
  (let* ((project-id (pivotal-project-id project))
         (url (format "https://www.pivotaltracker.com/services/v5/projects/%d/stories" project-id))
         (filter-fmt "state:accepted AND type:feature,bug,chore AND accepted_since:%s AND owner:%s")
         ;; Set a high limit to the number of stories by the query to avoid pagination.
         (stories (pivotal-request url `((limit 1000)
                                         (filter ,(format filter-fmt since user)))))
         (m (make-hash-table :test 'equal))
         (lst nil))
    (dolist (story stories)
      (let* ((month (format-time-string "%Y-%m" (date-to-time (cdr (assq 'accepted_at story)))))
             (type (cdr (assq 'story_type story)))
             (e (or (gethash month m) (list 0 0 0))))
        (cond
         ((equal type "feature") (setf (car e) (+ (car e) (cdr (assq 'estimate story)))))
         ((equal type "chore") (setf (cadr e) (1+ (cadr e))))
         ((equal type "bug") (setf (nth 2 e) (1+ (nth 2 e))))
         (t (message "foo")))
        (puthash month e m)))
    (maphash (lambda (k v)
               (setq lst (cons (cons k v) lst)))
             m)
    (let ((totals (cl-reduce (lambda (g c)
                               (list (+ (nth 0 g) (nth 0 c))
                                     (+ (nth 1 g) (nth 1 c))
                                     (+ (nth 2 g) (nth 2 c))))
                             (hash-table-values m)
                             :initial-value (list 0 0 0))))
      (append '((month points chores bugs))
              '(hline)
              (sort lst (lambda (a b) (string-greaterp (car b) (car a))))
              '(hline)
              `((totals . ,totals))))))

(provide 'mq-pivotal)
