;;; jira-workflow.el --- Because naming things is hard -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: August 18, 2022
;; Modified: August 18, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/elken/magit-jira
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Until this gets fleshed out and abstracted a bit there's no real point adding to this.
;;
;;; Code:

(require 'jiralib)

(defgroup jira-workflow nil
  "Settings related to jira-workflow."
  :group 'vc)

(defcustom jira-workflow-default-branch "develop"
  "Default branch to create branches from."
  :type 'string
  :group 'jira-workflow)

(defcustom jira-workflow-harvest-code nil
  "DO NOT SET THIS ON PAIN OF DEATH.
This is designed to be set scoped to a project."
  :type 'string
  :group 'jira-workflow)

(defcustom jira-workflow-harvest-service-name (replace-regexp-in-string "http[s]://" "" jiralib-url)
  "The name of the service.
Used in harvest to link a ticket to a service"
  :type 'string
  :group 'jira-workflow)

(defcustom jira-workflow-harvest-service-icon-url nil
  "Service icon to use next to tickets."
  :type 'string
  :group 'jira-workflow)

(defcustom jira-workflow-timer-backend 'harvest
  "Backend to use for the timer.
Currently supports a very small list due to the package only existing for my
needs."
  :type '(choice (const harvest))
  :group 'jira-workflow)

(defcustom jira-workflow-project-ignorelist nil
  "A list of project keys to not handle.
There are a few places that require regular updates from jira, however there are
also some projects that for one reason or another we don't care about."
  :type '(repeat string)
  :group 'jira-workflow)

(defcustom jira-workflow-pretty-ui t
  "Whether or not to use pretty UI when prompting for boards and tickets."
  :type 'boolean
  :group 'jira-workflow)

(defvar jira-workflow-sprints nil
  "Alist of current sprints.")

(defvar jira-workflow--board-history nil
  "History for board selection.")

(defvar jira-workflow--ticket-history nil
  "History for board selection.")

(defvar jira-workflow-image-locations nil
  "Alist of image locations in the format of (project-key . path).")

(defvar jira-workflow---annotation-cache nil
  "Cache for annotations.
Because the affix function recomputes the whole list every time a new candidate
is selected, we opt to cache the list of annotations for the lifetime of the
`completing-read' invocation.")

(defface jira-workflow-key-face
  '((t (:inherit font-lock-constant-face
        :weight bold)))
  "Face for the project key in the prompt."
  :group 'jira-workflow)

(defface jira-workflow-progress-face
  '((t (:inherit diary
        :weight bold)))
  "Face for the project key in the prompt."
  :group 'jira-workflow)

(defface jira-workflow-blocked-face
  '((t (:inherit error
        :weight bold)))
  "Face for the project key in the prompt."
  :group 'jira-workflow)

(defun jira-workflow--alist->cons (alist &rest keys)
  "Convert ALIST to a cons of (KEYS . ELT) for each ELT in ALIST.
Use JOIN-CHAR to format a prompt for multiple keys."
  (mapcar (lambda (elem)
            (cons
             (mapconcat
              (lambda (key) (format "%s" (map-nested-elt elem key)))
              keys
              " ")
             elem))
          alist))

(defun jira-workflow--branch-name->ticket (issue)
  "Return a formatted branch name from ISSUE."
  (let* ((ticket-id (cdr (assoc 'key issue)))
         (ticket-heading (map-nested-elt issue '(fields summary))))
    (format "feature/%s-%s" ticket-id (jira-workflow--slugify (truncate-string-to-width ticket-heading 50)))))

(defun jira-workflow--ticket->project (ticket)
  "Get a board from a TICKET."
  (map-nested-elt ticket '(fields project)))

(defun jira-workflow--get-harvest-code (project)
  "For a given PROJECT, get the harvest code or prompt to add one."
  (let* ((default-directory project)
         (enable-local-variables :all)
         (buffer (find-file-noselect default-directory))
         (harvest-code (prog1 (with-current-buffer buffer
                                (hack-dir-local-variables)
                                jira-workflow-harvest-code)
                         (kill-buffer buffer)))
         (prompt (format "Add a Harvest code%s: " (if harvest-code (format " (default %s)" harvest-code) ""))))
    (read-from-minibuffer prompt nil nil t nil harvest-code)))

(defun jira-workflow--key->permalink (issue-key)
  "Get a valid permalink from ISSUE-KEY."
  (url-encode-url (format "%s/browse/%s" jiralib-url issue-key)))

(defun jira-workflow--slugify (s)
  "Convert string S to a slug.
This is done by downcasing and converting non-alpha chars to dashes."
  (downcase
   (seq-reduce
    (lambda (accum item)
      (replace-regexp-in-string (car item) (cdr item)  accum))
    '(("[^[:alnum:]]" . "-")
      ("--+"          . "-")
      ("^-"           . "")
      ("-$"           . ""))
    s)))

(defun jira-workflow--get-latest-sprint (board)
  "Get the latest sprint for BOARD."
  (if-let ((sprint (cdr (assoc (map-nested-elt board '(location projectKey)) jira-workflow-sprints))))
      (prog1 sprint
        (when (and
               (string= (cdr (assoc 'state sprint)) "active")
               (>
                (time-to-seconds (current-time))
                (time-to-seconds (date-to-time (cdr (assoc 'endDate sprint))))))
          (setq jira-workflow-sprints (delete (assoc (map-nested-elt board '(location projectKey)) jira-workflow-sprints) jira-workflow-sprints))))
    (when (string= (cdr (assoc 'type board)) "scrum")
      (let ((sprints (thread-last
                       (jiralib-get-board-sprints (cdr (assoc 'id board)))
                       (assoc 'values)
                       cdr
                       nreverse)))
        (when (length> sprints 0)
          (add-to-list 'jira-workflow-sprints (cons (map-nested-elt board '(location projectKey)) (aref sprints 0)))
          (aref sprints 0))))))

(defun jira-workflow--get-sprint-date (sprint date)
  "Get the DATE for the SPRINT."
  (when-let ((date-string (cdr (assoc date sprint))))
    (car (split-string date-string "T"))))

(defun jira-workflow--get-logo-for-project (project-key)
  "Given a PROJECT-KEY, return the propertized logo.
:ascent center is required to ensure that the image is anchored to the center of
the frame."
  (when (cdr (assoc project-key jira-workflow-image-locations))
    (propertize
     " "
     'display
     `(image
       :margin (2 . 2)
       :ascent center
       :width 20
       :type ,(image-type (cdr (assoc project-key jira-workflow-image-locations)))
       :file ,(cdr (assoc project-key jira-workflow-image-locations))))))


(defun jira-workflow--board-affix-function (cands)
  "Given a list of CANDS, transform into a more detailed row.
For each candidate:
- Prefix a logo from the board (see `jira-workflow-image-locations')
- Include the key and current sprint info"
  (cl-loop
   for cand in cands
   collect
   (let* ((max-length (cl-loop for board in cands maximize (length board)))
          (column-start (+ 5 max-length))
          (board (cdr (assoc cand minibuffer-completion-table)))
          (project-key (map-nested-elt board '(location projectKey))))
     (unless (cdr (assoc project-key jira-workflow---annotation-cache))
       (unless (cdr (assoc project-key jira-workflow-image-locations))
         (let ((url-request-extra-headers `(,jiralib-token))
               (file (concat
                      (temporary-file-directory)
                      (make-temp-name project-key)
                      ".png")))
           (url-copy-file (map-nested-elt board '(location avatarURI)) file)
           (add-to-list 'jira-workflow-image-locations (cons project-key file))))
       (add-to-list
        'jira-workflow---annotation-cache
        (cons
         project-key
         (list
          cand
          (jira-workflow--get-logo-for-project project-key)
          (concat
           (string-pad " " (- column-start (length cand)))
           (propertize project-key 'face 'jira-workflow-key-face)
           (string-pad " " (- 10 (length project-key)))
           (when-let* ((latest-sprint (jira-workflow--get-latest-sprint board))
                       (name (cdr (assoc 'name latest-sprint))))
             (concat
              name
              (string-pad " " (- 20 (length name)))
              (jira-workflow--get-sprint-date latest-sprint 'startDate)
              " - "
              (jira-workflow--get-sprint-date latest-sprint 'endDate))))))))
     (cdr (assoc project-key jira-workflow---annotation-cache)))))

(defun jira-workflow--ticket-affix-function (cands)
  "Given a list of CANDS, transform into a more detailed row.
For each candidate:
- Prefix a logo from the board (see `jira-workflow-image-locations')
- Include the key,a coloured status after the candidate and the priority icon"
  (cl-loop
   for cand in cands
   collect
   (let* ((max-length (cl-loop for cand in cands maximize (length cand)))
          (column-start (+ 5 max-length))
          (ticket (cdr (assoc cand minibuffer-completion-table)))
          (project-key (map-nested-elt ticket '(fields project key)))
          (issue-key (alist-get 'key ticket))
          (status (map-nested-elt ticket '(fields status name))))
     (unless (cdr (assoc issue-key jira-workflow---annotation-cache))
       (unless (cdr (assoc project-key jira-workflow-image-locations))
         (let ((url-request-extra-headers `(,jiralib-token))
               (file (concat
                      (temporary-file-directory)
                      (make-temp-name project-key)
                      ".png")))
           (url-copy-file (map-nested-elt ticket '(fields project avatarUrls 48x48)) file)
           (add-to-list 'jira-workflow-image-locations (cons project-key file))))
       (add-to-list
        'jira-workflow---annotation-cache
        (cons
         issue-key
         (list
          cand
          (jira-workflow--get-logo-for-project project-key)
          (concat
           (string-pad " " (- column-start (length cand)))
           (propertize issue-key 'face 'jira-workflow-key-face)
           (string-pad " " (- 10 (length issue-key)))
           (propertize
            status
            'face
            (pcase status
              ("In Progress" 'jira-workflow-progress-face)
              ("Blocked" 'jira-workflow-blocked-face)
              (_ 'jira-workflow-key-face))))))))
     (cdr (assoc issue-key jira-workflow---annotation-cache)))))

(defun jira-workflow-read-board ()
  "Read a board interactively."
  (interactive)
  (setq jira-workflow---annotation-cache nil)
  (let* ((boards (jira-workflow--alist->cons
                  (if jira-workflow-project-ignorelist
                      (-filter (lambda (board)
                                 (member (map-nested-elt board '(location projectKey)) jira-workflow-project-ignorelist))
                               (jiralib-get-boards))
                    (jiralib-get-boards))
                  '(location projectName)))
         (completion-extra-properties
          (when jira-workflow-pretty-ui
            '(:affixation-function jira-workflow--board-affix-function))))
    (cdr (assoc (completing-read "Select a board: " boards nil t nil jira-workflow--board-history) boards))))

(defun jira-workflow-read-ticket (board &optional arg)
  "Read a ticket for a BOARD.
Optionally prompt for a board when ARG is set."
  (interactive (list (jira-workflow-read-board) current-prefix-arg))
  (setq jira-workflow---annotation-cache nil)
  (let* ((tickets (jira-workflow--alist->cons
                   (jiralib-do-jql-search
                    (format
                     "project = %s AND assignee = currentuser() AND status NOT IN ('Done', 'In Review') ORDER BY updated DESC"
                     (map-nested-elt board '(location projectKey))))
                   '(fields summary)))
         (completion-extra-properties
          (when jira-workflow-pretty-ui
            '(:affixation-function jira-workflow--ticket-affix-function))))
    (cdr (cdr (assoc (completing-read "Select a ticket: " tickets nil t nil jira-workflow--ticket-history) tickets)))))

;;;###autoload
(defun jira-workflow-create-branch (&optional callback)
  "Prompt for a board, ticket and a project and do the following:
- Pull `jira-workflow-default-branch' for that project
- Create a properly named branch in that project
- Optionally call a CALLBACK taking the selected ticket and project
- Move the issue into In Progress on jira TODO
- Switch to a workspace for that ticket TODO"
  (interactive)
  (when-let* ((ticket (call-interactively #'jira-workflow-read-ticket))
              (branch (jira-workflow--branch-name->ticket ticket)))
    (projectile-completing-read
     "Use project: "
     (projectile-load-known-projects)
     :action (lambda (project)
               (let* ((default-directory project)
                      (branches (magit-list-local-branches)))
                 (funcall callback ticket project)
                 (if (length= (-filter (lambda (local-branch)
                                         (string-match-p branch local-branch))
                                       branches)
                              0)
                     (progn
                       (magit-checkout jira-workflow-default-branch)
                       (magit-pull-from-pushremote nil)
                       (magit-branch-and-checkout branch jira-workflow-default-branch))
                   (magit-checkout branch)))))))

;;;###autoload
(defun jira-workflow-start-ticket ()
  "The main entry point."
  (interactive)
  (jira-workflow-create-branch #'jira-workflow-create-timer))

;; Timers

(defun jira-workflow--get-current-harvest-entry-id (task-id external-reference-id)
  "Attempt to get an entry id for a ticket by TASK-ID and EXTERNAL-REFERENCE-ID."
  (reaper-with-buffer
   (reaper-ensure-project-tasks)
   (let* ((response
           (cdr (assoc 'time_entries
                       (reaper-api
                        "GET"
                        (concat
                         (format
                          "time_entries?task_id=%d&updated_since=%s&external_reference_id=%d"
                          task-id
                          (format-time-string "%Y-%m-%d")
                          external-reference-id))
                        nil
                        nil)))))
     (when (length> response 0)
       (cdr (assoc 'id (aref response 0)))))))

(defun jira-workflow--start-harvest-timer (ticket project)
  "Start a timer in harvest against TICKET and PROJECT."
  (reaper-with-buffer
   (reaper-ensure-project-tasks)
   (let* ((code (jira-workflow--get-harvest-code project))
          (id (map-nested-elt ticket '(id)))
          (notes (map-nested-elt ticket '(fields summary)))
          (harvest-project (cdar (-filter (lambda (task)
                                            (string= (cdr (assoc :code (cdr task))) code))
                                          reaper-project-tasks)))
          (task-id (cdr (assoc "Development" (mapcar (lambda (task)
                                                       (cons (cdr task) (car task)))
                                                     (alist-get :tasks harvest-project)))))
          (external_reference (make-hash-table :test 'equal))
          (harvest-payload (make-hash-table :test 'equal)))
     (if-let ((entry-id (jira-workflow--get-current-harvest-entry-id task-id id)))
         (reaper-api "PATCH" (format "time_entries/%d/restart" entry-id) nil (format "[jira-workflow] Restarted timer for %s" notes))
       (puthash "id" id external_reference)
       (puthash "group_id" (cdr (assoc 'id (jira-workflow--ticket->project ticket))) external_reference)
       (puthash "permalink" (jira-workflow--key->permalink (map-nested-elt ticket '(key))) external_reference)
       (puthash "service" jira-workflow-harvest-service-name external_reference)
       (puthash "project_id" (cdr (assoc :id harvest-project)) harvest-payload)
       (puthash "task_id" task-id harvest-payload)
       (puthash "spent_date" (format-time-string "%Y-%m-%d") harvest-payload)
       (puthash "notes" (read-from-minibuffer "Add some notes: " notes) harvest-payload)
       (puthash "external_reference" external_reference harvest-payload)
       (reaper-api "POST" "time_entries" harvest-payload (format "[jira-workflow] Started timer for %s" notes)))
     (reaper--last-used harvest-project task-id)
     (reaper-refresh))))

;;;###autoload
(defun jira-workflow-create-timer (ticket project &optional arg)
  "Create a timer for TICKET and PROJECT.
Run interactively to prompt for a ticket to use with prefix ARG."
  (interactive (list
                (call-interactively #'jira-workflow-read-ticket)
                (projectile-completing-read
                 "Use project: "
                 (projectile-load-known-projects))
                current-prefix-arg))
  (pcase jira-workflow-timer-backend
    ('harvest
     (unless (require 'reaper nil t)
       (user-error "Harvest selected as the timer but reaper is missing"))
     (jira-workflow--start-harvest-timer ticket project))
    (_ (user-error "Invalid backend '%s' selected, no timer created" jira-workflow-timer-backend))))

(provide 'jira-workflow)
;;; jira-workflow.el ends here
