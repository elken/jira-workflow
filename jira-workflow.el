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
;;  Because naming things is hard
;;
;;; Code:

(require 'jiralib)

(defcustom jira-workflow-default-branch "develop"
  "Default branch to create branches from.")

(defcustom jira-workflow-harvest-code nil
  "This is designed to ONLY be set through dir-locals and as such is project-scoped.
DO NOT SET THIS ON PAIN OF DEATH.")

(defcustom jira-workflow-harvest-service-name (replace-regexp-in-string "http[s]://" "" jiralib-url)
  "The name of the service.
Used in harvest to link a ticket to a service")

(defcustom jira-workflow-harvest-service-icon-url nil
  "Service icon to use next to tickets.")

(defun jira-workflow--alist->cons (alist join-char &rest keys)
  "Convert ALIST to a cons of (KEYS . ELT) for each ELT in ALIST."
  (mapcar (lambda (elem)
            (cons
             (mapconcat (lambda (key) (format "%s" (map-nested-elt elem key))) keys join-char)
             elem))
          alist))

(defun jira-workflow--branch-name->ticket (branch-key)
  "Return a formatted branch name from BRANCH-KEY.
BRANCH-KEY is expected as 'BK-3614 Make the text colour red'"
  (let* ((tokens (string-split-words branch-key))
         (ticket-id (format "%s-%s" (pop tokens) (pop tokens)))
         (ticket-heading (mapconcat #'identity tokens "-")))
    (format "feature/%s-%s" ticket-id (jira-workflow--slugify (truncate-string-to-width ticket-heading 50)))))

(defun jira-workflow--get-harvest-code (project)
  "For a given project, get the harvest code or prompt to add one."
  (let* ((default-directory project)
         (enable-local-variables :all)
         (buffer (find-file-noselect default-directory))
         (harvest-code (prog1 (with-current-buffer buffer
                                (hack-dir-local-variables)
                                jira-workflow-harvest-code)
                         (kill-buffer buffer))))
    (unless harvest-code
      (save-window-excursion
        (setq-local harvest-code (read-from-minibuffer "Add a Harvest code: "))
        (modify-dir-local-variable
         nil
         'jira-workflow-harvest-code
         harvest-code
         'add-or-replace)
        (save-buffer)
        (kill-current-buffer)
        harvest-code))
    harvest-code))

(defun jira-workflow--notes->permalink (notes)
  "Get a valid permalink from NOTES."
  (url-encode-url (format "%s/browse/%s" jiralib-url (elt (split-string notes ":") 0))))

(defun jira-workflow--slugify (s)
  "Given a string S, convert it to a slug by downcasing and converting
    non-alpha chars to dashes."
  (downcase
   (seq-reduce
    (lambda(accum item)
      (replace-regexp-in-string (car item) (cdr item)  accum))
    '(("[^[:alnum:]]" . "-")
      ("--+"          . "-")
      ("^-"           . "")
      ("-$"           . ""))
    s)))

;;;###autoload
(defun jira-workflow-create-branch ()
  "Create a branch for a project based on completions."
  (interactive)
  (if-let* ((boards (jira-workflow--alist->cons (jiralib-get-boards) " " '(location projectName)))
            (board (cdr (assoc (completing-read "Select a board: " (mapcar #'car boards) nil t) boards)))
            (issues (jira-workflow--alist->cons
                     (jiralib-do-jql-search
                      (format
                       "project = %s AND assignee = currentuser() AND status NOT IN ('Done', 'In Review', 'Blocked') ORDER BY updated DESC"
                       (map-nested-elt board '(location projectKey))))
                     ": "
                     '(key) '(fields summary)))
            (ticket (completing-read "Select issue to branch: " (mapcar #'car issues) nil t))
            (branch (jira-workflow--branch-name->ticket ticket))
            (projects (projectile-relevant-known-projects)))
      (when projects
        (projectile-completing-read
         (format "Create '%s' for project: " branch)
         projects
         :action (lambda (project)
                   (let* ((default-directory project)
                          (branches (magit-list-local-branches))
                          (harvest-code (jira-workflow--get-harvest-code project)))
                     (jira-workflow-start-timer
                      harvest-code
                      (map-nested-elt (cdr (assoc ticket issues)) '(id))
                      (map-nested-elt board '(location projectId))
                      ticket)
                     (if (length= (-filter (lambda (local-branch)
                                             (string-match-p branch local-branch))
                                           branches)
                                  0)
                         (progn
                           (magit-checkout magit-jira-default-branch)
                           (magit-pull-from-pushremote nil)
                           (magit-branch-and-checkout branch jira-workflow-default-branch))
                       (magit-checkout branch))))))))

;;;###autoload
(defun jira-workflow-start-timer (code id group-id notes)
  "Start a timer against CODE and ID with GROUP-ID including NOTES."
  (reaper-with-buffer
   (reaper-ensure-project-tasks)
   (let* ((project (cdar (-filter (lambda (task)
                                    (string= (cdr (assoc :code (cdr task))) code))
                                  reaper-project-tasks)))
          (task-id (cdr (assoc "Development" (mapcar (lambda (task)
                                                       (cons (cdr task) (car task)))
                                                     (alist-get :tasks project)))))
          (external_reference (make-hash-table :test 'equal))
          (harvest-payload (make-hash-table :test 'equal)))
     (puthash "id" id external_reference)
     (puthash "group_id" group-id external_reference)
     (puthash "permalink" (jira-workflow--notes->permalink notes) external_reference)
     (puthash "service" jira-workflow-harvest-service-name external_reference)
     (puthash "project_id" (cdr (assoc :id project)) harvest-payload)
     (puthash "task_id" task-id harvest-payload)
     (puthash "spent_date" (format-time-string "%Y-%m-%d") harvest-payload)
     (puthash "notes" (read-from-minibuffer "Add some notes: " notes) harvest-payload)
     (puthash "external_reference" external_reference harvest-payload)
     (reaper-api "POST" "time_entries" harvest-payload "Started timer")
     (reaper--last-used project task-id)
     (reaper-refresh))))

(provide 'jira-workflow)
;;; jira-workflow.el ends here
