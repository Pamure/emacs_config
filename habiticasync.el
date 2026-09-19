;;; habiticasync.el -*- lexical-binding: t; -*-

(defun abbas/habitica-sync-to-org ()
  "Intelligently sync Habitica tasks to ~/f/notes-org-mode/todo.org.
Only updates Habitica sections, preserves all other content."
  (interactive)
  (let* ((file "~/f/notes-org-mode/todo.org")
         (tasks (habitica-api-get-tasks))
         (tags-list (habitica-api-get-tags)))
    (with-current-buffer (find-file-noselect file)
      ;; Update or create each section independently
      (abbas/update-section "Habitica Habits"
                           (abbas/generate-habits tasks tags-list))
      (abbas/update-section "Habitica Dailies"
                           (abbas/generate-dailies tasks tags-list))
      (abbas/update-section "Habitica Todos"
                           (abbas/generate-todos tasks tags-list))
      (save-buffer))
    (message "Habitica sync complete!")))

(defun abbas/update-section (heading new-content)
  "Update or create a top-level section with HEADING.
Replaces existing section's content or appends if not found.
Preserves all other content in the file."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
        ;; Section exists - replace its subtree
        (progn
          (org-back-to-heading t)
          (let ((start (point)))
            ;; Find end of this top-level tree
            (org-end-of-subtree t t)
            (delete-region start (point)))
          ;; Insert new content
          (insert new-content))
      ;; Section doesn't exist - append at end
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert new-content))))

(defun abbas/generate-habits (tasks tags-list)
  "Generate complete Habits section content as a string."
  (with-temp-buffer
    (insert "* Habitica Habits\n")
    (dolist (task tasks)
      (when (string= (alist-get 'type task) "habit")
        (let* ((text (alist-get 'text task))
               (notes (alist-get 'notes task))
               (priority (alist-get 'priority task))
               (task-tags (alist-get 'tags task))
               (tag-str (abbas/build-tag-string priority task-tags tags-list)))
          (insert (format "** TODO %s%s\n"
                         text
                         (if tag-str (concat " " tag-str) "")))
          (when (and notes (not (string-empty-p notes)))
            (insert (format "%s\n" notes)))
          (insert "\n"))))
    (buffer-string)))

(defun abbas/generate-dailies (tasks tags-list)
  "Generate complete Dailies section content as a string."
  (with-temp-buffer
    (insert "* Habitica Dailies\n")
    (dolist (task tasks)
      (when (string= (alist-get 'type task) "daily")
        (let* ((text (alist-get 'text task))
               (notes (alist-get 'notes task))
               (next-due (alist-get 'nextDue task))
               (repeat (alist-get 'repeat task))
               (priority (alist-get 'priority task))
               (task-tags (alist-get 'tags task))
               (checklist (alist-get 'checklist task))
               (tag-str (abbas/build-tag-string priority task-tags tags-list))
               (schedule (abbas/build-schedule next-due repeat)))

          (insert (format "** TODO %s%s\n"
                         text
                         (if tag-str (concat " " tag-str) "")))
          (insert ":PROPERTIES:\n")
          (insert ":STYLE: habit\n")
          (insert ":REPEAT_TO_STATE: TODO\n")
          (insert ":END:\n")

          (when schedule
            (insert (format "SCHEDULED: %s\n" schedule)))

          (when (and notes (not (string-empty-p notes)))
            (insert (format "%s\n" notes)))

          (when checklist
            (dolist (item checklist)
              (insert (format "- [ ] %s\n" (alist-get 'text item)))))

          (insert "\n"))))
    (buffer-string)))

(defun abbas/generate-todos (tasks tags-list)
  "Generate complete Todos section content as a string."
  (with-temp-buffer
    (insert "* Habitica Todos\n")
    (dolist (task tasks)
      (when (string= (alist-get 'type task) "todo")
        (let* ((text (alist-get 'text task))
               (notes (alist-get 'notes task))
               (due-date (alist-get 'date task))
               (priority (alist-get 'priority task))
               (task-tags (alist-get 'tags task))
               (checklist (alist-get 'checklist task))
               (tag-str (abbas/build-tag-string priority task-tags tags-list))
               (deadline (abbas/format-date due-date)))

          (insert (format "** TODO %s%s\n"
                         text
                         (if tag-str (concat " " tag-str) "")))

          (when deadline
            (insert (format "DEADLINE: %s\n" deadline)))

          (when (and notes (not (string-empty-p notes)))
            (insert (format "%s\n" notes)))

          (when checklist
            (dolist (item checklist)
              (insert (format "- [ ] %s\n" (alist-get 'text item)))))

          (insert "\n"))))
    (buffer-string)))

(defun abbas/build-tag-string (priority task-tags tags-list)
  "Build Org tags string combining priority and Habitica tags.
Returns ':work:health:medium:' format or empty string."
  (let ((tags '()))
    ;; Add Habitica category tags
    (when task-tags
      (dolist (tag-id task-tags)
        (let ((tag-name (abbas/get-tag-name tag-id tags-list)))
          (when tag-name
            (push (abbas/sanitize-tag tag-name) tags)))))

    ;; Add priority tag
    (let ((priority-tag (abbas/priority-to-tag priority)))
      (when priority-tag
        (push priority-tag tags)))

    ;; Build tag string
    (if tags
        (format ":%s:" (string-join (reverse tags) ":"))
      "")))

(defun abbas/get-tag-name (tag-id tags-list)
  "Get tag name from tag ID using the tags list."
  (when tags-list
    (let ((tag-obj (seq-find (lambda (tag)
                               (string= (alist-get 'id tag) tag-id))
                             tags-list)))
      (when tag-obj
        (alist-get 'name tag-obj)))))

(defun abbas/sanitize-tag (tag-name)
  "Sanitize tag name for Org mode (lowercase, no spaces)."
  (when tag-name
    (downcase
     (replace-regexp-in-string "[^a-zA-Z0-9_@]" "_" tag-name))))

(defun abbas/priority-to-tag (priority)
  "Convert Habitica priority number to tag string."
  (cond
   ((= priority 0.1) "trivial")
   ((= priority 1.0) "easy")
   ((= priority 1.5) "medium")
   ((= priority 2.0) "hard")
   (t nil)))

(defun abbas/build-schedule (next-due repeat)
  "Build Org schedule string with repeater from Habitica data."
  (let* ((base-date (if (and next-due (listp next-due))
                        (car next-due)
                      next-due))
         (parsed (and base-date (parse-time-string base-date))))
    (when parsed
      (let ((date-str (format "<%04d-%02d-%02d"
                              (nth 5 parsed)
                              (nth 4 parsed)
                              (nth 3 parsed))))
        (if (and repeat (alist-get 'frequency repeat))
            (let* ((freq (alist-get 'frequency repeat))
                   (every-x (or (alist-get 'everyX repeat) 1))
                   (repeater (abbas/frequency-to-repeater freq every-x)))
              (format "%s %s>" date-str repeater))
          (format "%s +1d>" date-str))))))

(defun abbas/frequency-to-repeater (frequency every-x)
  "Convert Habitica frequency to Org repeater syntax."
  (let ((interval (if (> every-x 1) (format "%d" every-x) "")))
    (cond
     ((string= frequency "daily") (format "+%sd" interval))
     ((string= frequency "weekly") (format "+%sw" interval))
     ((string= frequency "monthly") (format "+%sm" interval))
     ((string= frequency "yearly") (format "+%sy" interval))
     (t "+1d"))))

(defun abbas/format-date (date-str)
  "Format date string to Org format."
  (when date-str
    (let ((parsed (parse-time-string date-str)))
      (when parsed
        (format "<%04d-%02d-%02d>"
                (nth 5 parsed)
                (nth 4 parsed)
                (nth 3 parsed))))))
