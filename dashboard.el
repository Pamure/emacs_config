;; ============================================================
;;  PAM — Doom Dashboard  ·  murtaza · pamure
;;  Place in ~/.config/doom/config.el  (or require from it)
;; ============================================================

;; ── 0. Suppress invalid-value warning ───────────────────────
;; '+doom-dashboard-pwd-policy' only accepts nil / 'default / a fn.
;; 'home was causing the repeated warning you saw — remove it.
;; (the default behaviour is fine)

;; ── 1. Theme fix ────────────────────────────────────────────
(after! doom-themes
  (setq doom-themes-treemacs-theme "doom-colors"))

;; ── 2. Faces ────────────────────────────────────────────────
(defface pam/header-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Dashboard header face.")

(defface pam/key-face
  '((t (:inherit font-lock-string-face :weight bold)))
  "Dashboard key face.")

(defface pam/desc-face
  '((t (:inherit shadow)))
  "Dashboard description face.")

(defface pam/section-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Dashboard section title face.")

(defface pam/todo-done-face
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Completed TODO face.")

(defface pam/todo-face
  '((t (:inherit font-lock-warning-face)))
  "Pending TODO face.")

;; ── 3. Banner ────────────────────────────────────────────────
(setq +doom-dashboard-ascii-banner-fn
      (lambda ()
        (let* ((banner
                '("  ██████╗  █████╗ ███╗   ███╗  "
                  "  ██╔══██╗██╔══██╗████╗ ████║  "
                  "  ██████╔╝███████║██╔████╔██║  "
                  "  ██╔═══╝ ██╔══██║██║╚██╔╝██║  "
                  "  ██║     ██║  ██║██║ ╚═╝ ██║  "
                  "  ╚═╝     ╚═╝  ╚═╝╚═╝     ╚═╝  "
                  "                                "
                  "      murtaza  ·  pamure        "))
               (longest (apply #'max (mapcar #'length banner))))
          (put-text-property
           (point)
           (dolist (line banner (point))
             (insert (+doom-dashboard--center
                      +doom-dashboard--width
                      (concat line (make-string (max 0 (- longest (length line))) ?\s)))
                     "\n"))
           'face 'doom-dashboard-banner))))

;; ── 4. Dashboard width ───────────────────────────────────────
(setq +doom-dashboard--width 90)

;; ── 5. Quick-action menu ─────────────────────────────────────
(setq +doom-dashboard-menu-sections
      '(("Find file"
         :icon (nerd-icons-faicon "nf-fa-search" :face 'doom-dashboard-menu-title)
         :action find-file)

        ("Recent files"
         :icon (nerd-icons-faicon "nf-fa-history" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)

        ("Switch project"
         :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)

        ("Open bookmark"
         :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)

        ("Doom config"
         :icon (nerd-icons-faicon "nf-fa-cog" :face 'doom-dashboard-menu-title)
         :action doom/open-private-config)

        ("Doom docs"
         :icon (nerd-icons-faicon "nf-fa-book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

;; ── 6. Helper: centered insert with face ────────────────────
(defun pam/dash-insert-centered (str &optional face)
  "Insert STR centered in the dashboard, optionally with FACE."
  (let ((line (+doom-dashboard--center +doom-dashboard--width str)))
    (if face
        (insert (propertize line 'face face) "\n")
      (insert line "\n"))))

;; ── 7. Helper: ruled section header ─────────────────────────
(defun pam/dash-section (title)
  (let* ((rule-char "─")
         (total +doom-dashboard--width)
         (inner (concat "  " title "  "))
         (side  (max 4 (/ (- total (length inner)) 2)))
         (rule  (make-string side (string-to-char rule-char)))
         (line  (concat rule inner rule)))
    (insert "\n" (propertize
                  (+doom-dashboard--center +doom-dashboard--width line)
                  'face 'pam/section-face) "\n\n")))

;; ── 8. Keybinding table ──────────────────────────────────────
;;
;;  Each entry: (key . description)
;;  Columns are rendered side-by-side so the table is compact.
;;
(defun pam/dash-keybind-table (rows &optional cols)
  "Render ROWS of (key . desc) pairs in COLS columns (default 2)."
  (let* ((cols  (or cols 2))
         (w     (/ (- +doom-dashboard--width 4) cols))
         (key-w 16)
         (desc-w (- w key-w 4))
         (chunks (seq-partition rows cols)))
    (dolist (chunk chunks)
      (let ((line "  "))
        (dolist (cell chunk)
          (let* ((k    (propertize (format (format "%%-%ds" key-w) (car cell)) 'face 'pam/key-face))
                 (d    (propertize (truncate-string-to-width (cdr cell) desc-w) 'face 'pam/desc-face))
                 (col  (format "%s %s  " k d)))
            (setq line (concat line col))))
        (insert (+doom-dashboard--center +doom-dashboard--width
                                         (string-trim-right line)) "\n")))
    (insert "\n")))

;; ── 9. Org todos widget ──────────────────────────────────────
(defun pam/dash-org-todos ()
  "Insert pending org-mode TODO items from agenda files."
  (when (and (featurep! :lang org)
             org-agenda-files)
    (pam/dash-section "ORG  TODOS")
    (let ((items '())
          (limit 8))
      ;; Collect TODOs from agenda files
      (ignore-errors
        (dolist (file (org-agenda-files))
          (when (file-readable-p file)
            (with-current-buffer (or (get-file-buffer file)
                                     (find-file-noselect file t))
              (org-map-entries
               (lambda ()
                 (let* ((state  (org-get-todo-state))
                        (hdg    (org-get-heading t t t t))
                        (sched  (org-entry-get nil "SCHEDULED"))
                        (dead   (org-entry-get nil "DEADLINE")))
                   (when (and state (member state '("TODO" "NEXT" "INPROGRESS" "WAITING")))
                     (push (list state hdg sched dead) items))))
               "TODO<>\"DONE\"|TODO<>\"CANCELLED\""
               'agenda))))))
      (if (null items)
          (pam/dash-insert-centered "  No pending TODOs  ✓" 'pam/desc-face)
        (let ((count 0))
          (dolist (item (seq-take (nreverse items) limit))
            (cl-incf count)
            (let* ((state   (car item))
                   (heading (cadr item))
                   (sched   (caddr item))
                   (icon    (cond ((equal state "NEXT")       "▶")
                                  ((equal state "INPROGRESS") "◉")
                                  ((equal state "WAITING")    "⏸")
                                  (t                          "○")))
                   (sfx     (if sched
                                (propertize (concat "  📅 " (substring sched 1 11)) 'face 'pam/desc-face)
                              ""))
                   (state-face (if (equal state "DONE") 'pam/todo-done-face 'pam/todo-face))
                   (line    (format "  %s  %s  %s"
                                    (propertize (format "%-12s" (concat icon " " state)) 'face state-face)
                                    (truncate-string-to-width heading 48)
                                    sfx)))
              (insert (+doom-dashboard--center +doom-dashboard--width line) "\n")))
          (when (> (length items) limit)
            (pam/dash-insert-centered
             (format "  … and %d more" (- (length items) limit))
             'pam/desc-face))))
      (insert "\n")))

;; ── 10. Projectile projects widget ───────────────────────────
(defun pam/dash-projects ()
  "Insert recent Projectile projects."
  (when (featurep! :project projectile)
    (pam/dash-section "RECENT  PROJECTS")
    (let* ((projects (ignore-errors
                       (projectile-relevant-known-projects)))
           (limit    8)
           (shown    (seq-take projects limit)))
      (if (null shown)
          (pam/dash-insert-centered "  No projects found" 'pam/desc-face)
        (dolist (proj shown)
          (let* ((name  (file-name-nondirectory (directory-file-name proj)))
                 (path  (abbreviate-file-name proj))
                 (line  (format "  %-28s  %s"
                                (propertize name 'face 'pam/key-face)
                                (propertize path 'face 'pam/desc-face))))
            (insert (+doom-dashboard--center +doom-dashboard--width line) "\n")))
        (when (> (length projects) limit)
          (pam/dash-insert-centered
           (format "  … and %d more  (SPC p p to switch)" (- (length projects) limit))
           'pam/desc-face))))
    (insert "\n")))

;; ── 11. Vim / Doom keybindings cheatsheet ────────────────────
(defun pam/dash-keybindings ()
  "Insert a two-column keybinding reference card."
  (pam/dash-section "VIM  MOTIONS")
  (pam/dash-keybind-table
   '(("h j k l"      . "← ↓ ↑ →  character")
     ("w / b"         . "Next / prev word")
     ("W / B"         . "Next / prev WORD")
     ("e / ge"        . "End of word fwd/bwd")
     ("0 / ^"         . "Line col-0 / first non-blank")
     ("$ / g_"        . "Line end / last non-blank")
     ("gg / G"        . "File top / bottom")
     ("{ / }"         . "Prev / next paragraph")
     ("%"             . "Jump matching bracket")
     ("C-d / C-u"     . "Half page down / up")
     ("C-f / C-b"     . "Full page down / up")
     ("zz / zt / zb"  . "Center / top / bottom view"))
   2)

  (pam/dash-section "EDITING")
  (pam/dash-keybind-table
   '(("i / I"         . "Insert before cursor / line start")
     ("a / A"         . "Append after / line end")
     ("o / O"         . "New line below / above")
     ("x / X"         . "Delete char fwd / bwd")
     ("dd / D"        . "Delete line / to EOL")
     ("yy / Y"        . "Yank line")
     ("p / P"         . "Paste after / before")
     ("u / C-r"       . "Undo / Redo")
     ("c{motion}"     . "Change  (delete → insert)")
     ("r{char}"       . "Replace single char")
     ("."             . "Repeat last change")
     (">"  / "<"      . "Indent / dedent"))
   2)

  (pam/dash-section "SEARCH  &  REPLACE")
  (pam/dash-keybind-table
   '(("/{pat}"        . "Search forward")
     ("?{pat}"        . "Search backward")
     ("n / N"         . "Next / prev match")
     ("* / #"         . "Word under cursor fwd / bwd")
     (":s/a/b/g"      . "Replace in line")
     (":%s/a/b/gc"    . "Replace in file (confirm)"))
   2)

  (pam/dash-section "DOOM  EMACS  BINDINGS")
  (pam/dash-keybind-table
   '(("SPC ."         . "Find file")
     ("SPC ,"         . "Switch buffer")
     ("SPC /"         . "Search project")
     ("SPC SPC"       . "M-x (command palette)")
     ("SPC p p"       . "Switch project")
     ("SPC p f"       . "Find file in project")
     ("SPC g g"       . "Magit status")
     ("SPC g b"       . "Git blame")
     ("SPC b k"       . "Kill buffer")
     ("SPC b n / p"   . "Next / prev buffer")
     ("SPC w v"       . "Split right")
     ("SPC w s"       . "Split below")
     ("SPC w w"       . "Focus other window")
     ("SPC w d"       . "Close window")
     ("SPC o t"       . "Toggle vterm")
     ("SPC t t"       . "Toggle theme")
     ("SPC h d f"     . "Describe function")
     ("SPC h d v"     . "Describe variable")
     ("SPC h d k"     . "Describe key")
     ("SPC f s"       . "Save file")
     ("SPC q q"       . "Quit Emacs"))
   2))

;; ── 12. Footer ───────────────────────────────────────────────
(defun pam/dash-footer ()
  (insert "\n")
  (pam/dash-insert-centered
   (format "Doom %s  ·  Emacs %s  ·  %s packages"
           (or (bound-and-true-p doom-version) "?")
           emacs-version
           (length package-activated-list))
   'pam/desc-face)
  (insert "\n"))

;; ── 13. Wire everything into the dashboard ───────────────────
;;
;;  Order matters — functions run in the order they appear in the hook.
;;  We clear the hook and rebuild it so the order is explicit.
;;
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        pam/dash-org-todos          ;; org TODOs
        pam/dash-projects           ;; projectile projects
        pam/dash-keybindings        ;; vim / doom reference card
        pam/dash-footer))

;; ── 14. Refresh on reload ────────────────────────────────────
(add-hook! 'doom-after-reload-hook #'+doom-dashboard-reload)
