#!/usr/bin/env bash
set -e

ACTUAL_HOME="$HOME"
ACTUAL_USER="$(whoami)"

sudo apt-get update -y
sudo apt-get install -y \
    git curl wget unzip zip \
    build-essential cmake pkg-config \
    libtool libtool-bin \
    zsh \
    python3 python3-pip python3-venv \
    default-jdk \
    gcc g++ \
    clangd \
    ripgrep \
    fd-find \
    sqlite3 libsqlite3-dev \
    texlive-xetex texlive-latex-extra texlive-fonts-recommended \
    python3-pygments \
    mysql-server mysql-client libmysqlclient-dev \
    docker.io \
    xclip xsel \
    fontconfig \
    libvterm-dev \
    libtool-bin

mkdir -p "$ACTUAL_HOME/.local/share/fonts/JetBrainsMono"
cd /tmp
wget -q "https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip" -O JetBrainsMono.zip
unzip -o JetBrainsMono.zip -d "$ACTUAL_HOME/.local/share/fonts/JetBrainsMono/"
fc-cache -fv

curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs

sudo npm install -g pyright
sudo npm install -g vscode-langservers-extracted
sudo npm install -g graphql-language-service-cli
sudo npm install -g sql-language-server
sudo npm install -g typescript typescript-language-server

GO_VERSION="1.22.4"
cd /tmp
wget -q "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz" -O go.tar.gz
sudo rm -rf /usr/local/go
sudo tar -C /usr/local -xzf go.tar.gz
export PATH="$PATH:/usr/local/go/bin:$ACTUAL_HOME/go/bin"

echo 'export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"' >> "$ACTUAL_HOME/.bashrc"
echo 'export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"' >> "$ACTUAL_HOME/.zshrc" 2>/dev/null || true

/usr/local/go/bin/go install golang.org/x/tools/gopls@latest

cd /tmp
wget -q "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh" -O miniconda.sh
bash miniconda.sh -b -p "$ACTUAL_HOME/miniconda3"
"$ACTUAL_HOME/miniconda3/bin/conda" init bash
"$ACTUAL_HOME/miniconda3/bin/conda" init zsh 2>/dev/null || true
"$ACTUAL_HOME/miniconda3/bin/conda" create -n torchgpu python=3.11 -y
"$ACTUAL_HOME/miniconda3/bin/conda" run -n torchgpu pip install jupyter jupyterlab torch torchvision

mkdir -p "$ACTUAL_HOME/downloads/dev"
git clone https://github.com/flutter/flutter.git -b stable "$ACTUAL_HOME/downloads/dev/flutter"
"$ACTUAL_HOME/downloads/dev/flutter/bin/flutter" precache
echo "export PATH=\"\$PATH:$ACTUAL_HOME/downloads/dev/flutter/bin\"" >> "$ACTUAL_HOME/.bashrc"
echo "export PATH=\"\$PATH:$ACTUAL_HOME/downloads/dev/flutter/bin\"" >> "$ACTUAL_HOME/.zshrc" 2>/dev/null || true

chsh -s "$(which zsh)" "$ACTUAL_USER" || true

mkdir -p "$ACTUAL_HOME/.config/doom/lisp"
mkdir -p "$ACTUAL_HOME/.config/doom/snippets"

git clone https://github.com/rejeep/drag-stuff.el.git "$ACTUAL_HOME/.config/doom/lisp/drag-stuff"

touch "$ACTUAL_HOME/.config/doom/custom.el"

cat > "$ACTUAL_HOME/.config/doom/packages.el" << 'PACKAGES_EOF'
;;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! corfu-terminal)
(package! habitica
  :recipe (:host github :repo "abrochard/emacs-habitica"))
(package! conda)
(package! jupyter)
PACKAGES_EOF

cat > "$ACTUAL_HOME/.config/doom/init.el" << 'INIT_EOF'
;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (corfu +orderless)
       vertico

       :ui
       doom
       doom-dashboard
       indent-guides
       modeline
       treemacs
       tabs
       vi-tilde-fringe
       window-select

       :editor
       (evil)
       fold
       rotate-text
       snippets

       :emacs
       electric
       undo
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       (debugger +lsp)
       docker
       ein
       (eval +overlay)
       lookup
       lsp
       magit
       pdf
       (eval +overlay)

       :os
       tty

       :lang
       (cc +lsp)
       (dart +flutter +lsp)
       emacs-lisp
       (go +lsp)
       (graphql +lsp)
       json
       (java +lsp)
       javascript
       kotlin
       markdown
       (org +jupyter +pomodoro +pretty)
       (python +lsp +pyright)
       (rest +restclient)
       sh
       (web +lsp)
       yaml
       (sql +lsp +mysql)

       :email

       :app

       :config
       (default +bindings))
INIT_EOF

cat > "$ACTUAL_HOME/.config/doom/casync.el" << 'CA_EOF'
;;; casync.el -*- lexical-binding: t; -*-

(defun abbas/ca-sync-to-org ()
  "Intelligently sync ca tasks to ~/f/notes-org-mode/todo.org."
  (interactive)
  (let* ((file "~/f/notes-org-mode/todo.org")
         (tasks (ca-api-get-tasks))
         (tags-list (ca-api-get-tags)))
    (with-current-buffer (find-file-noselect file)
      (abbas/update-section "ca Habits"
                           (abbas/generate-habits tasks tags-list))
      (abbas/update-section "ca Dailies"
                           (abbas/generate-dailies tasks tags-list))
      (abbas/update-section "ca Todos"
                           (abbas/generate-todos tasks tags-list))
      (save-buffer))
    (message "ca sync complete!")))

(defun abbas/update-section (heading new-content)
  "Update or create a top-level section with HEADING."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
        (progn
          (org-back-to-heading t)
          (let ((start (point)))
            (org-end-of-subtree t t)
            (delete-region start (point)))
          (insert new-content))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert new-content))))

(defun abbas/generate-habits (tasks tags-list)
  "Generate complete Habits section content as a string."
  (with-temp-buffer
    (insert "* ca Habits\n")
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
    (insert "* ca Dailies\n")
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
    (insert "* ca Todos\n")
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
  "Build Org tags string combining priority and ca tags."
  (let ((tags '()))
    (when task-tags
      (dolist (tag-id task-tags)
        (let ((tag-name (abbas/get-tag-name tag-id tags-list)))
          (when tag-name
            (push (abbas/sanitize-tag tag-name) tags)))))
    (let ((priority-tag (abbas/priority-to-tag priority)))
      (when priority-tag
        (push priority-tag tags)))
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
  "Sanitize tag name for Org mode."
  (when tag-name
    (downcase
     (replace-regexp-in-string "[^a-zA-Z0-9_@]" "_" tag-name))))

(defun abbas/priority-to-tag (priority)
  "Convert ca priority number to tag string."
  (cond
   ((= priority 0.1) "trivial")
   ((= priority 1.0) "easy")
   ((= priority 1.5) "medium")
   ((= priority 2.0) "hard")
   (t nil)))

(defun abbas/build-schedule (next-due repeat)
  "Build Org schedule string with repeater from ca data."
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
  "Convert ca frequency to Org repeater syntax."
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
CA_EOF

cat > "$ACTUAL_HOME/.config/doom/config.el" << 'CONFIG_EOF'
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 15))

(setq display-line-numbers-type 'relative)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(border-width . 0))

(setq org-directory "~/f/notes-org-mode")

(after! org
  (setq org-agenda-files
        '("~/f/notes-org-mode/todo.org"
          "~/f/notes-org-mode/notes.org")))

(setq org-latex-src-block-backend 'minted)

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq doom-theme 'doom-material)

(defun my-compile-and-run-vterm ()
  "Save the current buffer, then compile and run it in vterm."
  (interactive)
  (save-buffer)
  (let* ((file-name (buffer-file-name))
         (file-base (file-name-sans-extension file-name))
         (file-ext (file-name-extension file-name))
         (vterm-buf "*vterm*")
         (cmd nil))
    (setq cmd
          (cond
           ((string= file-ext "java")
            (format "javac %s && java %s" file-name (file-name-nondirectory file-base)))
           ((string= file-ext "py")
            (format "python3 %s" file-name))
           ((string= file-ext "c")
            (format "gcc -g %s -o %s && %s" file-name file-base file-base))
           ((or (string= file-ext "cpp") (string= file-ext "cc"))
            (format "g++ -g %s -o %s && %s" file-name file-base file-base))
           (t nil)))
    (if cmd
        (let ((vterm-window (get-buffer-window vterm-buf)))
          (if (and (get-buffer vterm-buf) vterm-window)
              (select-window vterm-window)
            (progn
              (split-window-below)
              (other-window 1)
              (if (get-buffer vterm-buf)
                  (switch-to-buffer vterm-buf)
                (vterm vterm-buf))))
          (vterm-send-string cmd)
          (vterm-send-return)
          (other-window 1))
      (message "No compile rule for file type: %s" file-ext))))

(map! :g "<f5>" #'my-compile-and-run-vterm)

(defun save_buff()
  "save buffer in any mode"
  (interactive)
  (save-buffer)
  (message "save hogaaya !!"))

(map! :g "<f9>" #'save_buff)

(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'prog-mode-hook (lambda () (flyspell-mode -1)))

(setq sql-connection-alist
      '((muzaffarpur
         (sql-product 'mysql)
         (sql-server "localhost")
         (sql-database "twentysix")
         (sql-user "abbas"))
        (mario
         (sql-product 'mysql)
         (sql-server "localhost")
         (sql-database "DBMSproj")
         (sql-user "abbas"))))

(setq user-full-name "Pamure"
      user-mail-address "murtazaprogrammer100@gmail.com")

(display-time-mode t)

(map! :leader
      (:prefix ("p t" . "treemacs")
       :desc "Toggle Treemacs"        "t" #'treemacs
       :desc "Select Treemacs window" "w" #'treemacs-select-window
       :desc "Switch workspace"       "s" #'treemacs-switch-workspace))

(use-package! corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(map! "C-," #'rotate-text
      "C-<" #'rotate-text-backward)

(after! rotate-text
  (add-to-list 'rotate-text-words
               '("int" "float" "double" "string" "char" "bool" "void"))
  (add-to-list 'rotate-text-words
               '("mariyam" "abbas" "ahmad" "anas" "atif" "ayaan"))
  (add-to-list 'rotate-text-words
               '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (add-to-list 'rotate-text-words
               '("!=" "=="))
  (add-to-list 'rotate-text-words
               '("if" "else if" "else"))
  (add-to-list 'rotate-text-words
               '("for" "while" "do")))

(setq evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)

(map! :leader
      :prefix ("e" . "tabs")
      :desc "Pick a tab" "t" #'centaur-tabs-ace-jump
      :desc "Next group" "l" #'centaur-tabs-forward-group
      :desc "Prev group" "h" #'centaur-tabs-backward-group)

(map! :n "g s c" #'avy-goto-char)
(map! :i "C-." #'avy-goto-char)

(map! :leader
      (:prefix ("o" . "open")
       :desc "Ace Window" "x" #'ace-window))

(use-package! ca
  :defer t
  :commands (ca-api-get-tasks
             ca-api-user
             ca-api-task-create)
  :init
  (setq habitica-uid "namonamonamo"
        habitica-token "tokiktokttiktok"))

(after! vterm
  (setq vterm-shell "/usr/bin/zsh"
        vterm-command-args '("-i")))

(add-to-list 'load-path "~/.config/doom/lisp/drag-stuff")
(require 'drag-stuff)
(use-package! drag-stuff
  :init
  (drag-stuff-global-mode 1)
  :config
  (map! :n "M-<up>"    #'drag-stuff-up
        :n "M-<down>"  #'drag-stuff-down
        :n "M-<left>"  #'drag-stuff-left
        :n "M-<right>" #'drag-stuff-right)
  (map! :i "M-<up>"    #'drag-stuff-up
        :i "M-<down>"  #'drag-stuff-down
        :i "M-<left>"  #'drag-stuff-left
        :i "M-<right>" #'drag-stuff-right))

(use-package! conda
  :config
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "torchgpu")))

(use-package! jupyter
  :after conda
  :defer t
  :config
  (when (executable-find "jupyter")
    (jupyter-available-kernelspecs t)))

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(map! :map org-mode-map
      :localleader
      :desc "Edit in Python file" "|" #'my/edit-jupyter-in-python)

(after! org
  (require 'ob-jupyter nil t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))
  (when (featurep 'ob-jupyter)
    (org-babel-jupyter-override-src-block "python"))
  (setq org-confirm-babel-evaluate nil
        org-image-actual-width '(1024))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (defun my/org-src-python-setup ()
    "Setup Python LSP in org-src blocks cleanly."
    (when (eq major-mode 'python-mode)
      (setq-local buffer-file-name
                  (expand-file-name
                   (concat ".org-src-" (make-temp-name "temp") ".py")
                   default-directory))
      (setq-local completion-at-point-functions
                  (remove 'jupyter-org-completion-at-point
                          completion-at-point-functions))
      (setq-local completion-at-point-functions '(lsp-completion-at-point))
      (when (boundp 'corfu-auto)
        (setq-local corfu-auto t))
      (lsp-deferred)))

  (add-hook 'org-src-mode-hook #'my/org-src-python-setup))

(after! jupyter
  (defun my/jupyter-org-completion-guard (orig-fun &rest args)
    "Only run jupyter-org completion in actual org-mode buffers."
    (when (eq major-mode 'org-mode)
      (apply orig-fun args)))
  (advice-add 'jupyter-org-completion-at-point :around #'my/jupyter-org-completion-guard))

(after! flycheck
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled)))

(after! jupyter
  (map! :map jupyter-repl-interaction-mode-map
        "C-c C-c" nil
        :localleader
        (:prefix ("e" . "jupyter-eval")
         "e" #'jupyter-eval-line-or-region)))

(setq lsp-dart-sdk-dir "FLUTTER_HOME/bin/cache/dart-sdk")
(setq lsp-dart-flutter-sdk "FLUTTER_HOME")
(setq flutter-sdk-path "FLUTTER_HOME")

(after! lsp-mode
  (setenv "PATH" (concat (getenv "PATH") ":GO_BIN"))
  (add-to-list 'exec-path "GO_BIN"))

(auto-save-visited-mode +1)
(setq auto-save-visited-interval 1)

(after! doom-themes
  (setq doom-themes-treemacs-theme "doom-colors"))

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

(setq +doom-dashboard--width 90)

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

(defun pam/dash-insert-centered (str &optional face)
  "Insert STR centered in the dashboard, optionally with FACE."
  (let ((line (+doom-dashboard--center +doom-dashboard--width str)))
    (if face
        (insert (propertize line 'face face) "\n")
      (insert line "\n"))))

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

(defun pam/dash-keybind-table (rows &optional cols)
  "Render ROWS of (key . desc) pairs in COLS columns."
  (let* ((cols  (or cols 2))
         (w     (/ (- +doom-dashboard--width 4) cols))
         (key-w 16)
         (desc-w (- w key-w 4))
         (chunks (seq-partition rows cols)))
    (dolist (chunk chunks)
      (let ((line "  "))
        (dolist (cell chunk)
          (let* ((k    (propertize (format "%-*s" key-w (car cell)) 'face 'pam/key-face))
                 (d    (propertize (truncate-string-to-width (cdr cell) desc-w) 'face 'pam/desc-face))
                 (col  (format "%s %s  " k d)))
            (setq line (concat line col))))
        (insert (+doom-dashboard--center +doom-dashboard--width
                                         (string-trim-right line)) "\n")))
    (insert "\n")))

(defun pam/dash-org-todos ()
  "Insert pending org-mode TODO items from agenda files."
  (when (and (featurep! :lang org)
             org-agenda-files)
    (pam/dash-section "ORG  TODOS")
    (let ((items '())
          (limit 8))
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
          (pam/dash-insert-centered "  No pending TODOs" 'pam/desc-face)
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
                                (propertize (concat "  " (substring sched 1 11)) 'face 'pam/desc-face)
                              ""))
                   (state-face (if (equal state "DONE") 'pam/todo-done-face 'pam/todo-face))
                   (line    (format "  %s  %s  %s"
                                    (propertize (format "%-12s" (concat icon " " state)) 'face state-face)
                                    (truncate-string-to-width heading 48)
                                    sfx)))
              (insert (+doom-dashboard--center +doom-dashboard--width line) "\n")))
          (when (> (length items) limit)
            (pam/dash-insert-centered
             (format "  ... and %d more" (- (length items) limit))
             'pam/desc-face))))
      (insert "\n"))))

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
           (format "  ... and %d more  (SPC p p to switch)" (- (length projects) limit))
           'pam/desc-face))))
    (insert "\n")))

(defun pam/dash-keybindings ()
  "Insert a two-column keybinding reference card."
  (pam/dash-section "VIM  MOTIONS")
  (pam/dash-keybind-table
   '(("h j k l"      . "<- down up ->  character")
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
     ("c{motion}"     . "Change  (delete + insert)")
     ("r{char}"       . "Replace single char")
     ("."             . "Repeat last change")
     ("> / <"         . "Indent / dedent"))
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

(defun pam/dash-footer ()
  (insert "\n")
  (pam/dash-insert-centered
   (format "Doom %s  .  Emacs %s  .  %s packages"
           (or (bound-and-true-p doom-version) "?")
           emacs-version
           (length package-activated-list))
   'pam/desc-face)
  (insert "\n"))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        pam/dash-org-todos
        pam/dash-projects
        pam/dash-keybindings
        pam/dash-footer))

(add-hook! 'doom-after-reload-hook #'+doom-dashboard-reload)
CONFIG_EOF

sed -i "s|FLUTTER_HOME|$ACTUAL_HOME/downloads/dev/flutter|g" "$ACTUAL_HOME/.config/doom/config.el"
sed -i "s|GO_BIN|$ACTUAL_HOME/go/bin|g" "$ACTUAL_HOME/.config/doom/config.el"

mkdir -p "$ACTUAL_HOME/f/notes-org-mode"
touch "$ACTUAL_HOME/f/notes-org-mode/todo.org"
touch "$ACTUAL_HOME/f/notes-org-mode/notes.org"

sudo mysql -e "CREATE USER IF NOT EXISTS 'abbas'@'localhost' IDENTIFIED BY '';" 2>/dev/null || true
sudo mysql -e "CREATE DATABASE IF NOT EXISTS twentysix;" 2>/dev/null || true
sudo mysql -e "CREATE DATABASE IF NOT EXISTS DBMSproj;" 2>/dev/null || true
sudo mysql -e "GRANT ALL PRIVILEGES ON twentysix.* TO 'abbas'@'localhost';" 2>/dev/null || true
sudo mysql -e "GRANT ALL PRIVILEGES ON DBMSproj.* TO 'abbas'@'localhost';" 2>/dev/null || true
sudo mysql -e "FLUSH PRIVILEGES;" 2>/dev/null || true

export PATH="$PATH:$ACTUAL_HOME/downloads/dev/flutter/bin:/usr/local/go/bin:$ACTUAL_HOME/go/bin:$ACTUAL_HOME/miniconda3/bin"

"$ACTUAL_HOME/.config/emacs/bin/doom" sync --no-env

echo "setup complete"
echo "run: source ~/.bashrc"
echo "then open emacs and run: M-x nerd-icons-install-fonts"
