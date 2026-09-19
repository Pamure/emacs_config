;;; $doomdir/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; format key
;;(setq default-directory "/home/mjnoir/f")



;; (map! :leader
;;       :desc "Format buffer" "c f" #'+format/buffer)
;
;; In ~/.config/doom/config.el

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 15))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/)"
;; Remove window decorations and borders
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(border-width . 0))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq gc-cons-threshold most-positive-fixnum)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (setq gc-cons-threshold (* 16 1024 1024))))



;; the old compiler function that doesnt have vterm use compiler window
;; (defun my-compile-and-run ()
;;   "Save the current buffer, then compile and run it."
;;   (interactive)
;;   (save-buffer)
;;   (let* ((file-name (buffer-file-name))
;;          (file-base (file-name-sans-extension file-name))
;;          (file-ext (file-name-extension file-name)))
;;     (cond
;;      ((string= file-ext "java")
;;       (compile (format "javac %s && java %s" file-name (file-name-nondirectory file-base))))

;;      ((string= file-ext "py")
;;       (compile (format "python3 %s" file-name)))

;;      ((string= file-ext "c")
;;       ;; Corrected: Removed the "./" before the final "%s"
;;       (compile (format "gcc %s -o %s && %s" file-name file-base file-base)))

;;      ((or (string= file-ext "cpp") (string= file-ext "cc"))
;;       ;; Corrected: Removed the "./" before the final "%s"
;;       (compile (format "g++ %s -o %s && %s" file-name file-base file-base)))

;;      (t (message "No compile rule for file type: %s" file-ext)))))

;; ;; Now, bind it to <f5> using Doom's `map!` macro.
;; ;; The `:g` means it's a global binding, available in all modes.
;; (map! :g "<f5>" #'my-compile-and-run)
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
  "insert or normal dono mode mai buffer save krne k liye"
  (interactive)
  (save-buffer)
  (message "save hogaaya !!")
  )
(map! :g "<f9>" #'save_buff)


;; (after! lsp-ui
;;   (setq lsp-ui-doc-position 'bottom)
;;   (setq lsp-ui-sideline-enable nil))
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



(setq doom-theme 'doom-one)
;; (defun my/open-graphical-files-externally ()
;;   "When in a terminal, open .pdf and .xopp files in xournalpp."
;;   (let ((graphical-file-extensions '("\\.pdf\\'" "\\.xopp\\'")))
;;     ;; Check if we are in a terminal and if the buffer has a file
;;     (when (and (not (display-graphic-p)) (buffer-file-name))
;;       ;; Check if the filename matches one of the extensions
;;       (when (cl-some (lambda (ext) (string-match-p ext (buffer-file-name)))
;;                      graphical-file-extensions)
;;         ;; If it matches, run the external command
;;         (let ((command (concat "xournalpp " (shell-quote-argument (buffer-file-name)) " &")))
;;           (message "Opening %s in xournalpp..." (file-name-nondirectory (buffer-file-name)))
;;           (shell-command command)
;;           (kill-buffer (current-buffer)))))))

;; Add the function to the find-file-hook
;;           ;; Kill the buffer Emacs just opened, as it's not needed
;;(add-hook 'find-file-hook #'my/open-graphical-files-externally)
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (when (member (file-name-extension (buffer-file-name))
;;                           '("pdf" "xopp"))
;;               (my/open-graphical-files-externally))))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'prog-mode-hook (lambda () (flyspell-mode -1)))

;; This block is for MANUALLY running queries.
;; KEEP THIS BLOCK. abhi itne qabil nahi bane ho ke claude k bina kuch kr paao
;; `sql-connection-alist' (the MySQL quick-connect menu) carries local DB
;; users/hosts, so it lives in config.local.el.  See config.local.el.example
;; — adjust `sql-user'/`sql-server' there for the new machine.
;;(add-hook 'sql-mode-hook #'lsp)
;;(add-hook 'python-mode-hook #'lsp)
;; `user-mail-address' is personal, so it lives in config.local.el (untracked).
(setq user-full-name "Pamure")
(display-time-mode 1)
(after! treemacs
  (map! :leader
        :desc "Toggle Treemacs"        "p t" #'treemacs
        :desc "Select Treemacs window" "p w" #'treemacs-select-window
        :desc "Switch workspace"       "p s" #'treemacs-switch-workspace))
(use-package! corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Top-level — must be outside any after! block
(repeat-mode 1)
(after! rotate-text
  ;; ── Word lists ──────────────────────────────────────────────
  (add-to-list 'rotate-text-words '("int" "float" "double" "string" "char" "bool" "void"))
  (add-to-list 'rotate-text-words '("mariyam" "abbas" "ahmad" "anas" "atif" "ayaan"))
  (add-to-list 'rotate-text-words '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                                    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (add-to-list 'rotate-text-words '("!=" "=="))
  (add-to-list 'rotate-text-words '("if" "else if" "else"))
  (add-to-list 'rotate-text-words '("for" "while" "do"))

  ;; ── Repeat map (Emacs 29+ defvar-keymap syntax) ─────────────
  (defvar-keymap my/rotate-repeat-map
    :repeat t
    "t" #'rotate-text
    "T" #'rotate-text-backward)

;; ── Wire commands into the repeat map ────────────────────────
  (put 'rotate-text          'repeat-map 'my/rotate-repeat-map)
  (put 'rotate-text-backward 'repeat-map 'my/rotate-repeat-map))


  ;; ── Entry point ──────────────────────────────────────────────
(map! :leader
      :desc "Rotate →" "r t" #'rotate-text
      :desc "Rotate ←" "r T" #'rotate-text-backward)

(setq evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)
(map! :leader
      (:prefix ("e" . "tabs")     ; <-- gives the group a name
       :desc "Pick a tab" "t" #'centaur-tabs-ace-jump
       :desc "Next group" "l" #'centaur-tabs-forward-group
       :desc "Prev group" "h" #'centaur-tabs-backward-group))
(map! :n
      "g s c" #'avy-goto-char)
(map! :i
      "C-." #'avy-goto-char)

(map! :leader
      (:prefix ("o" . "open")
        :desc "Ace Window" "x" #'ace-window
        :desc "AI chat (gptel)"        "l l" #'gptel
        :desc "AI send to gptel"       "l s" #'gptel-send
        :desc "AI rewrite region"      "l r" #'gptel-rewrite
        :desc "AI explain region"      "l e" #'gptel-explain
        :desc "AI menu (change model)" "l m" #'gptel-menu
        :desc "AI add to context"      "l a" #'gptel-add
        :desc "AI quick explain"       "l q" #'gptel-quick))

;; NOTE: `habitica-uid' and `habitica-token' are credentials and are NOT
;; stored in git.  Set them in config.local.el (see config.local.el.example).
;; They're only read when one of the commands below is invoked, so the rest
;; of this config works fine without them.
(use-package! habitica
  :defer t
  :commands (habitica-api-get-tasks
             habitica-api-user
             habitica-api-task-create))


;; (after! org
;;   (add-to-list 'org-capture-templates
;;    '("j" "Daily Self Journal" entry
;;      (file+olp "~/f/notes-org-mode/AbbasNamah.org"
;;                 "%<%Y>"
;;                 "%<%Y-%m %B>"
;;                 "%<%Y-%m-%d %A>")
;;      "**** [%<%Y-%m-%d %a %H:%M>] Journal Entry

;; **  Happy with yourself today? (yes / no / ok)
;; - %?

;; **  Did you complete your todos today? (yes / no / ok)
;; -

;; **  Summary of today’s important events
;; -
;;(setq fancy-splash-image (concat doom-private-dir "logo.jpg"))
;; "
;;      :empty-lines 1)))
;; (defun my/habitica-sync ()
;;   (interactive)
;;   (load! "habiticasync"))
;;
;;
(after! org
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits-only-for-today t))

;; config.el
(after! org-pomodoro
  (setq org-pomodoro-length 20
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 20))
(after! vterm
  (setq vterm-shell "/usr/bin/zsh"
        vterm-command-args '("-i")))
(add-to-list 'load-path "~/.config/doom/lisp/drag-stuff")
(use-package! drag-stuff
  :config
  (drag-stuff-global-mode 1)
  ;; Normal mode
  (map! :n "M-<up>"    #'drag-stuff-up
        :n "M-<down>"  #'drag-stuff-down
        :n "M-<left>"  #'drag-stuff-left
        :n "M-<right>" #'drag-stuff-right)

  ;; Insert mode
  (map! :i "M-<up>"    #'drag-stuff-up
        :i "M-<down>"  #'drag-stuff-down
        :i "M-<left>"  #'drag-stuff-left
        :i "M-<right>" #'drag-stuff-right))
;;; ML / Jupyter workflow — Doom Emacs config --------------------------------
;; Fixed two structural bugs: (1) jupyter wires itself into every org buffer
;; on load, and your old config force-loaded it eagerly instead of lazily,
;; so it hit notes.org/todo.org too. (2) every notebook defaulted to the
;; same :session, so they all shared one kernel. Doom's builtin
;; (org +jupyter) flag fixes #1 the same way; done by hand below so the
;; header-arg/keybinding customizations further down still apply.

;; ── Conda (main env — REQUIRED) ─────────────────────────────────────────
;; conda-env-activate puts the env's bin/ FIRST on Emacs' PATH. Without it,
;; Emacs finds the broken Debian /usr/bin/jupyter (jupyter_core missing for
;; system python3) and every jupyter block fails. jupyter.el needs a working
;; `jupyter kernelspec list` to resolve kernels, even though the torchgpu
;; kernel spec itself has an absolute Python path.
(use-package! conda
  :config
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  (unless (equal (getenv "CONDA_DEFAULT_ENV") "torchgpu")
    (conda-env-activate "torchgpu")))

;; ── Flycheck ────────────────────────────────────────────────────────────
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; -- Flutter / Dart (unrelated to the ML stack, left as-is) ----------------
;; $HOME-relative, so the same file works under any username/machine.
(setq lsp-dart-sdk-dir (expand-file-name "~/downloads/dev/flutter/bin/cache/dart-sdk"))
(setq lsp-dart-flutter-sdk (expand-file-name "~/downloads/dev/flutter"))
(setq flutter-sdk-path (expand-file-name "~/downloads/dev/flutter"))

;; ── Go on PATH for lsp-mode (unrelated, left as-is) ───────────────────────
(after! lsp-mode
  (let ((go-bin (expand-file-name "~/go/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" go-bin))
    (add-to-list 'exec-path go-bin)))

;; -- Autosave ------------------------------------------------------------
(auto-save-visited-mode +1)
(setq auto-save-visited-interval 5) ; was 1s — fine either way, just less
                                     ; noisy while async results stream in

;; -- org-alert -----------------------------------------------------------
(after! org-alert
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10)
  (org-alert-enable))

;; -- Jupyter + org-babel -------------------------------------------------

;; Load ob-jupyter lazily - only when a `jupyter-*' block is actually
;; executed, not on every org file. `+org-babel-load-functions' is Doom's
;; own hook for exactly this (it's what the builtin +jupyter flag uses too).
(defun +my/org-babel-load-jupyter-lazily (lang)
  (and (string-prefix-p "jupyter-" (symbol-name lang))
       (require 'ob-jupyter nil t)
       (org-babel-jupyter-make-local-aliases)))
(add-hook '+org-babel-load-functions #'+my/org-babel-load-jupyter-lazily)

(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:session . "py") ; fallback only for quick scratch blocks -
        (:kernel . "torchgpu") ; every real notebook should override
        (:results . "both drawer"))) ; see note below

;; Your own testing found that plain ":results both" (no format keyword)
;; reliably crashes on error output, while ":results drawer" doesn't --
;; verified that against a real captured traceback. "drawer" alone risked
;; silently changing what gets captured too (drawer is a *format*
;; keyword; "value"/"output"/"both" is a separate *content* keyword, and I
;; couldn't confirm content still includes stdout with drawer alone) --
;; "both drawer" keeps the content behavior you already had, tested, and
;; working, and only changes the wrapping to the one that doesn't crash.
;; I could not find "both" hardcoded anywhere in jupyter.el itself, so
;; this is very likely a plain org-babel result-insertion difference
;; (`#+begin_example' block vs `:RESULTS:' drawer) reacting differently to
;; ANSI-coded, specially-propertized text -- not fully traced to one
;; line, same limitation as before: no live kernel here to confirm
;; further. If a real exception (not a SyntaxError) still crashes under
;; this, that's the next thing to isolate.

(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

(use-package! jupyter
  :after conda
  :config
  ;; Ensures the activated conda env's jupyter is on PATH before jupyter.el
  ;; resolves kernels. Don't silently launch a GPU-backed kernel just because
  ;; point moved into a block or corfu asked for completions. A kernel now
  ;; only starts when you actually execute something with C-c C-c.
  (setq jupyter-org-auto-connect nil)
  ;; Official v1.0 knob (CHANGELOG.org): pin the jupyter executable so the
  ;; broken /usr/bin/jupyter stub (no kernelspec subcommand) never shadows
  ;; conda's during org-babel-jupyter-make-local-aliases kernelspec discovery.
  (setq jupyter-executable (expand-file-name "~/miniconda3/envs/torchgpu/bin/jupyter")))

;; Undo the blanket org-mode-hook jupyter adds for itself, and only turn
;; its interaction mode on in a buffer once a session has actually started
;; there. This is the fix for notes.org/todo.org/agenda getting jupyter's
;; completion-at-point and keybindings despite having no jupyter content.
(after! jupyter-org-client
  (remove-hook 'org-mode-hook #'jupyter-org-interaction-mode)
  ;; Belt-and-suspenders: never let a completion request throw a visible
  ;; error even inside a real session (e.g. the kernel died mid-typing).
  (advice-add 'jupyter-org-completion-at-point :around
              (lambda (orig &rest args)
                (condition-case nil (apply orig args) (error nil)))))

(after! ob-jupyter
  (defadvice! +my/jupyter-org-interaction-on-connect-a (&rest _)
    "Turn on `jupyter-org-interaction-mode' only once a session actually
starts in THIS buffer, instead of unconditionally in every org buffer."
    :after #'org-babel-jupyter-initiate-session
    (unless (bound-and-true-p jupyter-org-interaction-mode)
      (jupyter-org-interaction-mode 1))))

;; ── Kernel completion reliability (fix: "shows once then dies") ──────────
;; emacs-jupyter's completion is async: jupyter-completion-at-point sends a
;; complete_request to the kernel, then returns a completion-table-dynamic
;; whose lambda does ONE (sit-for 0.1) and returns nil if the reply hasn't
;; landed (jupyter-client.el ~1639). Corfu queries the capf ONCE per cycle,
;; so on a slow reply it gets nil, quits, and never re-queries — the
;; "completions show once then die" bug (emacs-jupyter#249, doomemacs#7757).
;; The maintainer's own proposed fix (issue #249): "wait for as long as
;; needed for the complete_reply to come in".  This advice makes the
;; prefetch block briefly (bounded 0.5s, aborting when the user types) so
;; corfu's single query always finds the kernel's answer.  Local kernels
;; reply in 20-80ms, so the popup appears on the first keystroke.
(after! jupyter-client
  (defun my/jupyter-completion-wait-a (&rest _)
    "After the prefetch request, pump ZMQ output until the reply lands."
    (let ((deadline (+ (float-time) 0.5)))
      (while (and (null (bound-and-true-p jupyter-completion-cache))
                  (not (input-pending-p))
                  (< (float-time) deadline))
        (accept-process-output nil 0.02)
        (sit-for 0.01)))
    nil)
  (advice-add #'jupyter-completion-prefetch :after
              #'my/jupyter-completion-wait-a))

;; ── corfu-popupinfo kernel-crash guard ───────────────────────────────────
;; jupyter's capf advertises a :company-doc-buffer handler; corfu-popupinfo
;; calls it on hover, which sends an inspect_request to the kernel.  The
;; request's message carries complex org marker objects that prin1 can't
;; serialize, killing the ZMQ subprocess (emacs-jupyter#592; doomemacs#7757
;; — the "kernel died / user-ptrp nil" crashes seen in past sessions).
;; Strip the property so corfu-popupinfo never inspects through jupyter.
;; (The existing company-doc-buffer polyfill already neutralized the
;; void-function half of #7757; this kills the inspect request entirely.)
(defun my/jupyter-capf-strip-company-doc-a (result)
  "Remove `:company-doc-buffer' from jupyter's capf RESULT."
  (when (and (consp result) (plist-member (nthcdr 3 result) :company-doc-buffer))
    (let ((plist (nthcdr 3 result)) out)
      (while plist
        (unless (eq (car plist) :company-doc-buffer)
          (setq out (nconc out (list (car plist) (cadr plist)))))
        (setq plist (cddr plist)))
      (setf (nthcdr 3 result) out)))
  result)

(after! jupyter-client
  (advice-add #'jupyter-completion-at-point :filter-return
              #'my/jupyter-capf-strip-company-doc-a))


;; Give every notebook its own kernel ----------------------------------------
;; jupyter.el identifies a kernel purely by the string "<session>-<kernel>".
;; Two files with the same :session share the same running kernel and
;; Python namespace. Run this once in a new notebook (or hit the keybinding
;; below) to scope it to that file specifically.
(defun my/org-jupyter-set-session ()
  "Set this file's jupyter session to a name derived from its own
filename, so it never collides with another notebook's kernel."
  (interactive)
  (let* ((base (if buffer-file-name (file-name-base buffer-file-name) "scratch"))
         (session (replace-regexp-in-string "[^a-zA-Z0-9_-]+" "-" base))
         (line (format
                "#+PROPERTY: header-args:jupyter-python :session %s :kernel %s :async yes :results drawer"
                session "torchgpu")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+PROPERTY: header-args:jupyter-python" nil t)
          (progn (beginning-of-line) (kill-line) (insert line))
        (progn
          (while (looking-at "^#\\+") (forward-line 1))
          (insert line "\n"))))
    (message "This file's jupyter session is now: %s" session)))

;; The plain `jupyter-repl-pop-to-buffer' command only works when
;; `jupyter-current-client' is already dynamically bound -- true inside
;; jupyter's own internals, false when just called from a keybinding in
;; the org buffer, which is why it said "Buffer not associated with a
;; REPL" even with a live session running. `jupyter-org-with-src-block-client'
;; is the actual mechanism jupyter.el uses internally to find the client
;; for whatever block point is in (mirrors
;; `jupyter-org--call-with-src-block-client', the same internal pattern).
;;
;; CORRECTION: this does NOT show you tracebacks from blocks you already
;; ran. org-babel execution talks to the kernel directly and never echoes
;; into the REPL buffer's visible transcript (confirmed: only
;; `jupyter-eval-*'-family commands do that, gated by
;; `jupyter-repl-echo-eval-p', a completely separate code path from
;; `org-babel-execute:jupyter'). What this IS still good for: typing new
;; commands directly into the REPL to poke at the live kernel state --
;; e.g. `type(Xtest)' -- since it's the same running kernel/namespace your
;; blocks use. See the exception-hook snippet in your org file for
;; actually seeing tracebacks.
(defun my/jupyter-org-repl-pop-to-buffer ()
  "Jump to the REPL for the jupyter-python block point is in, to poke
at the live kernel interactively. Needs the block to have been
executed at least once already."
  (interactive)
  (if (org-in-src-block-p)
      (jupyter-org-with-src-block-client
        (call-interactively #'jupyter-repl-pop-to-buffer))
    (message "Point isn't inside a src block -- move into one first")))

(map! :map org-mode-map
      :localleader
      (:prefix ("j" . "jupyter")
       :desc "Give this file its own kernel session" "s" #'my/org-jupyter-set-session
       :desc "Jump to this session's REPL (poke at live kernel)" "r" #'my/jupyter-org-repl-pop-to-buffer
       :desc "Shut down this file's kernel (frees GPU memory)" "k" #'jupyter-repl-shutdown-kernel))

;; C-c ' edit buffer: give it a real filename so LSP can attach, and make
;; that name unique per source file + block position so two edit buffers
;; open at once (e.g. two notebooks in the same folder) don't collide and
;; confuse the LSP server.
(defun my/jupyter-edit-buffer-setup (info)
  (let* ((org-file (buffer-file-name (buffer-base-buffer)))
         (base (if org-file (file-name-base org-file) "scratch")))
    ;; FIXED: Removed `(point)` so Pyright only caches ONE file in RAM
    (setq buffer-file-name
          (expand-file-name (format ".%s-src.py" base) default-directory))
    (defvar lsp-auto-guess-root)
    (let ((lsp-auto-guess-root t))
      (lsp-deferred))))

(after! jupyter
  (advice-add 'org-babel-edit-prep:jupyter-python
              :after #'my/jupyter-edit-buffer-setup))

;; ── code-cells: "# %%" cell markers for plain .py files ───────────────────
;; Separate feature, unrelated to the org-mode stack above — only hooks into
;; python-mode. Good for quickly running chunks of a standalone script; not
;; meant to be a full notebook replacement (that's what the org setup is for).
(use-package! code-cells
  :hook (python-mode . code-cells-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . python-mode))
  (map! :map code-cells-mode-map
        :n "]c"   #'code-cells-forward-cell
        :n "[c"   #'code-cells-backward-cell
        :n "<f6>" #'code-cells-eval
        :localleader
        (:prefix ("j" . "jupyter cells")
         :desc "Run cell"       "r" #'code-cells-eval
         :desc "Next cell"      "n" #'code-cells-forward-cell
         :desc "Prev cell"      "p" #'code-cells-backward-cell
         :desc "New cell below" "o" (lambda ()
                                      (interactive)
                                      (code-cells-forward-cell)
                                      (insert "\n# %%\n"))
         :desc "Run all"        "R" #'jupyter-eval-buffer)))

;; ── LSP + Corfu glue (standard Doom pattern, unchanged) ───────────────────
(after! lsp-mode
  (setq lsp-completion-provider :none)
  (defun my/lsp-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-completion-mode-hook #'my/lsp-setup-completion))

;; TAB not accepting the highlighted completion (inserts a space instead) --
;; Doom's corfu module doesn't bind TAB itself; it relies on Corfu's own
;; keymap ("TAB" -> `corfu-complete'). In Evil + GUI Emacs, insert-state's
;; own TAB/<tab> bindings can win that conflict before Corfu's popup-active
;; keymap is even consulted -- documented in Doom's own corfu module README
;; under "Fixing TAB Keybindings" -- so TAB was falling through to plain
;; indentation (inserting whitespace) instead of reaching Corfu at all.
;; REGRESSION FIX: the version of this that shipped earlier only checked
;; corfu, so the same global override that fixed TAB in code buffers was
;; ALSO shadowing vertico's own "TAB" binding (vertico-insert) inside the
;; minibuffer -- exactly the SPC . / find-file "tab gives a space instead
;; of picking the first result" bug. Confirmed against vertico's actual
;; source: vertico-map already binds "TAB" to vertico-insert correctly on
;; its own -- it never needed touching, my global binding just outranked
;; it. Minibuffer is checked first and unconditionally deferred to vertico.
(defun +my/corfu-tab-or-indent ()
  "Insert the selected vertico candidate in the minibuffer; accept the
Corfu candidate if its popup is genuinely showing candidates; indent
normally otherwise."
  (interactive)
  (cond
   ((and (minibufferp) (fboundp 'vertico-insert)) (vertico-insert))
   ((and (bound-and-true-p corfu-mode) corfu--candidates) (corfu-complete))
   (t (indent-for-tab-command))))
(map! :gi "TAB"   #'+my/corfu-tab-or-indent
      :gi "<tab>" #'+my/corfu-tab-or-indent)

;; Doom sets corfu-preselect to 'prompt, meaning nothing is highlighted
;; until you navigate with C-n/M-n. Corfu's own upstream default is
;; 'valid, which auto-highlights the top candidate so TAB/RET on it works
;; immediately -- closer to "press tab to select it" as you'd expect.
(after! corfu
  (setq corfu-preselect 'valid))

;; "Error running timer 'corfu-popupinfo--show': (void-function
;; company-doc-buffer)" -- jupyter.el's own completion metadata calls
;; `company-doc-buffer' (a company.el helper) to build its documentation
;; popup, even though you're not running company as a UI. It's a trivial,
;; side-effect-free helper (verified against company.el's actual source) --
;; defining it directly is simpler than pulling in all of company.el for
;; one function, and never touches company-mode itself.
(unless (fboundp 'company-doc-buffer)
  (defun company-doc-buffer (&optional string)
    (with-current-buffer (get-buffer-create "*company-documentation*")
      (erase-buffer)
      (fundamental-mode)
      (when string
        (save-excursion (insert string) (visual-line-mode)))
      (current-buffer))))

;; If warnings show up that you can't otherwise explain, comment this out
;; temporarily. It was very likely papering over org-element parse warnings
;; caused by jupyter's font-lock hacks running in every org buffer, which is
;; fixed above — if those warnings have genuinely stopped, delete this line.
(add-to-list 'warning-suppress-log-types '(org-element))
(require 'ansi-color)

(defun my/org-babel-apply-ansi-colors ()
  "Apply ANSI color codes to the org-babel result block."
  (when-let ((beg (org-babel-where-is-src-block-result)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (inhibit-read-only t))
          (ansi-color-apply-on-region beg end))))))

(add-hook 'org-babel-after-execute-hook #'my/org-babel-apply-ansi-colors)
;; =====================================================================
;; JUPYTER & ASYNC FIXES (Emacs 30 / Doom Compatibility)
;; =====================================================================

;; 1. Silence ZMQ null pointer panics when a kernel dies/restarts
(after! zmq
  (defadvice! my/mute-zmq-null-ptr-a (orig-fn process string)
    :around #'zmq--subprocess-filter
    (condition-case err
        (funcall orig-fn process string)
      (error
       (unless (string-match-p "user-ptrp.*nil" (error-message-string err))
         (signal (car err) (cdr err)))))))

;; 2. Force ob-async to back off from Jupyter blocks completely
(after! ob-async
  (add-to-list 'ob-async-no-async-languages-alist "jupyter-python")
  (add-to-list 'ob-async-no-async-languages-alist "jupyter"))

;; 3. Add Jupyter to Doom's native async whitelist to stop the warning spam
(after! org
  (add-to-list '+org-babel-native-async-langs 'jupyter-python)
  (add-to-list '+org-babel-native-async-langs 'jupyter))

;; 4. Safely bypass the Emacs 30 typo in emacs-jupyter
(after! jupyter-repl
  (defalias 'jupyter-repl-sync-execution-state #'ignore))
;; Godmode integration: conditionally load helper if it exists
(let ((godmode-helper (expand-file-name "~/f/projects/godmode/mcp/org/godmode-org-helpers.el")))
  (when (file-exists-p godmode-helper)
    (load godmode-helper)))

;; ── gptel: AI assistant in Emacs (OpenRouter) ─────────────────────────────
;; Uses Doom's built-in :tools llm module (ships gptel; <leader> o l l = chat,
;; o l s = send, o l r = rewrite, o l e = explain).
;;
;; OpenRouter: one key, every model.  Key comes from OPENROUTER_API_KEY,
;; written to ~/.config/emacs/env (Doom loads it at startup).
;;
;; Two things were wrong before (source-verified against gptel 0.9.9.5):
;;   1. gptel-make-openai only REGISTERS the backend; the result must be
;;      ASSIGNED to gptel-backend or gptel warns "No gptel-backend defined".
;;   2. gptel-model must be a SYMBOL that appears in the backend's :models.
;;      (gptel-completion-mode was removed in the 0.9 rewrite — the ghost-text
;;      completion now lives in the separate `minuet' package, below.)
(use-package! gptel
  :config
  ;; Eager + authoritative: assign the backend AND model here so gptel
  ;; never falls back to its first-run setup prompt ("ask it anyways").
  (setq gptel-model 'nvidia/nemotron-3.5-lightning:free   ; free, fast, verified 200
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          ;; KEY SOURCE: ~/.authinfo (host openrouter.ai / apikey).  This is
          ;; the standard Emacs secret store — survives `doom sync', unlike
          ;; writing into the generated 05-doom-env.load.el (sync regenerates
          ;; and wipes it).  The old (getenv ...) lambda broke after every
          ;; sync, which caused the "string-trim-right: Wrong type argument:
          ;; stringp, nil" crash (gptel-request.el:962 trims a nil key).
          :key #'gptel-api-key-from-auth-source
          :models '(nvidia/nemotron-3.5-lightning:free ;; free · fast · verified working
                    minimax/minimax-m3:free           ;; free · verified working
                    z-ai/glm-5.2:free                 ;; free · strong coder · can 429
                    google/gemma-4-31b-it:free        ;; free · big context
                    qwen/qwen3.8-flash                ;; $0.00000015/1M · reliable fallback
                    openrouter/auto)))                ;; router
  (setq gptel-default-mode 'org-mode))

;; The doom+ tools/llm module's README documents <leader> o l keys, but the
;; module itself defines NONE (verified: no map! in the module).  Bind them
;; here so SPC o l actually exists.


;; ── minuet: AI ghost-text completion (OpenRouter) ───────────────────────
;; The 2026-verified completion path for OpenAI-compatible/OpenRouter
;; (gptel-completion-mode no longer exists).  Comment→code, ghost text,
;; free models.  Keys: M-i show, M-a accept line, M-w accept word.
(use-package! minuet
  ;; NOTE: keys go in minuet-active-mode-map (active only while a
  ;; suggestion is showing), NOT globally — a global M-i would shadow
  ;; jupyter's kernel-inspect (M-i = jupyter-inspect-at-point in org
  ;; jupyter blocks), which is what the user needs for 'what does this
  ;; function do'.
  :init (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (define-key minuet-active-mode-map (kbd "M-i") #'minuet-show-suggestion)
  (define-key minuet-active-mode-map (kbd "M-a") #'minuet-accept-suggestion-line)
  (define-key minuet-active-mode-map (kbd "M-w") #'minuet-accept-suggestion-word)
  (setq minuet-provider 'openai-compatible
        minuet-request-timeout 2.5
        minuet-auto-suggestion-throttle-delay 1.5
        minuet-auto-suggestion-debounce-delay 0.6)
  (plist-put minuet-openai-compatible-options :end-point
             "https://openrouter.ai/api/v1/chat/completions")
  ;; minuet's :api-key must be an ENV VAR NAME (string) or a function —
  ;; minuet--get-api-key does (getenv api-key) on it.  A literal key string
  ;; broke availability ("provider openai-compatible is not available").
  ;; Set OPENROUTER_API_KEY from auth-source at load, then point minuet at it.
  (setenv "OPENROUTER_API_KEY"
          (gptel-api-key-from-auth-source "openrouter.ai"))
  (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
  (plist-put minuet-openai-compatible-options :model "nvidia/nemotron-3.5-lightning:free")
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9))

;; ── Performance ──────────────────────────────────────────────────────────
;; LSP / jupyter ZMQ / vterm output arrives in chunks; 64KB default
;; read-process-output-max makes process interaction sluggish.  4MB is
;; the community-standard bump (used by both Doom and Emacs-heavy users).
(setq read-process-output-max (* 4 1024 1024))

;; Use the machine's cores for async native compilation.
(setq native-comp-async-jobs-number 4)

;; ob-jupyter runs `jupyter kernelspec list' (a subprocess) on EVERY
;; org buffer via org-mode-hook — with a pinned jupyter-executable this
;; is pure waste per file.  Run the alias pass once at load instead.
(after! ob-jupyter
  (org-babel-jupyter-make-local-aliases)
  (remove-hook 'org-mode-hook #'org-babel-jupyter-make-local-aliases))

;; ── Machine/person-specific values (NOT tracked in git) ───────────────────
;; Habitica uid+token, `user-mail-address', `sql-connection-alist' and any
;; other per-machine override live in config.local.el:
;;
;;     cp config.local.el.example config.local.el   # then edit it
;;
;; This is loaded LAST, so its values win over anything above.  The file is
;; optional: if it doesn't exist, the rest of this config still loads.
(load! "config.local" nil 'noerror)
