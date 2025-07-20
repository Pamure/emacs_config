;;; init.el --- My Personal Emacs Configuration

;;; Commentary:
;; This is my personal configuration file for Emacs, setting up
;; the UI, packages, and keybindings for development.

;;; Code:

;; --- Pre-declare variables to silence compiler warnings ---
(defvar projectile-command-map)

;; --- Package Setup ---
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq default-directory "/mnt/z/CodiCodi/")

;; Select yank behavior
(delete-selection-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(use-package all-the-icons)

;;--- UI Tweaks ---
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-confirm))

;; --- Modeline ---
;; --- Modeline tweaks ---
(display-time-mode 1)

;; Make sure powerline is available (spaceline depends on it)
(use-package powerline
  :ensure t)

;; Core spaceline
(use-package spaceline
  :ensure t
  :after powerline)

;; Icon-powered spaceline
(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
  ;; Separator style (slant/arrow/utf-boundary/utf-right-thin/etc.)
  (setq spaceline-all-the-icons-separator-type 'slant)

  ;; Left side: buffer state, name, VC info
  (setq spaceline-all-the-icons-left-segments
        '((buffer-modified :fallback "●")   ; little dot if modified
          buffer-id                          ; buffer name
          (version-control :when active)     ; git branch
          (buffer-size :when >1mb)))         ; hide if <1MB

  ;; Right side: major mode, LSP/flycheck, position
  (setq spaceline-all-the-icons-right-segments
        '((major-mode)                       ; e.g. “Emacs-Lisp”
          (flycheck-error flycheck-warning flycheck-info)
          (lsp-status :when active)          ; LSP diagnostics
          (line-column)                      ; “L23:C5”
          (percent-position)))               ; “Top/50%/Bot”

  ;; Finally activate the theme
  (spaceline-all-the-icons-theme))
;;;; Navigation & Visual Aids
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-word-1)))

(use-package beacon
  :config (beacon-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Set the indent guide color every time the mode is enabled
(defun my-set-guide-colors-on-enable ()
  "Set the foreground color for character-based indent guides."
  (set-face-foreground 'highlight-indent-guides-character-face "#ffffff"))
(add-hook 'highlight-indent-guides-mode-hook #'my-set-guide-colors-on-enable)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; --- File Tree (Treemacs) ---
(use-package treemacs
  :defer t
  :commands (treemacs treemacs-select-window)
  :bind (("<f8>" . treemacs))
  :config
  (setq treemacs-width 30
        treemacs-is-never-other-window t)
  (use-package treemacs-all-the-icons
    :after (treemacs all-the-icons)
    :config
    (treemacs-load-theme "all-the-icons")))

;; Delay projectile keybinding until it's ready
(with-eval-after-load 'projectile
  (with-eval-after-load 'treemacs
    (define-key projectile-command-map (kbd "t") #'treemacs-projectile)))


;; --- Completion UI Stack ---
(use-package vertico :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil))

(use-package marginalia :init (marginalia-mode))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))


;; --- Environment ---
(setenv "JAVA_HOME" "/usr/lib/jvm/java-21-openjdk-amd64")
(setenv "PATH" (concat "/usr/lib/jvm/java-21-openjdk-amd64/bin:" (getenv "PATH")))


;; --- Templates ---
(use-package autoinsert
  :config
  (auto-insert-mode 1)
  (setq auto-insert-directory (expand-file-name "templates" user-emacs-directory)
        auto-insert-query nil)
  (define-auto-insert "\\.java\\'"
    (lambda ()
      (insert (format "public class %s {\n    public static void main(String[] args) {\n        System.out.println(\"Hello, World!\");\n    }\n}\n" (file-name-base (buffer-file-name))))))
  (define-auto-insert "\\.cpp\\'"
    (lambda ()
      (insert "#include <iostream>\nusing namespace std;\n\nint main() {\n    cout << \"Hello, World!\" << endl;\n    return 0;\n}\n"))))


;; --- Dev Tools ---
(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'default)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package ripgrep)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package yasnippet :init (yas-global-mode 1))
(use-package yasnippet-snippets)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :config (setq undo-tree-enable-undo-in-region t))


;; --- Magit ---
(use-package magit
  :defer t
  :commands magit-status
  :bind ("C-x g" . magit-status))


;; --- Which Key ---
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))


;; --- LSP Configuration ---
(use-package lsp-mode
  :commands lsp
  :hook ((java-mode . lsp)
         (python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-response-timeout 30)
  (lsp-restart 'auto-restart)
  (lsp-log-io nil)
  (lsp-diagnostics-provider :auto)
  (lsp-auto-configure t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-signature-auto-activate t)
  (lsp-lens-enable t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-show-code-actions-via 'which-key)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)))


;; --- Language Servers ---
(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-save-actions-organize-imports nil))

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


;; --- Compile and Run ---
(defun my-compile-and-run ()
  "Compile and run the current file based on file extension."
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-extension (file-name-extension (buffer-file-name))))
    (cond
     ((string= file-extension "java")
      (let* ((class-name (file-name-sans-extension (file-name-nondirectory file-name)))
             (compile-command (format "javac %s && java %s" file-name class-name)))
        (compile compile-command)))
     ((string= file-extension "py")
      (compile (format "python3 %s" file-name)))
     ((string= file-extension "c")
      (let* ((exe-name (file-name-sans-extension file-name))
             (compile-command (format "gcc %s -o %s && ./%s" file-name exe-name exe-name)))
        (compile compile-command)))
     ((or (string= file-extension "cpp") (string= file-extension "cc") (string= file-extension "cxx"))
      (let* ((exe-name (file-name-sans-extension file-name))
             (compile-command (format "g++ %s -o %s && ./%s" file-name exe-name exe-name)))
        (compile compile-command)))
     (t (message "No compile rule for file type: %s" file-extension)))))

(global-set-key (kbd "<f5>") 'my-compile-and-run)


;; --- Global Keybindings ---
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l h") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c l e") 'flycheck-list-errors)
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c l i") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c l s") 'lsp-workspace-symbol)
  (define-key lsp-mode-map (kbd "C-c l R") 'lsp-restart-workspace))


;; --- Global Behavior ---
(global-display-line-numbers-mode 1)
(windmove-default-keybindings)


;; --- Session Save & Restore ---
;; --- Session Save & Restore ---
(defvar desktop-dirname)
(defvar desktop-base-file-name)
(defvar desktop-path)
(defvar desktop-save)
(defvar desktop-restore-eager)
(defvar desktop-auto-save-timeout)
(defvar save-place-file)

;; Save cursor position in files
(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))

;; Desktop save session settings
(setq desktop-dirname             (expand-file-name "desktop/" user-emacs-directory)
      desktop-base-file-name      "emacs-desktop"
      desktop-path                (list desktop-dirname)
      desktop-save                'always
      desktop-restore-eager       10
      desktop-auto-save-timeout   10
      desktop-load-locked-desktop t)

(unless (file-directory-p desktop-dirname)
  (make-directory desktop-dirname t))

(desktop-save-mode 1)

;; Auto-read the saved session if it exists
(when (file-exists-p (desktop-full-file-name))
  (desktop-read))

;; --- Finalize ---
(provide 'init)
;;; init.el ends here
