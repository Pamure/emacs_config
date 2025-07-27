;;; init.el --- My Personal Emacs Configuration

;;; Commentary:
;; This is a refined version of my Emacs configuration, focused on
;; performance, organization, and robustness.

;;; Code:


;; --- Package & Performance Setup ---
;;; --- Package Management ---
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Initialize packages
(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;; --- Core Emacs Behavior ---
;; NOTE: Grouped core settings into a single block for clarity.
(use-package emacs
  :ensure nil ; Special block for built-in settings
  :config
  (delete-selection-mode 1)		
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (global-display-line-numbers-mode 1)
  (windmove-default-keybindings))

;; --- UI Tweaks ---
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; --- Modeline ---
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                " %b "                             ; buffer name
                " (%m) "                           ; major mode
                " [%l:%c] "                        ; line:column
		";)"
                mode-line-end-spaces))

;; --- Navigation & Visual Aids ---
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-word-1)))

(use-package beacon
  :config (beacon-mode 1))


(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  ;; First, define the function.
  (defun my-set-guide-colors-on-enable ()
    "Set the foreground color for character-based indent guides."
    (set-face-foreground 'highlight-indent-guides-character-face "#ffffff"))
  ;; THEN, add it to the hook.
  (add-hook 'highlight-indent-guides-mode-hook #'my-set-guide-colors-on-enable)
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- File Tree (Treemacs) ---
(use-package treemacs
  :bind (("<f8>" . treemacs))
  :config
  (setq treemacs-width 30
        treemacs-is-never-other-window t)
  ;; NOTE: Simplified projectile integration using :after.
  (use-package treemacs-all-the-icons
    :after (treemacs all-the-icons)
    :config (treemacs-load-theme "all-the-icons")))

;; --- Completion UI Stack ---
(use-package vertico :init (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil))
(use-package marginalia :init (marginalia-mode))
(use-package corfu
  :init (global-corfu-mode)
  :custom (corfu-auto t) (corfu-cycle t) (corfu-preselect 'prompt))
(use-package cape
  :init (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; --- Dev Tools ---
(use-package ripgrep)
(use-package flycheck
  :init (global-flycheck-mode)
  ;; NOTE: Moved keybindings into the relevant package block.
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)))
(use-package yasnippet :init (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package undo-tree :init (global-undo-tree-mode 1))
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;; --- LSP Configuration ---
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; NOTE: Using lsp-deferred for faster startup. All language hooks are here.
  :hook ((java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-log-io nil)
  :bind (:map lsp-mode-map
              ("C-c l f" . lsp-format-buffer)
              ("C-c l r" . lsp-rename)
              ("C-c l a" . lsp-execute-code-action)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom) (lsp-ui-sideline-enable nil))

;; --- Language Servers ---
(use-package lsp-java :config (setq lsp-java-save-actions-organize-imports t))
(use-package lsp-pyright)

;; --- Compile and Run ---
(defun my-compile-and-run ()
  "Save the current buffer, then compile and run it."
  (interactive)
  (save-buffer) ; NOTE: Added this to ensure you're running the latest version.
  (let* ((file-name (buffer-file-name))
         (file-base (file-name-sans-extension file-name))
         (file-ext (file-name-extension file-name)))
    (cond
     ((string= file-ext "java") (compile (format "javac %s && java %s" file-name (file-name-nondirectory file-base))))
     ((string= file-ext "py") (compile (format "python3 %s" file-name)))
     ((string= file-ext "c") (compile (format "gcc %s -o %s && ./%s" file-name file-base file-base)))
     ((or (string= file-ext "cpp") (string= file-ext "cc")) (compile (format "g++ %s -o %s && ./%s" file-name file-base file-base)))
     (t (message "No compile rule for file type: %s" file-ext)))))
(global-set-key (kbd "<f5>") 'my-compile-and-run)



;; --- Session Save & Restore ---
(use-package desktop
  :ensure nil ; Built-in
  :config
  (setq desktop-dirname (expand-file-name ".cache/desktop/" user-emacs-directory)
        desktop-base-file-name "emacs-desktop"
        desktop-path (list desktop-dirname)
        desktop-auto-save-timeout 60)
  ;; NOTE: This check is important and correctly placed.
  (unless (file-directory-p desktop-dirname)
    (make-directory desktop-dirname t))
  (desktop-save-mode 1)
  ;; NOTE: This is the correct way to save on exit.
  (add-hook 'kill-emacs-hook (lambda () (desktop-save desktop-dirname))))

;; --- Finalize ---
(provide 'init)
;;; init.el ends here
