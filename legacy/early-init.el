;;; early-init.el --- runs before init.el


;;; Commentary:
;; This is my personal configuration file for Emacs, setting up
;; the UI, packages, and keybindings for development.

;;; Code:

;;; early-init.el
(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Don't resize UI elements during init


;; Remove frame decorations
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(border-width . 0))


;;; early-init.el ends here

