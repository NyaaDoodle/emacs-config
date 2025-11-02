;; Straight.el installation and setup
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; No littering
(use-package no-littering)

;; Theme
(load-theme 'modus-vivendi-tinted t)

;; Font
(set-face-attribute 'default nil :family "Hack" :height 200)

;; Remove UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup message
(setq inhibit-startup-message t)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Recent files
(recentf-mode 1)

;; Minibuffer history
(setq history-length 25)
(savehist-mode 1)

;; Restore last cursor location on file
(save-place-mode 1)
