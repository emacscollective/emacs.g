;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
(borg-report-load-duration)
;;; Early birds
(progn ;     startup
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `use-package'
  (setopt use-package-enable-imenu-support t)
  (setopt use-package-verbose t)
  (require 'use-package))

(use-package compat)

(use-package dash
  :config (global-dash-fontify-mode))

(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t))

(use-package epkg
  :defer t
  :init
  (setq epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file nil t)))

(use-package server
  :functions (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time) before-init-time))))

;;; Long tail

(use-package cond-let
  :config
  (font-lock-add-keywords 'emacs-lisp-mode cond-let-font-lock-keywords t))

(use-package copyright
  :defer t
  :config
  (add-hook 'before-save-hook 'copyright-update))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (set-face-attribute 'diff-refine-changed nil :extend t)
  (set-face-attribute 'diff-refine-removed nil :extend t)
  (set-face-attribute 'diff-refine-added   nil :extend t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package sisyphus
  :when (>= emacs-major-version 30)
  :after magit)

(use-package smerge-mode
  :defer t
  :config
  (set-face-attribute 'smerge-refined-removed nil :extend t)
  (set-face-attribute 'smerge-refined-added   nil :extend t))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

;;; Tequila worms

(borg--load-config (concat (user-real-login-name) ".el"))
(borg-report-init-duration)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
