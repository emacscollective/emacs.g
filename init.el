;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package autorevert
  :config
  (setq auto-revert-verbose nil))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package git-commit
  :defer t
  :config
  (setq git-commit-finish-query-functions nil)
  (setq git-commit-summary-max-length 64)
  (remove-hook 'git-commit-setup-hook 'git-commit-setup-changelog-support)
  (remove-hook 'git-commit-setup-hook 'git-commit-propertize-diff)
  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell t))

(use-package git-rebase
  :defer t
  :config
  (setq git-rebase-confirm-cancel nil)
  (setq git-rebase-show-instructions nil))

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
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  :functions (magit-add-section-hook)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (define-key magit-mode-map "f" 'magit-pull-and-fetch-popup)
  (define-key magit-mode-map "F" nil)
  ;;
  ;; Disable safety nets
  (setq magit-commit-squash-confirm nil)
  (setq magit-popup-use-prefix-argument 'default)
  (setq magit-save-repository-buffers 'dontask)
  (add-to-list 'magit-no-confirm 'safe-with-wip t)
  (add-to-list 'magit-no-confirm 'rename t)
  (add-to-list 'magit-no-confirm 'resurrect t)
  (add-to-list 'magit-no-confirm 'trash t)
  ;;
  ;; Disable usage information
  (setq magit-popup-show-help-echo nil)
  (setq magit-popup-show-common-commands nil)
  ;;
  ;; Window managment
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-topleft-v1)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-revision-buffer)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer)
  (add-hook 'magit-section-movement-hook 'magit-log-maybe-update-blob-buffer)
  ;;
  ;; Global settings
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (global-magit-file-mode)
  ;;
  ;; Commit settings
  (setq magit-commit-extend-override-date nil)
  (setq magit-commit-reword-override-date nil)
  ;;
  ;; Branch settings
  (setq magit-branch-adjust-remote-upstream-alist
        '(("master" "master" "next" "maint")))
  ;;
  ;; Push settings
  (setq magit-push-current-set-remote-if-missing 'default)
  ;;
  ;; Status buffer settings
  (setq magit-status-expand-stashes nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-pushremote
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-submodules
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpulled-from-upstream-or-recent
                          'magit-insert-unpulled-from-upstream
                          'replace)
  ;;
  ;; Refs buffer settings
  (define-key magit-branch-section-map "\r" 'magit-show-commit)
  (define-key magit-tag-section-map    "\r" 'magit-show-commit)
  ;;
  ;; Diff buffer settings
  (setq magit-diff-refine-hunk 'all)
  ;;
  ;; Log buffer settings
  (setq magit-log-show-margin nil)
  ;;
  ;; Load extensions
  (require 'magit-rockstar)
  (require 'magit-wip))

(use-package magit-rockstar
  :defer t
  :functions (magit-define-popup-action)
  :config
  (magit-define-popup-action 'magit-commit-popup
    ?n "Reshelve" 'magit-reshelve)
  (magit-define-popup-action 'magit-rebase-popup
    ?R "Rockstar" 'magit-rockstar)
  (magit-define-popup-action 'magit-pull-and-fetch-popup
    ?P "pull request" 'magit-branch-pull-request))

(use-package magit-wip
  :defer t
  :config
  (magit-wip-before-change-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-after-save-mode)
  (setq magit-wip-before-change-mode-lighter "")
  (setq magit-wip-after-apply-mode-lighter "")
  (setq magit-wip-after-save-local-mode-lighter ""))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package shell
  :defer t
  :config
  (require 'with-editor)
  (add-hook 'shell-mode-hook 'with-editor-export-editor))

(use-package simple
  :config (column-number-mode))

(use-package smerge-mode
  :defer t
  :config (setq smerge-refine-ignore-whitespace nil))

(use-package term
  :defer t
  :config
  (require 'with-editor)
  (add-hook 'term-exec-hook 'with-editor-export-editor))

(progn      `text-mode'
  (add-hook 'test-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
