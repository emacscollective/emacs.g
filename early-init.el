;;; early-init.el --- Earliest birds               -*- lexical-binding: t -*-
;;; Load borg

(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;;; Load auto-compile

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;;; Inhibit package

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; _
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
