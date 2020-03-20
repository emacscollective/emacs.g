;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               (cons "melpa" "https://melpa.org/packages/")
               t)
  (add-to-list 'package-archives
               (cons "org" "https://orgmode.org/elpa/")
               t))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
