;;; helm-pkg.el --- define helm for package.el -*- lexical-binding: t -*-

(define-package "helm" "1.6.4"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24")
    (cl-lib "0.5")
    (async "1.2")))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; no-byte-compile: t
;; End:
