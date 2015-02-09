;;; helm-pkg.el --- define helm for package.el -*- lexical-binding: t -*-

(define-package "helm" "1.6.7"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24")
    (cl-lib "0.5")
    (async "1.2"))
  :url "http://github.com/emacs-helm/helm")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; no-byte-compile: t
;; End:
