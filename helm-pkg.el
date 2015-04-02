;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "1.6.9"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24")
    (cl-lib "0.5")
    (async "1.2"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
