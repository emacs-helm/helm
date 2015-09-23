;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "1.7.9"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24")
    (cl-lib "0.5")
    (async "1.5")
    (helm-core "1.7.9"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
