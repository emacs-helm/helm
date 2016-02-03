;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "1.9.2"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24.3")
    (async "1.6")
    (popup "0.5.3")
    (helm-core "1.9.1"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
