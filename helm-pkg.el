;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "2.7.1"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24.4")
    (async "1.9.2")
    (popup "0.5.3")
    (helm-core "2.7.1"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
