;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "3.8.4"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "25.1")
    (async "1.9.4")
    (popup "0.5.3")
    (helm-core "3.8.4"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
