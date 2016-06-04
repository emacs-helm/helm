;;; helm-pkg.el --- define helm for package.el

(define-package "helm" "1.9.7"
  "Helm is an Emacs incremental and narrowing framework"
  '((emacs "24.3")
    (async "1.9")
    (popup "0.5.3")
    (helm-core "1.9.7"))
  :url "https://emacs-helm.github.io/helm/")

;; Local Variables:
;; no-byte-compile: t
;; End:
