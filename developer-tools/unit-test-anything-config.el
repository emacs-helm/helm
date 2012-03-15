;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "candidates-file plug-in")
      (expect '(helm-p-candidats-file-init)
        (assoc-default 'init
                       (car (helm-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")))
                             '(helm-compile-source--candidates-file)))))
      (expect '(helm-p-candidats-file-init
                (lambda () 1))
        (assoc-default 'init
                       (car (helm-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")
                                (init . (lambda () 1))))
                             '(helm-compile-source--candidates-file)))))
      (expect '(helm-p-candidats-file-init
                (lambda () 1))
        (assoc-default 'init
                       (car (helm-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")
                                (init (lambda () 1))))
                             '(helm-compile-source--candidates-file)))))
      ;; FIXME error
      ;; (desc "helm-c-source-buffers")
      ;; (expect '(("Buffers" ("foo" "curbuf")))
      ;;   (stub buffer-list => '("curbuf" " hidden" "foo" "*anything*"))
      ;;   (let ((helm-c-boring-buffer-regexp
      ;;          (rx (or
      ;;               (group bos  " ")
      ;;               "*anything"
      ;;               ;; echo area
      ;;               " *Echo Area" " *Minibuf"))))
      ;;     (flet ((buffer-name (&optional x) x))
      ;;       (helm-test-candidates 'helm-c-source-buffers))))
      (desc "helm-c-stringify")
      (expect "str1"
        (helm-c-stringify "str1"))
      (expect "str2"
        (helm-c-stringify 'str2))
      (desc "helm-c-symbolify")
      (expect 'sym1
        (helm-c-symbolify "sym1"))
      (expect 'sym2
        (helm-c-symbolify 'sym2))
      (desc "plug-in:default-action")
      (expect '(((action ("default" . default) ("original" . original))
                 (default-action . ("default" . default))
                 (action ("original" . original))))
        (helm-compile-sources
         '(((default-action . ("default" . default))
            (action ("original" . original))))
         '(helm-compile-source--default-action)))
      (expect '(((action ("a1" . a1) ("a2" . a2))
                 (default-action . ("a1" . a1))
                 (action ("a1" . a1) ("a2" . a2))))
        (helm-compile-sources
         '(((default-action . ("a1" . a1))
            (action ("a1" . a1) ("a2" . a2))))
         '(helm-compile-source--default-action)))
      (expect '(((action ("a2" . a2) ("a1" . a1))
                 (default-action . ("a2" . a2))
                 (action ("a1" . a1) ("a2" . a2))))
        (helm-compile-sources
         '(((default-action . ("a2" . a2))
            (action ("a1" . a1) ("a2" . a2))))
         '(helm-compile-source--default-action)))
      (desc "helm-c-adaptive-store-selection")
      (expect '(("test adaptative" ("a" ("a" . 1))))
        (let ((helm-c-adaptive-history nil))
          (when (anything
                 :sources '(((name . "test adaptative")
                             (candidates . (list "a" "b" "c" "d"))
                             (action . identity)
                             (filtered-candidate-transformer helm-c-adaptive-sort)))
                 :input "a"
                 :execute-action-at-once-if-one t)
            (helm-c-adaptive-store-selection))
          helm-c-adaptive-history))
      (desc "helm-ff-human-size")
      (expect "6.7G"
        (helm-ff-human-size 7141892608.0))
      (expect "1.2M"
        (helm-ff-human-size 1221554))
      (expect "386.6K"
        (helm-ff-human-size 395897))
      (expect "456"
        (helm-ff-human-size 456)))))
