;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "candidates-file plug-in")
      (expect '(anything-p-candidats-file-init)
        (assoc-default 'init
                       (car (anything-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")))
                             '(anything-compile-source--candidates-file)))))
      (expect '(anything-p-candidats-file-init
                (lambda () 1))
        (assoc-default 'init
                       (car (anything-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")
                                (init . (lambda () 1))))
                             '(anything-compile-source--candidates-file)))))
      (expect '(anything-p-candidats-file-init
                (lambda () 1))
        (assoc-default 'init
                       (car (anything-compile-sources
                             '(((name . "test")
                                (candidates-file . "test.txt")
                                (init (lambda () 1))))
                             '(anything-compile-source--candidates-file)))))
      ;; FIXME error
      ;; (desc "anything-c-source-buffers")
      ;; (expect '(("Buffers" ("foo" "curbuf")))
      ;;   (stub buffer-list => '("curbuf" " hidden" "foo" "*anything*"))
      ;;   (let ((anything-c-boring-buffer-regexp
      ;;          (rx (or
      ;;               (group bos  " ")
      ;;               "*anything"
      ;;               ;; echo area
      ;;               " *Echo Area" " *Minibuf"))))
      ;;     (flet ((buffer-name (&optional x) x))
      ;;       (anything-test-candidates 'anything-c-source-buffers))))
      (desc "anything-c-stringify")
      (expect "str1"
        (anything-c-stringify "str1"))
      (expect "str2"
        (anything-c-stringify 'str2))
      (desc "anything-c-symbolify")
      (expect 'sym1
        (anything-c-symbolify "sym1"))
      (expect 'sym2
        (anything-c-symbolify 'sym2))
      (desc "plug-in:default-action")
      (expect '(((action ("default" . default) ("original" . original))
                 (default-action . ("default" . default))
                 (action ("original" . original))))
        (anything-compile-sources
         '(((default-action . ("default" . default))
            (action ("original" . original))))
         '(anything-compile-source--default-action)))
      (expect '(((action ("a1" . a1) ("a2" . a2))
                 (default-action . ("a1" . a1))
                 (action ("a1" . a1) ("a2" . a2))))
        (anything-compile-sources
         '(((default-action . ("a1" . a1))
            (action ("a1" . a1) ("a2" . a2))))
         '(anything-compile-source--default-action)))
      (expect '(((action ("a2" . a2) ("a1" . a1))
                 (default-action . ("a2" . a2))
                 (action ("a1" . a1) ("a2" . a2))))
        (anything-compile-sources
         '(((default-action . ("a2" . a2))
            (action ("a1" . a1) ("a2" . a2))))
         '(anything-compile-source--default-action)))
      (desc "anything-c-adaptive-store-selection")
      (expect '(("test adaptative" ("a" ("a" . 1))))
        (let ((anything-c-adaptive-history nil))
          (when (anything
                 :sources '(((name . "test adaptative")
                             (candidates . (list "a" "b" "c" "d"))
                             (action . identity)
                             (filtered-candidate-transformer anything-c-adaptive-sort)))
                 :input "a"
                 :execute-action-at-once-if-one t)
            (anything-c-adaptive-store-selection))
          anything-c-adaptive-history))
      (desc "anything-ff-human-size")
      (expect "6.7G"
        (anything-ff-human-size 7141892608.0))
      (expect "1.2M"
        (anything-ff-human-size 1221554))
      (expect "386.6K"
        (anything-ff-human-size 395897))
      (expect "456"
        (anything-ff-human-size 456)))))
