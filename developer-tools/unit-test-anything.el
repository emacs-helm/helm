;;; unit tests for anything.el
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")


(defmacro helm-test-update (sources pattern)
  "Test helper macro for anything. It is meant for testing *anything* buffer contents."
  `(progn (stub helm-get-sources => ,sources)
          (stub helm-log-run-hook => nil)
          (stub run-with-idle-timer => nil)
          (let (helm-test-mode (helm-pattern ,pattern))
            (helm-update))))


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "helm-current-buffer")
      (expect "__a_buffer"
        (with-current-buffer (get-buffer-create "__a_buffer")
          (helm-test-candidates '(((name . "FOO"))) "")
          (prog1
              (buffer-name helm-current-buffer)
            (kill-buffer "__a_buffer")
            )))
      (desc "helm-buffer-file-name")
      (expect (regexp "/__a_file__")
        (with-current-buffer (get-buffer-create "__a_file__")
          (setq buffer-file-name "/__a_file__")
          (helm-test-candidates '(((name . "FOO"))) "")
          (prog1
              helm-buffer-file-name
            ;;(kill-buffer "__a_file__")
            )))
      (desc "helm-interpret-value")
      (expect "literal"
        (helm-interpret-value "literal"))
      (expect "lambda"
        (helm-interpret-value (lambda () "lambda")))
      (expect "lambda with source name"
        (let ((source '((name . "lambda with source name"))))
          (helm-interpret-value (lambda () helm-source-name) source)))
      (expect "function symbol"
        (flet ((f () "function symbol"))
          (helm-interpret-value 'f)))
      (expect "variable symbol"
        (let ((v "variable symbol"))
          (helm-interpret-value 'v)))
      (expect (error error *)
        (helm-interpret-value 'unbounded-1))
      (desc "helm-compile-sources")
      (expect '(((name . "foo")))
        (helm-compile-sources '(((name . "foo"))) nil)
        )
      (expect '(((name . "foo") (type . test) (action . identity)))
        (let ((helm-type-attributes '((test (action . identity)))))
          (helm-compile-sources '(((name . "foo") (type . test)))
                                    '(helm-compile-source--type))))
      (desc "helm-sources accepts symbols")
      (expect '(((name . "foo")))
        (let* ((foo '((name . "foo"))))
          (helm-compile-sources '(foo) nil)))
      (desc "helm-get-sources action")
      (expect '(((name . "Actions") (candidates . actions)))
        (stub helm-action-window => t)
        (let (helm-compiled-sources
              (helm-sources '(((name . "Actions") (candidates . actions)))))
          (helm-get-sources)))
      (desc "get-buffer-create candidates-buffer")
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer . helm-candidates-in-buffer)
                 (candidates . helm-candidates-in-buffer)
                 (volatile) (match identity)))
        (helm-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer . helm-candidates-in-buffer)))
         '(helm-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (candidates . helm-candidates-in-buffer)
                 (volatile) (match identity)))
        (helm-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer)))
         '(helm-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (type . test)
                 (action . identity)
                 (candidates . helm-candidates-in-buffer)
                 (volatile) (match identity)))
        (let ((helm-type-attributes '((test (action . identity)))))
          (helm-compile-sources
           '(((name . "many") (init . many-init)
              (candidates-in-buffer)
              (type . test)))
           '(helm-compile-source--type
             helm-compile-source--candidates-in-buffer))))

      (desc "helm-get-candidates")
      (expect '("foo" "bar")
        (helm-get-candidates '((name . "foo") (candidates "foo" "bar"))))
      (expect '("FOO" "BAR")
        (helm-get-candidates '((name . "foo") (candidates "foo" "bar")
                                   (candidate-transformer
                                    . (lambda (cands) (mapcar 'upcase cands))))))
      (expect '("foo" "bar")
        (helm-get-candidates '((name . "foo")
                                   (candidates . (lambda () '("foo" "bar"))))))
      (expect '("foo" "bar")
        (let ((var '("foo" "bar")))
          (helm-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (helm-get-candidates '((name . "foo")
                                   (candidates . "err"))))
      (expect (error error *)
        (let ((var "err"))
          (helm-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (helm-get-candidates '((name . "foo")
                                   (candidates . unDeFined-syMbol))))
      (desc "helm-compute-matches")
      (expect '("foo" "bar")
        (let ((helm-pattern ""))
          (helm-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("foo")
        (let ((helm-pattern "oo"))
          (helm-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("bar")
        (let ((helm-pattern "^b"))
          (helm-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("a" "b")
        (let ((helm-pattern "")
              (helm-candidate-number-limit 2))
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b")
        (let ((helm-pattern ".")
              (helm-candidate-number-limit 2))
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((helm-pattern "")
              helm-candidate-number-limit)
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((helm-pattern "[abc]")
              helm-candidate-number-limit)
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '(a b c)
        (let ((helm-pattern "[abc]")
              helm-candidate-number-limit)
          (helm-compute-matches '((name . "FOO") (candidates a b c) (volatile)))))
      (expect '(("foo" . "FOO") ("bar" . "BAR"))
        (let ((helm-pattern ""))
          (helm-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "BAR")) (volatile)))))
      (expect '(("foo" . "FOO"))
        (let ((helm-pattern "foo"))
          (helm-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "foo")) (volatile)))))
      ;; using helm-test-candidate-list
      (desc "helm-test-candidates")
      (expect '(("FOO" ("foo" "bar")))
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar")))))
      (expect '(("FOO" ("bar")))
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar"))) "ar"))
      (expect '(("T1" ("hoge" "aiue"))
                ("T2" ("test" "boke")))
        (helm-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke")))))
      (expect '(("T1" ("hoge"))
                ("T2" ("boke")))
        (helm-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke"))) "o"))
      (desc "requires-pattern attribute")
      (expect nil
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1)))))
      (expect '(("FOO" ("bar")))
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1))) "b"))
      (desc "delayed attribute(for test)")
      (expect '(("T2" ("boke"))
                ("T1" ("hoge")))
        (helm-test-candidates
         '(((name . "T1") (candidates "hoge" "aiue") (delayed))
           ((name . "T2") (candidates "test" "boke")))
         "o"))
      (desc "match attribute(prefix search)")
      (expect '(("FOO" ("bar")))
        (helm-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" helm-pattern) c)))))
         "ba"))
      (expect nil
        (helm-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" helm-pattern) c)))))
         "ar"))
      (expect "TestSource"
        (let (x)
          (helm-test-candidates
           '(((name . "TestSource") (candidates "a")
              (match (lambda (c) (setq x helm-source-name)))))
           "a")
          x))
      (desc "init attribute")
      (expect '(("FOO" ("bar")))
        (let (v)
          (helm-test-candidates
           '(((name . "FOO") (init . (lambda () (setq v '("foo" "bar"))))
              (candidates . v)))
           "ar")))
      (desc "candidate-transformer attribute")
      (expect '(("FOO" ("BAR")))
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (candidate-transformer
                                      . (lambda (cands) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "filtered-candidate-transformer attribute")
      ;; needs more tests
      (expect '(("FOO" ("BAR")))
        (helm-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (filtered-candidate-transformer
                                      . (lambda (cands src) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "helm-transform-candidates in process")
      (expect (mock (helm-composed-funcall-with-source
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     (lambda (cands src) (mapcar 'upcase cands))
                     '("foo" "bar")
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     t))
        (stub helm-process-candidate-transformer => '("foo" "bar"))
        (helm-transform-candidates
         '("foo" "bar")
         '((name . "FOO") (candidates "foo" "bar")
           (filtered-candidate-transformer
            . (lambda (cands src) (mapcar 'upcase cands))))
         t)
        )
      (desc "helm-candidates-in-buffer-1")
      (expect nil
        (helm-candidates-in-buffer-1
         nil ""
         'buffer-substring-no-properties '(re-search-forward) 50 nil))
      (expect '("foo+" "bar+" "baz+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 5 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           'buffer-substring-no-properties '(re-search-forward) 50 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1 
           (current-buffer) "oo+"
           #'buffer-substring-no-properties '(search-forward) 50 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1
           (current-buffer) "."
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '(("foo+" "FOO+"))
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (helm-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           (lambda (s e)
             (let ((l (buffer-substring-no-properties s e)))
               (list l (upcase l))))
           '(re-search-forward) 50 nil)))
      (desc "helm-candidates-in-buffer")
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))))
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (let (helm-candidate-number-limit)
          (helm-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                              (insert "foo+\nbar+\nbaz+\n"))))
              (candidates . helm-candidates-in-buffer)
              (match identity)
              (volatile))))))
      (expect '(("TEST" ("foo+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo\\+"))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("" "a" "b")))
        (helm-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (insert "a\nb\n"))))
            (candidates-in-buffer)))
         "a*"))
      (desc "search attribute")
      (expect '(("TEST" ("foo+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward)
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward re-search-forward)
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search re-search-forward search-forward)
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("ooo" "foo+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search re-search-forward search-forward)
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      ;; faster exact match
      (expect '(("TEST" ("bar+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (and (search-forward (concat "\n" pattern "\n") nil t)
                           (forward-line -1))))
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "bar+"))
      ;; faster prefix match
      (expect '(("TEST" ("bar+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (search-forward (concat "\n" pattern) nil t)))
            (candidates . helm-candidates-in-buffer)
            (match identity)
            (volatile)))
         "ba"))
      (desc "helm-current-buffer-is-modified")
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (helm-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (helm-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources)
            (helm-test-candidates sources))))
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (helm-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources)
            (insert "2")
            (helm-test-candidates sources))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources1)
            (helm-test-candidates sources2))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources1)
            (helm-test-candidates sources2)
            (helm-test-candidates sources1))))
      (expect '(("BAR" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources1)
            (helm-test-candidates sources2)
            (helm-test-candidates sources2))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (helm-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash helm-tick-hash)
            (insert "1")
            (helm-test-candidates sources1)
            (helm-test-candidates sources2)
            (with-temp-buffer
              (helm-test-candidates sources2)))))
      (desc "helm-source-name")
      (expect "FOO"
        (let (v)
          (helm-test-candidates '(((name . "FOO")
                                       (init
                                        . (lambda () (setq v helm-source-name)))
                                       (candidates "ok"))))
          v))
      (expect "FOO"
        (let (v)
          (helm-test-candidates '(((name . "FOO")
                                       (candidates
                                        . (lambda ()
                                            (setq v helm-source-name)
                                            '("ok"))))))
          v))
      (expect "FOO"
        (let (v)
          (helm-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (candidate-transformer
                                        . (lambda (c)
                                            (setq v helm-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (helm-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (filtered-candidate-transformer
                                        . (lambda (c s)
                                            (setq v helm-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (helm-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (display-to-real
                                        . (lambda (c)
                                            (setq v helm-source-name)
                                            c))
                                       (action . identity))))
          (helm-execute-selection-action)
          v))
      (desc "helm-candidate-buffer create")
      (expect " *anything candidates:FOO*"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (buf (helm-candidate-buffer 'global)))
          (prog1 (buffer-name buf)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (helm-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (helm-candidate-buffer 'local)))
          (prog1 (buffer-name buf)
            (kill-buffer helm-current-buffer)
            (kill-buffer buf))))
      (expect 0
        (let (helm-candidate-buffer-alist
              (helm-source-name "FOO") buf)
          (with-current-buffer  (helm-candidate-buffer 'global)
            (insert "1"))
          (setq buf  (helm-candidate-buffer 'global))
          (prog1 (buffer-size buf)
            (kill-buffer buf))))
      (desc "helm-candidate-buffer get-buffer")
      (expect " *anything candidates:FOO*"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (buf (helm-candidate-buffer 'global)))
          (prog1 (buffer-name (helm-candidate-buffer))
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (helm-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (helm-candidate-buffer 'local)))
          (prog1 (buffer-name (helm-candidate-buffer))
            (kill-buffer helm-current-buffer)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (buf-local (helm-candidate-buffer 'local))
               (buf-global (helm-candidate-buffer 'global)))
          (prog1 (buffer-name (helm-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (helm-candidate-buffer-alist
               (helm-source-name "FOO")
               (helm-current-buffer (get-buffer-create "aTestBuffer"))
               (buf-global (helm-candidate-buffer 'global))
               (buf-local (helm-candidate-buffer 'local)))
          (prog1 (buffer-name (helm-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect nil
        (let* (helm-candidate-buffer-alist
               (helm-source-name "NOP__"))
          (helm-candidate-buffer)))
      (desc "helm-candidate-buffer register-buffer")
      (expect " *anything test candidates*"
        (let (helm-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (prog1 (buffer-name (helm-candidate-buffer buf))
              (kill-buffer (current-buffer))))))
      (expect " *anything test candidates*"
        (let (helm-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (helm-candidate-buffer buf)
            (prog1 (buffer-name (helm-candidate-buffer))
              (kill-buffer (current-buffer))))))
      (expect "1\n2\n"
        (let (helm-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (helm-candidate-buffer buf)
            (prog1 (buffer-string)
              (kill-buffer (current-buffer))))))
      (expect "buf1"
        (let (helm-candidate-buffer-alist
              (helm-source-name "foo")
              (buf1 (get-buffer-create "buf1"))
              (buf2 (get-buffer-create "buf2")))
          (helm-candidate-buffer buf1)
          (helm-candidate-buffer buf2)
          (prog1 (buffer-name (helm-candidate-buffer buf1))
            (kill-buffer buf1)
            (kill-buffer buf2))))
      (desc "action attribute")
      (expect "foo"
        (helm-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (helm-execute-selection-action))
      (expect "foo"
        (helm-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . (lambda (c) (identity c)))))))
        (helm-execute-selection-action))
      (desc "helm-get-default-action")
      (expect 'upcase
        (helm-get-default-action '(("upcase" . upcase))))
      (expect 'downcase
        (helm-get-default-action '(("downcase" . downcase))))
      (expect (lambda (x) (capitalize x))
        (helm-get-default-action (lambda (x) (capitalize x))))
      (expect 'identity
        (helm-get-default-action 'identity))
      (desc "helm-execute-selection-action")
      (expect "FOO"
        (helm-execute-selection-action
         "foo" '(("upcase" . upcase))  nil))
      (expect "FOO"
        (helm-execute-selection-action
         "foo" '(("upcase" . (lambda (c) (upcase c)))) nil))

      (desc "display-to-real attribute")
      (expect "FOO"
        (helm-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (display-to-real . upcase)
            (action ("identity" . identity)))))
        (helm-execute-selection-action))
      (desc "cleanup test")
      (expect 'cleaned
        (let (v)
          (helm-test-candidates
           '(((name . "TEST")
              (cleanup . (lambda () (setq v 'cleaned))))))
          v))
      (desc "helm-get-current-source")
      ;; in init/candidates/action/candidate-transformer/filtered-candidate-transformer
      ;; display-to-real/cleanup function
      (expect "FOO"
        (assoc-default
         'name
         (helm-funcall-with-source '((name . "FOO")) 'helm-get-current-source)))
      ;; init
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (helm-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates . (lambda () (setq v (helm-get-current-source)) '("a"))))))
          (assoc-default 'name v)))
      ;; action
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action
               . (lambda (c) (setq v (helm-get-current-source)) c)))))
          (helm-execute-selection-action)
          (assoc-default 'name v)))
      ;; candidate-transformer
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (candidate-transformer
               . (lambda (c) (setq v (helm-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; filtered-candidate-transformer
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (filtered-candidate-transformer
               . (lambda (c s) (setq v (helm-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; action-transformer
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action-transformer
               . (lambda (a c) (setq v (helm-get-current-source)) a))
              (action . identity))))
          (helm-execute-selection-action)
          (assoc-default 'name v)))
      ;; display-to-real
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (init . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                                   (insert "a\n"))))
              (candidates-in-buffer)
              (display-to-real
               . (lambda (c) (setq v (helm-get-current-source)) c))
              (action . identity))))
          (helm-execute-selection-action)
          (assoc-default 'name v)))
      ;; cleanup
      (expect "FOO"
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (cleanup
               . (lambda () (setq v (helm-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates are displayed
      (expect "TEST"
        (helm-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (assoc-default 'name (helm-get-current-source)))
      (desc "helm-attr")
      (expect "FOO"
        (helm-funcall-with-source
         '((name . "FOO"))
         (lambda ()
           (helm-attr 'name))))
      (expect 'fuga
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (hoge . fuga)
              (init . (lambda () (setq v (helm-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (helm-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (hoge)                    ;INCOMPATIBLE!
              (init . (lambda () (setq v (helm-attr 'hoge))))
              (candidates "a"))))
          v))
      (desc "helm-attr*")
      (expect "generic"
        (let (v (value1 "generic"))
          (helm-test-candidates
           '(((name . "FOO")
              (hoge . value1)
              (init . (lambda () (setq v (helm-attr* 'hoge)))))))
          v))
      (desc "helm-attr-defined")
      (expect (non-nil)
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (hoge)
              (init . (lambda () (setq v (helm-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (expect nil
        (let (v)
          (helm-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (helm-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (desc "helm-attrset")
      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge))))
          (helm-attrset 'hoge 77 src)
          src))
      (expect 77
        (helm-attrset 'hoge 77 '((name . "FOO") (hoge))))

      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge . 1))))
          (helm-attrset 'hoge 77 src)
          src))

      (expect '((name . "FOO") (hoge . 77) (x))
        (let ((src '((name . "FOO") (x))))
          (helm-attrset 'hoge 77 src)
          src))
      (expect 77
        (helm-attrset 'hoge 77 '((name . "FOO"))))
      (desc "helm-preselect")
      ;; entire candidate
      (expect "foo"
        (with-current-buffer (helm-create-helm-buffer t)
          (let ((helm-pattern "")
                (helm-test-mode t))
            (helm-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (helm-preselect "foo")
            (helm-get-selection))))
      ;; regexp
      (expect "foo"
        (with-current-buffer (helm-create-helm-buffer t)
          (let ((helm-pattern "")
                (helm-test-mode t))
            (helm-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (helm-preselect "fo+")
            (helm-get-selection))))
      ;; no match -> first entry
      (expect "hoge"
        (with-current-buffer (helm-create-helm-buffer t)
          (let ((helm-pattern "")
                (helm-test-mode t))
            (helm-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (helm-preselect "not found")
            (helm-get-selection))))
      (desc "helm-check-new-input")
      (expect "newpattern"
        (stub helm-update)
        (stub helm-action-window)
        (let ((helm-pattern "pattern"))
          (helm-check-new-input "newpattern")
          helm-pattern))
      ;; helm-input == nil when action window is available
      (expect nil
        (stub helm-update)
        (stub helm-action-window => t)
        (let ((helm-pattern "pattern")
              helm-input)
          (helm-check-new-input "newpattern")
          helm-input))
      ;; helm-input == helm-pattern unless action window is available
      (expect "newpattern"
        (stub helm-update)
        (stub helm-action-window => nil)
        (let ((helm-pattern "pattern")
              helm-input)
          (helm-check-new-input "newpattern")
          helm-input))
      (expect (mock (helm-update))
        (stub helm-action-window)
        (let (helm-pattern)
          (helm-check-new-input "foo")))
      (desc "helm-update")
      (expect (mock (helm-process-source '((name . "1"))))
        (helm-test-update '(((name . "1"))) ""))
      ;; (find-function 'helm-update)
      ;; TODO el-mock.el should express 2nd call of function.
      ;;     (expect (mock (helm-process-source '((name . "2"))))
      ;;       (stub helm-get-sources => '(((name . "1")) ((name . "2"))))
      ;;       (stub helm-log-run-hook)
      ;;       (stub helm-maybe-fit-frame)
      ;;       (stub run-with-idle-timer)
      ;;       (helm-update))
      (expect (mock (run-with-idle-timer * nil 'helm-process-delayed-sources
                                         '(((name . "2") (delayed)))))
        (stub helm-get-sources => '(((name . "1"))
                                        ((name . "2") (delayed))))
        (stub helm-log-run-hook)
        (stub helm-maybe-fit-frame)
        (let ((helm-pattern "") helm-test-mode)
          (helm-update)))

      (desc "requires-pattern attribute")
      (expect (not-called helm-process-source)
        (helm-test-update '(((name . "1") (requires-pattern))) ""))
      (expect (not-called helm-process-source)
        (helm-test-update '(((name . "1") (requires-pattern . 3))) "xx"))

      (desc "helm-normalize-sources")
      (expect '(helm-c-source-test)
        (helm-normalize-sources 'helm-c-source-test))
      (expect '(helm-c-source-test)
        (helm-normalize-sources '(helm-c-source-test)))
      (expect '(helm-c-source-test)
        (let ((helm-sources '(helm-c-source-test)))
          (helm-normalize-sources nil)))
      (expect '(((name . "test")))
        (helm-normalize-sources '((name . "test"))))
      (expect '(((name . "test")))
        (helm-normalize-sources '(((name . "test")))))
      (desc "helm-get-action")
      (expect '(("identity" . identity))
        (stub buffer-size => 1)
        (stub helm-get-current-source => '((name . "test")
                                               (action ("identity" . identity))))
        (helm-get-action))
      (expect 'identity
        (stub buffer-size => 1)
        (stub helm-get-current-source => '((name . "test")
                                               (action . identity)))
        (helm-get-action))
      (expect '((("identity" . identity)) "action-transformer is called")
        (stub buffer-size => 1)
        (stub helm-get-current-source
              => '((name . "test")
                   (action ("identity" . identity))
                   (action-transformer
                    . (lambda (actions selection)
                        (list actions selection)))))
        (stub helm-get-selection => "action-transformer is called")
        (helm-get-action))
      (desc "helm-select-nth-action")
      (expect (error error *)
        (stub helm-get-selection => nil)
        (helm-select-nth-action 0))
      (desc "helm-get-nth-action")
      (expect 'cadr
        (helm-get-nth-action 2 '(("0" . car) ("1" . cdr) ("2" . cadr))))
      (expect (error error *)
        (helm-get-nth-action 2 '(("0" . car))))
      (expect 'identity
        (helm-get-nth-action 0 'identity))
      (expect (error error *)
        (helm-get-nth-action 1 'identity))
      (expect (error error *)
        (helm-get-nth-action 0 'unbound-function-xxx))
      (expect (error error *)
        (helm-get-nth-action 0 "invalid data"))
      (desc "helm-funcall-foreach")
      (expect (mock (upcase "foo"))
        (stub helm-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (helm-funcall-foreach 'init))
      (expect (mock (downcase "bar"))
        (stub helm-get-sources => '(((init . (lambda () (upcase "foo"))))
                                        ((init . (lambda () (downcase "bar"))))))
        (helm-funcall-foreach 'init))
      (expect (not-called helm-funcall-with-source)
        (stub helm-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (helm-funcall-foreach 'not-found))
      ;; TODO helm-select-with-digit-shortcut test
      (desc "helm-get-cached-candidates")
      (expect '("cached" "version")
        (let ((helm-candidate-cache '(("test" "cached" "version"))))
          (helm-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '("new")
        (let ((helm-candidate-cache '(("other" "cached" "version"))))
          (helm-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '(("test" "new")
                ("other" "cached" "version"))
        (let ((helm-candidate-cache '(("other" "cached" "version"))))
          (helm-get-cached-candidates '((name . "test")
                                            (candidates "new")))
          helm-candidate-cache))
      (expect '(("other" "cached" "version"))
        (let ((helm-candidate-cache '(("other" "cached" "version"))))
          (helm-get-cached-candidates '((name . "test")
                                            (candidates "new")
                                            (volatile)))
          helm-candidate-cache))
      ;; TODO when candidates == process
      ;; TODO helm-output-filter
      (desc "candidate-number-limit attribute")
      (expect '("a" "b")
        (let ((helm-pattern "")
              (helm-candidate-number-limit 20))
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b")
        (let ((helm-pattern "[abc]")
              (helm-candidate-number-limit 20))
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b" "c" "d")
        (let ((helm-pattern "[abcd]")
              (helm-candidate-number-limit 2))
          (helm-compute-matches '((name . "FOO") (candidates "a" "b" "c" "d")
                                      (candidate-number-limit) (volatile)))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((helm-candidate-number-limit 2))
          (helm-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . helm-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile))))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((helm-candidate-number-limit 2))
          (helm-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . helm-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile)))
           ".")))
      (desc "multiple init")
      (expect '(1 . 2)
        (let (a b)
          (helm-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))
                    (lambda () (setq b 2))))))
          (cons a b)))
      (expect 1
        (let (a)
          (helm-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))))))
          a))
      (desc "multiple cleanup")
      (expect '(1 . 2)
        (let (a b)
          (helm-test-candidates
           '(((name . "test")
              (cleanup (lambda () (setq a 1))
                       (lambda () (setq b 2))))))
          (cons a b)))
      (desc "helm-mklist")
      (expect '(1)
        (helm-mklist 1))
      (expect '(2)
        (helm-mklist '(2)))
      (expect '((lambda ()))
        (helm-mklist (lambda ())))
      (desc "helm-before-initialize-hook")
      (expect 'called
        (let ((helm-before-initialize-hook '((lambda () (setq v 'called))))
              v)
          (helm-initial-setup)
          v))
      (desc "helm-after-initialize-hook")
      (expect '(b a)
        (let ((helm-before-initialize-hook
               '((lambda () (setq v '(a)))))
              (helm-after-initialize-hook
               '((lambda () (setq v (cons 'b v)))))
              v)
          (helm-initial-setup)
          v))
      (expect 0
        (let ((helm-after-initialize-hook
               '((lambda () (setq v (buffer-size (get-buffer helm-buffer))))))
              v)
          (helm-initial-setup)
          v))
      (desc "get-line attribute")
      (expect '(("TEST" ("FOO+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (get-line . (lambda (s e) (upcase (buffer-substring-no-properties s e))))))
         "oo\\+"))
      (desc "with-helm-restore-variables")
      (expect '(7 8)
        (let ((a 7) (b 8)
              (helm-restored-variables '(a b)))
          (with-helm-restore-variables
            (setq a 0 b 0))
          (list a b)))
      (desc "helm-cleanup-hook")
      (expect 'called
        (let ((helm-cleanup-hook
               '((lambda () (setq v 'called))))
              v)
          (helm-cleanup)
          v))
      (desc "with-helm-display-same-window")
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-helm-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-helm-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-helm-display-same-window
              (switch-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-helm-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-helm-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (desc "search-from-end attribute")
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))))
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))
         "\\+"))
      (expect '(("TEST" ("baz+" "bar+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))))
      (expect '(("TEST" ("baz+" "bar+")))
        (helm-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))
         "\\+"))
      (expect '(("a" ("c2" "c1")))
        (helm-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (insert "c1\nc2\n"))))
            (search-from-end)
            (candidates-in-buffer)))))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("c" "b" "a" "")))
        (helm-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (insert "a\nb\nc\n"))))
            (search-from-end)
            (candidates-in-buffer)))
         "a*"))
      (desc "header-name attribute")
      (expect "original is transformed"
        (helm-test-update '(((name . "original")
                                 (candidates "1")
                                 (header-name
                                  . (lambda (name)
                                      (format "%s is transformed" name)))))
                              "")
        (with-current-buffer (helm-buffer-get)
          (buffer-string)
          (overlay-get (car (overlays-at (1+(point-min)))) 'display)))
      (desc "volatile and match attribute")
      ;; candidates function is called once per `helm-process-delayed-sources'
      (expect 1
        (let ((v 0))
          (helm-test-candidates '(((name . "test")
                                       (candidates . (lambda () (incf v) '("ok")))
                                       (volatile)
                                       (match identity identity identity)))
                                    "o")
          v))
      (desc "accept-empty attribute")
      (expect nil
        (helm-test-candidates
         '(((name . "test") (candidates "") (action . identity))))
        (helm-execute-selection-action))
      (expect ""
        (helm-test-candidates
         '(((name . "test") (candidates "") (action . identity) (accept-empty))))
        (helm-execute-selection-action))
      (desc "helm-tick-hash")
      (expect nil
        (with-current-buffer (get-buffer-create " *00create+*")
          (puthash " *00create+*/xxx" 1 helm-tick-hash)
          (kill-buffer (current-buffer)))
        (gethash " *00create+*/xxx" helm-tick-hash))
      (desc "helm-execute-action-at-once-if-once")
      (expect "HOGE"
        (let ((helm-execute-action-at-once-if-one t))
          (anything '(((name . "one test1")
                       (candidates "hoge")
                       (action . upcase))))))
      (expect "ANY"
        (let ((helm-execute-action-at-once-if-one t))
          (anything '(((name . "one test2")
                       (candidates "hoge" "any")
                       (action . upcase)))
                    "an")))
      ;; candidates > 1
      (expect (mock (read-string "word: " nil))
        (let ((helm-execute-action-at-once-if-one t))
          (anything '(((name . "one test3")
                       (candidates "hoge" "foo" "bar")
                       (action . identity)))
                    nil "word: ")))
      (desc "helm-quit-if-no-candidate")
      (expect nil
        (let ((helm-quit-if-no-candidate t))
          (anything '(((name . "zero test1") (candidates) (action . upcase))))))
      (expect 'called
        (let (v (helm-quit-if-no-candidate (lambda () (setq v 'called))))
          (anything '(((name . "zero test2") (candidates) (action . upcase))))
          v))
      (desc "real-to-display attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (helm-test-candidates '(((name . "test")
                                     (candidates "ddd")
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("test" (("DDD" . "ddd"))))
        (helm-test-candidates '(((name . "test")
                                     (candidates ("ignored" . "ddd"))
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("Commands" (("xxxhoge" . "hoge") ("xxxboke" . "boke"))))
        (helm-test-candidates '(((name . "Commands")
                                     (candidates
                                      "hoge" "boke")
                                     (real-to-display . (lambda (x) (concat "xxx" x)))
                                     (action . identity)))
                                  "xxx"))
      (expect "test\nDDD\n"
        (helm-test-update '(((name . "test")
                                 (candidates "ddd")
                                 (real-to-display . upcase)
                                 (action . identity)))
                              "")
        (with-current-buffer (helm-buffer-get) (buffer-string)))
      (desc "real-to-display and candidate-transformer attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (helm-test-candidates
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nDDD\n"
        (helm-test-update
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))
         "")
        (with-current-buffer (helm-buffer-get) (buffer-string)))
      (desc "real-to-display and candidates-in-buffer")
      (expect '(("test" (("A" . "a") ("B" . "b"))))
        (helm-test-candidates
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nA\nB\n"
        (stub read-string)
        (anything
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity))))
        (with-current-buffer (helm-buffer-get) (buffer-string)))
      (desc "Symbols are acceptable as candidate.")
      (expect '(("test" (sym "str")))
        (helm-test-candidates
         '(((name . "test")
            (candidates sym "str")))))
      (expect '(("test" ((sym . realsym) ("str" . "realstr"))))
        (helm-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))))
      (expect '(("test" (sym)))
        (helm-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "sym"))
      (expect '(("test" ("str")))
        (helm-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "str"))
      (expect '(("test" ((sym . realsym))))
        (helm-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "sym"))
      (expect '(("test" (("str" . "realstr"))))
        (helm-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "str"))
      (desc "multiple transformers")
      (expect '(("test" ("<FOO>")))
        (helm-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             . (lambda (cands)
                 (helm-compose (list cands)
                                   (list (lambda (c) (mapcar 'upcase c))
                                         (lambda (c) (list (concat "<" (car c) ">")))))))))))
      (expect '("<FOO>")
        (helm-composed-funcall-with-source
         '((name . "test"))
         (list (lambda (c) (mapcar 'upcase c))
               (lambda (c) (list (concat "<" (car c) ">"))))
         '("foo"))
        )
      (expect '(("test" ("<FOO>")))
        (helm-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             (lambda (c) (mapcar 'upcase c))
             (lambda (c) (list (concat "<" (car c) ">"))))))))
      (expect '(("test" ("<BAR>")))
        (helm-test-candidates
         '(((name . "test")
            (candidates "bar")
            (filtered-candidate-transformer
             (lambda (c s) (mapcar 'upcase c))
             (lambda (c s) (list (concat "<" (car c) ">"))))))))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub helm-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    . (lambda (a s)
                        (helm-compose
                         (list a s)
                         (list (lambda (a s) (push '("view-file" . view-file) a))
                               (lambda (a s) (push '("find-file" . find-file) a))))))))
        (helm-get-action))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub helm-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    (lambda (a s) (push '("view-file" . view-file) a))
                    (lambda (a s) (push '("find-file" . find-file) a)))))
        (helm-get-action))
      (desc "define-helm-type-attribute")
      (expect '((file (action . find-file)))
        (let (helm-type-attributes)
          (define-helm-type-attribute 'file '((action . find-file)))
          helm-type-attributes))
      (expect '((file (action . find-file)))
        (let ((helm-type-attributes '((file (action . view-file)))))
          (define-helm-type-attribute 'file '((action . find-file)))
          helm-type-attributes))
      (expect '((file (action . find-file))
                (buffer (action . switch-to-buffer)))
        (let (helm-type-attributes)
          (define-helm-type-attribute 'buffer '((action . switch-to-buffer)))
          (define-helm-type-attribute 'file '((action . find-file)))
          helm-type-attributes))
      (desc "helm-approximate-candidate-number")
      (expect 0
        (with-temp-buffer
          (let ((helm-buffer (current-buffer)))
            (helm-approximate-candidate-number))))
      (expect 1
        (with-temp-buffer
          (let ((helm-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n")
            (helm-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((helm-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n"
                    "candiate2\n")
            (<= 2 (helm-approximate-candidate-number)))))
      (expect 1
        (with-temp-buffer
          (let ((helm-buffer (current-buffer)))
            (insert "Title\n"
                    (propertize "multi\nline\n" 'helm-multiline t))
            (helm-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((helm-buffer (current-buffer))
                (helm-candidate-separator "-----"))
            (insert "Title\n"
                    (propertize "multi\nline1\n" 'helm-multiline t)
                    "-----\n"
                    (propertize "multi\nline2\n" 'helm-multiline t))
            (<= 2 (helm-approximate-candidate-number)))))
      (desc "delayed-init attribute")
      (expect 0
        (let ((value 0))
          (helm-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "")
          value))
      (expect 1
        (let ((value 0))
          (helm-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect 2
        (let ((value 0))
          (helm-test-candidates '(((name . "test")
                                       (delayed-init (lambda () (incf value))
                                                     (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect t
        (let (value)
          (with-temp-buffer
            (helm-test-candidates '(((name . "test")
                                         (delayed-init
                                          . (lambda () (setq value
                                                             (eq helm-current-buffer (current-buffer)))))
                                         (candiates "abc")
                                         (requires-pattern . 2)))
                                      "abc")
            value)))
      (desc "pattern-transformer attribute")
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (helm-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (helm-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (helm-test-candidates '(((name . "test1")
                                     (init
                                      . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (init
                                      . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (init
                                      . (lambda () (with-current-buffer (helm-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer)))
                                  "xfoo"))
      (desc "helm-recent-push")
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "baz")))
          (helm-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("foo" "bar" "baz")))
          (helm-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "foo" "baz")))
          (helm-recent-push "foo" 'lst)))
      (desc "helm-require-at-least-version")
      (expect nil
        (helm-require-at-least-version "1.1"))
      (expect nil
        (helm-require-at-least-version "1.200"))
      (expect nil
        (helm-require-at-least-version
         (and (string-match "1\.\\([0-9]+\\)" helm-version)
              (match-string 0 helm-version))))
      (expect (error)
        (helm-require-at-least-version "1.999"))
      (expect (error)
        (helm-require-at-least-version "1.2000"))
      (desc "helm-once")
      (expect 2
        (let ((i 0))
          (helm-test-candidates
           '(((name . "1")
              (init . (lambda () (incf i))))
             ((name . "2")
              (init . (lambda () (incf i))))))
          i))
      (expect 1
        (let ((i 0))
          (helm-test-candidates
           '(((name . "1")
              (init . (lambda () (helm-once (lambda () (incf i))))))
             ((name . "2")
              (init . (lambda () (helm-once (lambda () (incf i))))))))
          i))
      (expect 1
        (let ((i 0))
          (flet ((init1 () (helm-once (lambda () (incf i)))))
            (helm-test-candidates
             '(((name . "1")
                (init . init1))
               ((name . "2")
                (init . init1)))))
          i))
      (desc "helm-marked-candidates")
      (expect '("mark3" "mark1")
        (let* ((source '((name . "mark test")))
               (helm-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub helm-buffer-get => (current-buffer))
          (stub helm-get-current-source => source)
          (helm-marked-candidates)))
      (expect '("current")
        (let* ((source '((name . "mark test")))
               (helm-marked-candidates nil))
          (stub helm-get-current-source => source)
          (stub helm-get-selection => "current")
          (helm-marked-candidates)))
      (desc "helm-marked-candidates with coerce")
      (expect '(mark3 mark1)
        (let* ((source '((name . "mark test")
                         (coerce . intern)))
               (helm-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub helm-buffer-get => (current-buffer))
          (stub helm-get-current-source => source)
          (helm-marked-candidates)))
      (desc "helm-let")
      (expect '(1 10000 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let ((a 1)
                         (b (1+ a))
                         c)
            (helm-create-helm-buffer))
          (with-current-buffer helm-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let ((a 1)
                         (b (1+ a))
                         c)
            (helm-create-helm-buffer))
          (with-current-buffer helm-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let ((a 1)
                         (b (1+ a))
                         c)
            'retval)))
      (desc "helm-let*")
      (expect '(1 2 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let* ((a 1)
                          (b (1+ a))
                          c)
            (helm-create-helm-buffer))
          (with-current-buffer helm-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let* ((a 1)
                          (b (1+ a))
                          c)
            (helm-create-helm-buffer))
          (with-current-buffer helm-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval*
        (let ((a 9999)
              (b 8)
              (c)
              (helm-buffer (exps-tmpbuf)))
          (helm-let* ((a 1)
                          (b (1+ a))
                          c)
            'retval*)))
      (desc "anything with keyword")
      (expect (mock (helm-internal 'test-source "input" "prompt: " nil "preselect" "*test*" nil))
        (anything :sources   'test-source
                  :input     "input"
                  :prompt    "prompt: "
                  :resume    nil
                  :preselect "preselect"
                  :buffer    "*test*"
                  :keymap    nil))
      (expect (mock (helm-internal 'test-source nil nil nil nil "*test*" nil))
        (anything :sources                'test-source
                  :buffer                 "*test*"
                  :candidate-number-limit 20))
      (expect (mock (helm-internal 'test-source nil nil nil nil "*test*" nil))
        (anything 'test-source nil nil nil nil "*test*" nil))
      (desc "helm-log-eval-internal")
      (expect (mock (helm-log "%S = %S" '(+ 1 2) 3))
        (helm-log-eval-internal '((+ 1 2))))
      (expect (mock (helm-log "%S = ERROR!" 'unDeFined))
        (helm-log-eval-internal '(unDeFined)))

      (desc "helm-output-filter--collect-candidates")
      (expect '("a" "b" "")
        (split-string "a\nb\n" "\n"))
      (expect '("a" "b")
        (helm-output-filter--collect-candidates
         '("a" "b" "") (cons 'incomplete-line  "")))
      (expect '("a" "b")
        (split-string "a\nb" "\n"))
      (expect '("a")
        (helm-output-filter--collect-candidates
         '("a" "b") (cons 'incomplete-line  "")))
      (expect '(incomplete-line . "b")
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (helm-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          incomplete-line-info))
      (expect '("" "c" "")
        (split-string "\nc\n" "\n"))
      (expect '("b" "c")
        ;; "a\nb" + "\nc\n"
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (helm-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          (helm-output-filter--collect-candidates
           '("" "c" "") incomplete-line-info)))
      (desc "coerce attribute")
      (expect "string"
        (anything :sources '(((name . "test")
                              (candidates "string")
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'symbol
        (anything :sources '(((name . "test")
                              (candidates "symbol")
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates ("display" . "real"))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates)
                              (candidate-transformer
                               (lambda (c) '(("display" . "real"))))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates)
                              (filtered-candidate-transformer
                               (lambda (c s) '(("display" . "real"))))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates "dummy")
                              (display-to-real (lambda (disp) "real"))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (desc "helm-next-point-in-list")
      (expect 10
        (helm-next-point-in-list 5 '(10 20) nil))
      (expect 20
        (helm-next-point-in-list 15 '(10 20) nil))
      (expect 25
        (helm-next-point-in-list 25 '(10 20) nil))
      (expect 5
        (helm-next-point-in-list 5 '(10 20) t))
      (expect 10
        (helm-next-point-in-list 15 '(10 20) t))
      (expect 20
        (helm-next-point-in-list 25 '(10 20) t))
      (expect 5
        (helm-next-point-in-list 5 '() nil))
      (expect 5
        (helm-next-point-in-list 5 '() t))
      (expect 10
        (helm-next-point-in-list 5 '(10) nil))
      (expect 10
        (helm-next-point-in-list 15 '(10) t))
      (expect 20
        (helm-next-point-in-list 10 '(10 20) nil))
      (expect 10
        (helm-next-point-in-list 20 '(10 20) t))
      (expect 20
        (helm-next-point-in-list 30 '(10 20 30) t))
      )))
