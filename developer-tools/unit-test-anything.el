;;; unit tests for anything.el
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")


(defmacro anything-test-update (sources pattern)
  "Test helper macro for anything. It is meant for testing *anything* buffer contents."
  `(progn (stub anything-get-sources => ,sources)
          (stub anything-log-run-hook => nil)
          (stub run-with-idle-timer => nil)
          (let (anything-test-mode (anything-pattern ,pattern))
            (anything-update))))


(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "anything-current-buffer")
      (expect "__a_buffer"
        (with-current-buffer (get-buffer-create "__a_buffer")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              (buffer-name anything-current-buffer)
            (kill-buffer "__a_buffer")
            )))
      (desc "anything-buffer-file-name")
      (expect (regexp "/__a_file__")
        (with-current-buffer (get-buffer-create "__a_file__")
          (setq buffer-file-name "/__a_file__")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              anything-buffer-file-name
            ;;(kill-buffer "__a_file__")
            )))
      (desc "anything-interpret-value")
      (expect "literal"
        (anything-interpret-value "literal"))
      (expect "lambda"
        (anything-interpret-value (lambda () "lambda")))
      (expect "lambda with source name"
        (let ((source '((name . "lambda with source name"))))
          (anything-interpret-value (lambda () anything-source-name) source)))
      (expect "function symbol"
        (flet ((f () "function symbol"))
          (anything-interpret-value 'f)))
      (expect "variable symbol"
        (let ((v "variable symbol"))
          (anything-interpret-value 'v)))
      (expect (error error *)
        (anything-interpret-value 'unbounded-1))
      (desc "anything-compile-sources")
      (expect '(((name . "foo")))
        (anything-compile-sources '(((name . "foo"))) nil)
        )
      (expect '(((name . "foo") (type . test) (action . identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources '(((name . "foo") (type . test)))
                                    '(anything-compile-source--type))))
      (desc "anything-sources accepts symbols")
      (expect '(((name . "foo")))
        (let* ((foo '((name . "foo"))))
          (anything-compile-sources '(foo) nil)))
      (desc "anything-get-sources action")
      (expect '(((name . "Actions") (candidates . actions)))
        (stub anything-action-window => t)
        (let (anything-compiled-sources
              (anything-sources '(((name . "Actions") (candidates . actions)))))
          (anything-get-sources)))
      (desc "get-buffer-create candidates-buffer")
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer . anything-candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer . anything-candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (type . test)
                 (action . identity)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources
           '(((name . "many") (init . many-init)
              (candidates-in-buffer)
              (type . test)))
           '(anything-compile-source--type
             anything-compile-source--candidates-in-buffer))))

      (desc "anything-get-candidates")
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar"))))
      (expect '("FOO" "BAR")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar")
                                   (candidate-transformer
                                    . (lambda (cands) (mapcar 'upcase cands))))))
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo")
                                   (candidates . (lambda () '("foo" "bar"))))))
      (expect '("foo" "bar")
        (let ((var '("foo" "bar")))
          (anything-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (anything-get-candidates '((name . "foo")
                                   (candidates . "err"))))
      (expect (error error *)
        (let ((var "err"))
          (anything-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (anything-get-candidates '((name . "foo")
                                   (candidates . unDeFined-syMbol))))
      (desc "anything-compute-matches")
      (expect '("foo" "bar")
        (let ((anything-pattern ""))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("foo")
        (let ((anything-pattern "oo"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("bar")
        (let ((anything-pattern "^b"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern ".")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "[abc]")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '(a b c)
        (let ((anything-pattern "[abc]")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates a b c) (volatile)))))
      (expect '(("foo" . "FOO") ("bar" . "BAR"))
        (let ((anything-pattern ""))
          (anything-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "BAR")) (volatile)))))
      (expect '(("foo" . "FOO"))
        (let ((anything-pattern "foo"))
          (anything-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "foo")) (volatile)))))
      ;; using anything-test-candidate-list
      (desc "anything-test-candidates")
      (expect '(("FOO" ("foo" "bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar"))) "ar"))
      (expect '(("T1" ("hoge" "aiue"))
                ("T2" ("test" "boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke")))))
      (expect '(("T1" ("hoge"))
                ("T2" ("boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke"))) "o"))
      (desc "requires-pattern attribute")
      (expect nil
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1)))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1))) "b"))
      (desc "delayed attribute(for test)")
      (expect '(("T2" ("boke"))
                ("T1" ("hoge")))
        (anything-test-candidates
         '(((name . "T1") (candidates "hoge" "aiue") (delayed))
           ((name . "T2") (candidates "test" "boke")))
         "o"))
      (desc "match attribute(prefix search)")
      (expect '(("FOO" ("bar")))
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ba"))
      (expect nil
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ar"))
      (expect "TestSource"
        (let (x)
          (anything-test-candidates
           '(((name . "TestSource") (candidates "a")
              (match (lambda (c) (setq x anything-source-name)))))
           "a")
          x))
      (desc "init attribute")
      (expect '(("FOO" ("bar")))
        (let (v)
          (anything-test-candidates
           '(((name . "FOO") (init . (lambda () (setq v '("foo" "bar"))))
              (candidates . v)))
           "ar")))
      (desc "candidate-transformer attribute")
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (candidate-transformer
                                      . (lambda (cands) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "filtered-candidate-transformer attribute")
      ;; needs more tests
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (filtered-candidate-transformer
                                      . (lambda (cands src) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "anything-transform-candidates in process")
      (expect (mock (anything-composed-funcall-with-source
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     (lambda (cands src) (mapcar 'upcase cands))
                     '("foo" "bar")
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     t))
        (stub anything-process-candidate-transformer => '("foo" "bar"))
        (anything-transform-candidates
         '("foo" "bar")
         '((name . "FOO") (candidates "foo" "bar")
           (filtered-candidate-transformer
            . (lambda (cands src) (mapcar 'upcase cands))))
         t)
        )
      (desc "anything-candidates-in-buffer-1")
      (expect nil
        (anything-candidates-in-buffer-1
         nil ""
         'buffer-substring-no-properties '(re-search-forward) 50 nil))
      (expect '("foo+" "bar+" "baz+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 5 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           'buffer-substring-no-properties '(re-search-forward) 50 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1 
           (current-buffer) "oo+"
           #'buffer-substring-no-properties '(search-forward) 50 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "."
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '(("foo+" "FOO+"))
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           (lambda (s e)
             (let ((l (buffer-substring-no-properties s e)))
               (list l (upcase l))))
           '(re-search-forward) 50 nil)))
      (desc "anything-candidates-in-buffer")
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))))
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (let (anything-candidate-number-limit)
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "foo+\nbar+\nbaz+\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (volatile))))))
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo\\+"))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("" "a" "b")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "a\nb\n"))))
            (candidates-in-buffer)))
         "a*"))
      (desc "search attribute")
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward re-search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("ooo" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      ;; faster exact match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (and (search-forward (concat "\n" pattern "\n") nil t)
                           (forward-line -1))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "bar+"))
      ;; faster prefix match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (search-forward (concat "\n" pattern) nil t)))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "ba"))
      (desc "anything-current-buffer-is-modified")
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (anything-test-candidates sources))))
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (insert "2")
            (anything-test-candidates sources))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources1))))
      (expect '(("BAR" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources2))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (with-temp-buffer
              (anything-test-candidates sources2)))))
      (desc "anything-source-name")
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (init
                                        . (lambda () (setq v anything-source-name)))
                                       (candidates "ok"))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates
                                        . (lambda ()
                                            (setq v anything-source-name)
                                            '("ok"))))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (candidate-transformer
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (filtered-candidate-transformer
                                        . (lambda (c s)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (display-to-real
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c))
                                       (action . identity))))
          (anything-execute-selection-action)
          v))
      (desc "anything-candidate-buffer create")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name buf)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name buf)
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect 0
        (let (anything-candidate-buffer-alist
              (anything-source-name "FOO") buf)
          (with-current-buffer  (anything-candidate-buffer 'global)
            (insert "1"))
          (setq buf  (anything-candidate-buffer 'global))
          (prog1 (buffer-size buf)
            (kill-buffer buf))))
      (desc "anything-candidate-buffer get-buffer")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf-local (anything-candidate-buffer 'local))
               (buf-global (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf-global (anything-candidate-buffer 'global))
               (buf-local (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect nil
        (let* (anything-candidate-buffer-alist
               (anything-source-name "NOP__"))
          (anything-candidate-buffer)))
      (desc "anything-candidate-buffer register-buffer")
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (prog1 (buffer-name (anything-candidate-buffer buf))
              (kill-buffer (current-buffer))))))
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-name (anything-candidate-buffer))
              (kill-buffer (current-buffer))))))
      (expect "1\n2\n"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-string)
              (kill-buffer (current-buffer))))))
      (expect "buf1"
        (let (anything-candidate-buffer-alist
              (anything-source-name "foo")
              (buf1 (get-buffer-create "buf1"))
              (buf2 (get-buffer-create "buf2")))
          (anything-candidate-buffer buf1)
          (anything-candidate-buffer buf2)
          (prog1 (buffer-name (anything-candidate-buffer buf1))
            (kill-buffer buf1)
            (kill-buffer buf2))))
      (desc "action attribute")
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . (lambda (c) (identity c)))))))
        (anything-execute-selection-action))
      (desc "anything-get-default-action")
      (expect 'upcase
        (anything-get-default-action '(("upcase" . upcase))))
      (expect 'downcase
        (anything-get-default-action '(("downcase" . downcase))))
      (expect (lambda (x) (capitalize x))
        (anything-get-default-action (lambda (x) (capitalize x))))
      (expect 'identity
        (anything-get-default-action 'identity))
      (desc "anything-execute-selection-action")
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . upcase))  nil))
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . (lambda (c) (upcase c)))) nil))

      (desc "display-to-real attribute")
      (expect "FOO"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (display-to-real . upcase)
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (desc "cleanup test")
      (expect 'cleaned
        (let (v)
          (anything-test-candidates
           '(((name . "TEST")
              (cleanup . (lambda () (setq v 'cleaned))))))
          v))
      (desc "anything-get-current-source")
      ;; in init/candidates/action/candidate-transformer/filtered-candidate-transformer
      ;; display-to-real/cleanup function
      (expect "FOO"
        (assoc-default
         'name
         (anything-funcall-with-source '((name . "FOO")) 'anything-get-current-source)))
      ;; init
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates . (lambda () (setq v (anything-get-current-source)) '("a"))))))
          (assoc-default 'name v)))
      ;; action
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (candidate-transformer
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; filtered-candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (filtered-candidate-transformer
               . (lambda (c s) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; action-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action-transformer
               . (lambda (a c) (setq v (anything-get-current-source)) a))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; display-to-real
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                   (insert "a\n"))))
              (candidates-in-buffer)
              (display-to-real
               . (lambda (c) (setq v (anything-get-current-source)) c))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; cleanup
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (cleanup
               . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates are displayed
      (expect "TEST"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (assoc-default 'name (anything-get-current-source)))
      (desc "anything-attr")
      (expect "FOO"
        (anything-funcall-with-source
         '((name . "FOO"))
         (lambda ()
           (anything-attr 'name))))
      (expect 'fuga
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge . fuga)
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)                    ;INCOMPATIBLE!
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (desc "anything-attr*")
      (expect "generic"
        (let (v (value1 "generic"))
          (anything-test-candidates
           '(((name . "FOO")
              (hoge . value1)
              (init . (lambda () (setq v (anything-attr* 'hoge)))))))
          v))
      (desc "anything-attr-defined")
      (expect (non-nil)
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (desc "anything-attrset")
      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO") (hoge))))

      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge . 1))))
          (anything-attrset 'hoge 77 src)
          src))

      (expect '((name . "FOO") (hoge . 77) (x))
        (let ((src '((name . "FOO") (x))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO"))))
      (desc "anything-preselect")
      ;; entire candidate
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "foo")
            (anything-get-selection))))
      ;; regexp
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "fo+")
            (anything-get-selection))))
      ;; no match -> first entry
      (expect "hoge"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "not found")
            (anything-get-selection))))
      (desc "anything-check-new-input")
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window)
        (let ((anything-pattern "pattern"))
          (anything-check-new-input "newpattern")
          anything-pattern))
      ;; anything-input == nil when action window is available
      (expect nil
        (stub anything-update)
        (stub anything-action-window => t)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      ;; anything-input == anything-pattern unless action window is available
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window => nil)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      (expect (mock (anything-update))
        (stub anything-action-window)
        (let (anything-pattern)
          (anything-check-new-input "foo")))
      (desc "anything-update")
      (expect (mock (anything-process-source '((name . "1"))))
        (anything-test-update '(((name . "1"))) ""))
      ;; (find-function 'anything-update)
      ;; TODO el-mock.el should express 2nd call of function.
      ;;     (expect (mock (anything-process-source '((name . "2"))))
      ;;       (stub anything-get-sources => '(((name . "1")) ((name . "2"))))
      ;;       (stub anything-log-run-hook)
      ;;       (stub anything-maybe-fit-frame)
      ;;       (stub run-with-idle-timer)
      ;;       (anything-update))
      (expect (mock (run-with-idle-timer * nil 'anything-process-delayed-sources
                                         '(((name . "2") (delayed)))))
        (stub anything-get-sources => '(((name . "1"))
                                        ((name . "2") (delayed))))
        (stub anything-log-run-hook)
        (stub anything-maybe-fit-frame)
        (let ((anything-pattern "") anything-test-mode)
          (anything-update)))

      (desc "requires-pattern attribute")
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern))) ""))
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern . 3))) "xx"))

      (desc "anything-normalize-sources")
      (expect '(anything-c-source-test)
        (anything-normalize-sources 'anything-c-source-test))
      (expect '(anything-c-source-test)
        (anything-normalize-sources '(anything-c-source-test)))
      (expect '(anything-c-source-test)
        (let ((anything-sources '(anything-c-source-test)))
          (anything-normalize-sources nil)))
      (expect '(((name . "test")))
        (anything-normalize-sources '((name . "test"))))
      (expect '(((name . "test")))
        (anything-normalize-sources '(((name . "test")))))
      (desc "anything-get-action")
      (expect '(("identity" . identity))
        (stub buffer-size => 1)
        (stub anything-get-current-source => '((name . "test")
                                               (action ("identity" . identity))))
        (anything-get-action))
      (expect 'identity
        (stub buffer-size => 1)
        (stub anything-get-current-source => '((name . "test")
                                               (action . identity)))
        (anything-get-action))
      (expect '((("identity" . identity)) "action-transformer is called")
        (stub buffer-size => 1)
        (stub anything-get-current-source
              => '((name . "test")
                   (action ("identity" . identity))
                   (action-transformer
                    . (lambda (actions selection)
                        (list actions selection)))))
        (stub anything-get-selection => "action-transformer is called")
        (anything-get-action))
      (desc "anything-select-nth-action")
      (expect (error error *)
        (stub anything-get-selection => nil)
        (anything-select-nth-action 0))
      (desc "anything-get-nth-action")
      (expect 'cadr
        (anything-get-nth-action 2 '(("0" . car) ("1" . cdr) ("2" . cadr))))
      (expect (error error *)
        (anything-get-nth-action 2 '(("0" . car))))
      (expect 'identity
        (anything-get-nth-action 0 'identity))
      (expect (error error *)
        (anything-get-nth-action 1 'identity))
      (expect (error error *)
        (anything-get-nth-action 0 'unbound-function-xxx))
      (expect (error error *)
        (anything-get-nth-action 0 "invalid data"))
      (desc "anything-funcall-foreach")
      (expect (mock (upcase "foo"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'init))
      (expect (mock (downcase "bar"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))
                                        ((init . (lambda () (downcase "bar"))))))
        (anything-funcall-foreach 'init))
      (expect (not-called anything-funcall-with-source)
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'not-found))
      ;; TODO anything-select-with-digit-shortcut test
      (desc "anything-get-cached-candidates")
      (expect '("cached" "version")
        (let ((anything-candidate-cache '(("test" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '("new")
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '(("test" "new")
                ("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))
          anything-candidate-cache))
      (expect '(("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")
                                            (volatile)))
          anything-candidate-cache))
      ;; TODO when candidates == process
      ;; TODO anything-output-filter
      (desc "candidate-number-limit attribute")
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "[abc]")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b" "c" "d")
        (let ((anything-pattern "[abcd]")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c" "d")
                                      (candidate-number-limit) (volatile)))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile))))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile)))
           ".")))
      (desc "multiple init")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))
                    (lambda () (setq b 2))))))
          (cons a b)))
      (expect 1
        (let (a)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))))))
          a))
      (desc "multiple cleanup")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (cleanup (lambda () (setq a 1))
                       (lambda () (setq b 2))))))
          (cons a b)))
      (desc "anything-mklist")
      (expect '(1)
        (anything-mklist 1))
      (expect '(2)
        (anything-mklist '(2)))
      (expect '((lambda ()))
        (anything-mklist (lambda ())))
      (desc "anything-before-initialize-hook")
      (expect 'called
        (let ((anything-before-initialize-hook '((lambda () (setq v 'called))))
              v)
          (anything-initial-setup)
          v))
      (desc "anything-after-initialize-hook")
      (expect '(b a)
        (let ((anything-before-initialize-hook
               '((lambda () (setq v '(a)))))
              (anything-after-initialize-hook
               '((lambda () (setq v (cons 'b v)))))
              v)
          (anything-initial-setup)
          v))
      (expect 0
        (let ((anything-after-initialize-hook
               '((lambda () (setq v (buffer-size (get-buffer anything-buffer))))))
              v)
          (anything-initial-setup)
          v))
      (desc "get-line attribute")
      (expect '(("TEST" ("FOO+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (get-line . (lambda (s e) (upcase (buffer-substring-no-properties s e))))))
         "oo\\+"))
      (desc "with-anything-restore-variables")
      (expect '(7 8)
        (let ((a 7) (b 8)
              (anything-restored-variables '(a b)))
          (with-anything-restore-variables
            (setq a 0 b 0))
          (list a b)))
      (desc "anything-cleanup-hook")
      (expect 'called
        (let ((anything-cleanup-hook
               '((lambda () (setq v 'called))))
              v)
          (anything-cleanup)
          v))
      (desc "with-anything-display-same-window")
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (switch-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (desc "search-from-end attribute")
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))))
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))
         "\\+"))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))
         "\\+"))
      (expect '(("a" ("c2" "c1")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "c1\nc2\n"))))
            (search-from-end)
            (candidates-in-buffer)))))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("c" "b" "a" "")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "a\nb\nc\n"))))
            (search-from-end)
            (candidates-in-buffer)))
         "a*"))
      (desc "header-name attribute")
      (expect "original is transformed"
        (anything-test-update '(((name . "original")
                                 (candidates "1")
                                 (header-name
                                  . (lambda (name)
                                      (format "%s is transformed" name)))))
                              "")
        (with-current-buffer (anything-buffer-get)
          (buffer-string)
          (overlay-get (car (overlays-at (1+(point-min)))) 'display)))
      (desc "volatile and match attribute")
      ;; candidates function is called once per `anything-process-delayed-sources'
      (expect 1
        (let ((v 0))
          (anything-test-candidates '(((name . "test")
                                       (candidates . (lambda () (incf v) '("ok")))
                                       (volatile)
                                       (match identity identity identity)))
                                    "o")
          v))
      (desc "accept-empty attribute")
      (expect nil
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity))))
        (anything-execute-selection-action))
      (expect ""
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity) (accept-empty))))
        (anything-execute-selection-action))
      (desc "anything-tick-hash")
      (expect nil
        (with-current-buffer (get-buffer-create " *00create+*")
          (puthash " *00create+*/xxx" 1 anything-tick-hash)
          (kill-buffer (current-buffer)))
        (gethash " *00create+*/xxx" anything-tick-hash))
      (desc "anything-execute-action-at-once-if-once")
      (expect "HOGE"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test1")
                       (candidates "hoge")
                       (action . upcase))))))
      (expect "ANY"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test2")
                       (candidates "hoge" "any")
                       (action . upcase)))
                    "an")))
      ;; candidates > 1
      (expect (mock (read-string "word: " nil))
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test3")
                       (candidates "hoge" "foo" "bar")
                       (action . identity)))
                    nil "word: ")))
      (desc "anything-quit-if-no-candidate")
      (expect nil
        (let ((anything-quit-if-no-candidate t))
          (anything '(((name . "zero test1") (candidates) (action . upcase))))))
      (expect 'called
        (let (v (anything-quit-if-no-candidate (lambda () (setq v 'called))))
          (anything '(((name . "zero test2") (candidates) (action . upcase))))
          v))
      (desc "real-to-display attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates '(((name . "test")
                                     (candidates "ddd")
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates '(((name . "test")
                                     (candidates ("ignored" . "ddd"))
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("Commands" (("xxxhoge" . "hoge") ("xxxboke" . "boke"))))
        (anything-test-candidates '(((name . "Commands")
                                     (candidates
                                      "hoge" "boke")
                                     (real-to-display . (lambda (x) (concat "xxx" x)))
                                     (action . identity)))
                                  "xxx"))
      (expect "test\nDDD\n"
        (anything-test-update '(((name . "test")
                                 (candidates "ddd")
                                 (real-to-display . upcase)
                                 (action . identity)))
                              "")
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "real-to-display and candidate-transformer attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nDDD\n"
        (anything-test-update
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))
         "")
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "real-to-display and candidates-in-buffer")
      (expect '(("test" (("A" . "a") ("B" . "b"))))
        (anything-test-candidates
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
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
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity))))
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "Symbols are acceptable as candidate.")
      (expect '(("test" (sym "str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))))
      (expect '(("test" ((sym . realsym) ("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))))
      (expect '(("test" (sym)))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "sym"))
      (expect '(("test" ("str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "str"))
      (expect '(("test" ((sym . realsym))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "sym"))
      (expect '(("test" (("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "str"))
      (desc "multiple transformers")
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             . (lambda (cands)
                 (anything-compose (list cands)
                                   (list (lambda (c) (mapcar 'upcase c))
                                         (lambda (c) (list (concat "<" (car c) ">")))))))))))
      (expect '("<FOO>")
        (anything-composed-funcall-with-source
         '((name . "test"))
         (list (lambda (c) (mapcar 'upcase c))
               (lambda (c) (list (concat "<" (car c) ">"))))
         '("foo"))
        )
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             (lambda (c) (mapcar 'upcase c))
             (lambda (c) (list (concat "<" (car c) ">"))))))))
      (expect '(("test" ("<BAR>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "bar")
            (filtered-candidate-transformer
             (lambda (c s) (mapcar 'upcase c))
             (lambda (c s) (list (concat "<" (car c) ">"))))))))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    . (lambda (a s)
                        (anything-compose
                         (list a s)
                         (list (lambda (a s) (push '("view-file" . view-file) a))
                               (lambda (a s) (push '("find-file" . find-file) a))))))))
        (anything-get-action))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    (lambda (a s) (push '("view-file" . view-file) a))
                    (lambda (a s) (push '("find-file" . find-file) a)))))
        (anything-get-action))
      (desc "define-anything-type-attribute")
      (expect '((file (action . find-file)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file)))
        (let ((anything-type-attributes '((file (action . view-file)))))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file))
                (buffer (action . switch-to-buffer)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'buffer '((action . switch-to-buffer)))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (desc "anything-approximate-candidate-number")
      (expect 0
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (anything-approximate-candidate-number))))
      (expect 1
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n")
            (anything-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n"
                    "candiate2\n")
            (<= 2 (anything-approximate-candidate-number)))))
      (expect 1
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    (propertize "multi\nline\n" 'anything-multiline t))
            (anything-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((anything-buffer (current-buffer))
                (anything-candidate-separator "-----"))
            (insert "Title\n"
                    (propertize "multi\nline1\n" 'anything-multiline t)
                    "-----\n"
                    (propertize "multi\nline2\n" 'anything-multiline t))
            (<= 2 (anything-approximate-candidate-number)))))
      (desc "delayed-init attribute")
      (expect 0
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "")
          value))
      (expect 1
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect 2
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init (lambda () (incf value))
                                                     (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect t
        (let (value)
          (with-temp-buffer
            (anything-test-candidates '(((name . "test")
                                         (delayed-init
                                          . (lambda () (setq value
                                                             (eq anything-current-buffer (current-buffer)))))
                                         (candiates "abc")
                                         (requires-pattern . 2)))
                                      "abc")
            value)))
      (desc "pattern-transformer attribute")
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer)))
                                  "xfoo"))
      (desc "anything-recent-push")
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "baz")))
          (anything-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("foo" "bar" "baz")))
          (anything-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "foo" "baz")))
          (anything-recent-push "foo" 'lst)))
      (desc "anything-require-at-least-version")
      (expect nil
        (anything-require-at-least-version "1.1"))
      (expect nil
        (anything-require-at-least-version "1.200"))
      (expect nil
        (anything-require-at-least-version
         (and (string-match "1\.\\([0-9]+\\)" anything-version)
              (match-string 0 anything-version))))
      (expect (error)
        (anything-require-at-least-version "1.999"))
      (expect (error)
        (anything-require-at-least-version "1.2000"))
      (desc "anything-once")
      (expect 2
        (let ((i 0))
          (anything-test-candidates
           '(((name . "1")
              (init . (lambda () (incf i))))
             ((name . "2")
              (init . (lambda () (incf i))))))
          i))
      (expect 1
        (let ((i 0))
          (anything-test-candidates
           '(((name . "1")
              (init . (lambda () (anything-once (lambda () (incf i))))))
             ((name . "2")
              (init . (lambda () (anything-once (lambda () (incf i))))))))
          i))
      (expect 1
        (let ((i 0))
          (flet ((init1 () (anything-once (lambda () (incf i)))))
            (anything-test-candidates
             '(((name . "1")
                (init . init1))
               ((name . "2")
                (init . init1)))))
          i))
      (desc "anything-marked-candidates")
      (expect '("mark3" "mark1")
        (let* ((source '((name . "mark test")))
               (anything-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub anything-buffer-get => (current-buffer))
          (stub anything-get-current-source => source)
          (anything-marked-candidates)))
      (expect '("current")
        (let* ((source '((name . "mark test")))
               (anything-marked-candidates nil))
          (stub anything-get-current-source => source)
          (stub anything-get-selection => "current")
          (anything-marked-candidates)))
      (desc "anything-marked-candidates with coerce")
      (expect '(mark3 mark1)
        (let* ((source '((name . "mark test")
                         (coerce . intern)))
               (anything-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub anything-buffer-get => (current-buffer))
          (stub anything-get-current-source => source)
          (anything-marked-candidates)))
      (desc "anything-let")
      (expect '(1 10000 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            'retval)))
      (desc "anything-let*")
      (expect '(1 2 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval*
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            'retval*)))
      (desc "anything with keyword")
      (expect (mock (anything-internal 'test-source "input" "prompt: " nil "preselect" "*test*" nil))
        (anything :sources   'test-source
                  :input     "input"
                  :prompt    "prompt: "
                  :resume    nil
                  :preselect "preselect"
                  :buffer    "*test*"
                  :keymap    nil))
      (expect (mock (anything-internal 'test-source nil nil nil nil "*test*" nil))
        (anything :sources                'test-source
                  :buffer                 "*test*"
                  :candidate-number-limit 20))
      (expect (mock (anything-internal 'test-source nil nil nil nil "*test*" nil))
        (anything 'test-source nil nil nil nil "*test*" nil))
      (desc "anything-log-eval-internal")
      (expect (mock (anything-log "%S = %S" '(+ 1 2) 3))
        (anything-log-eval-internal '((+ 1 2))))
      (expect (mock (anything-log "%S = ERROR!" 'unDeFined))
        (anything-log-eval-internal '(unDeFined)))

      (desc "anything-output-filter--collect-candidates")
      (expect '("a" "b" "")
        (split-string "a\nb\n" "\n"))
      (expect '("a" "b")
        (anything-output-filter--collect-candidates
         '("a" "b" "") (cons 'incomplete-line  "")))
      (expect '("a" "b")
        (split-string "a\nb" "\n"))
      (expect '("a")
        (anything-output-filter--collect-candidates
         '("a" "b") (cons 'incomplete-line  "")))
      (expect '(incomplete-line . "b")
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (anything-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          incomplete-line-info))
      (expect '("" "c" "")
        (split-string "\nc\n" "\n"))
      (expect '("b" "c")
        ;; "a\nb" + "\nc\n"
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (anything-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          (anything-output-filter--collect-candidates
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
      (desc "anything-next-point-in-list")
      (expect 10
        (anything-next-point-in-list 5 '(10 20) nil))
      (expect 20
        (anything-next-point-in-list 15 '(10 20) nil))
      (expect 25
        (anything-next-point-in-list 25 '(10 20) nil))
      (expect 5
        (anything-next-point-in-list 5 '(10 20) t))
      (expect 10
        (anything-next-point-in-list 15 '(10 20) t))
      (expect 20
        (anything-next-point-in-list 25 '(10 20) t))
      (expect 5
        (anything-next-point-in-list 5 '() nil))
      (expect 5
        (anything-next-point-in-list 5 '() t))
      (expect 10
        (anything-next-point-in-list 5 '(10) nil))
      (expect 10
        (anything-next-point-in-list 15 '(10) t))
      (expect 20
        (anything-next-point-in-list 10 '(10 20) nil))
      (expect 10
        (anything-next-point-in-list 20 '(10 20) t))
      (expect 20
        (anything-next-point-in-list 30 '(10 20 30) t))
      )))
