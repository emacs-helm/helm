;;; unit-test-match-plugin.el -- unit-tests for match-plugin.

;; WARNING: Some test here are deprecated, some functions/macros have been removed.

;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "amp-mp-make-regexps")
      (expect '("")
        (amp-mp-make-regexps ""))
      (expect '("foo" "bar")
        (amp-mp-make-regexps "foo bar"))
      (expect '("foo" "bar")
        (amp-mp-make-regexps " foo bar"))
      (expect '("foo" "bar")
        (amp-mp-make-regexps " foo bar "))
      (expect '("foo bar" "baz")
        (let ((helm-mp-space-regexp "\\\\ "))
          (amp-mp-make-regexps "foo\\ bar baz")))
      (desc "helm-mp-3-get-patterns-internal")
      (expect '((identity . "foo"))
        (helm-mp-3-get-patterns-internal "foo"))
      (expect '((identity . "foo") (identity . "bar"))
        (helm-mp-3-get-patterns-internal "foo bar"))
      (expect '((identity . "foo") (not . "bar"))
        (helm-mp-3-get-patterns-internal "foo !bar"))
      (desc "agp-command-line")
      (expect "grep -ih foo /f1"
        (agp-command-line "foo" '("/f1")))
      (expect "grep -ih foo /f1 | grep -ih bar"
        (agp-command-line "foo bar" '("/f1")))
      (expect "grep -ih foo /f1 | grep -ih -v bar"
        (agp-command-line "foo !bar" '("/f1")))
      (expect "grep -ih foo /f1 /f\\ 2 | grep -ih -v bar | grep -ih baz"
        (agp-command-line "foo !bar baz" '("/f1" "/f 2")))
      (expect (concat "grep -ih foo " (expand-file-name "~/.emacs.el"))
        (agp-command-line "foo" '("~/.emacs.el")))
      (expect "grep -ih f\\ o /f\\ 1"
        (agp-command-line "f  o" '("/f 1")))
      (expect "grep -ih foo /f1 | head -n 5"
        (agp-command-line "foo" '("/f1") 5))
      (expect "grep -ih foo /f1 | head -n 5 | nkf -w"
        (agp-command-line "foo" '("/f1") 5 "nkf -w"))
      (expect "cat /f1 | head -n 5 | nkf -w"
        (agp-command-line "" '("/f1") 5 "nkf -w"))
      (expect "tac /f1 | grep -ih foo"
        (agp-command-line "foo" '("/f1") nil nil t))
      (expect "tac /f1 | grep -ih foo | head -n 5 | nkf -w"
        (agp-command-line "foo" '("/f1") 5 "nkf -w" t))
      (expect "tac /f1 | head -n 5 | nkf -w"
        (agp-command-line "" '("/f1") 5 "nkf -w" t))
      (desc "helm-exact-match")
      (expect (non-nil)
        (helm-exact-match "thunder" "thunder"))
      (expect nil
        (helm-exact-match "thunder" "fire"))
      (desc "helm-exact-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-exact-search "thunder" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "\nfire\nthunder\n")
          (goto-char 1)
          (helm-exact-search "fire" nil t)))
      (desc "helm-prefix-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-min))
          (helm-prefix-search "thund" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-min))
          (helm-prefix-search "hund" nil t)))
      (desc "helm-prefix-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-prefix-search-backward "thund" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-prefix-search-backward "hund" nil t)))
      (desc "amp-mp-1-make-regexp")
      (expect "a.*b"
        (amp-mp-1-make-regexp "a b"))
      (expect "a b"
        (let ((helm-mp-space-regexp "\\\\ "))
          (amp-mp-1-make-regexp "a\\ b")))
      (expect "a.*b c"
        (let ((helm-mp-space-regexp "\\\\ "))
          (amp-mp-1-make-regexp "a b\\ c")))
      (expect ""
        (amp-mp-1-make-regexp ""))
      (desc "helm-mp-1-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-1-search "th+ r" nil t)))
      (desc "helm-mp-2-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-2-search "h+ r" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-2-search "th+ r" nil t)))
      (desc "helm-mp-3-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "h+ r" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "th+ r" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "r th+" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "under hue" nil t)))
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "r th+ n" nil t)))
      (desc "helm-mp-3-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "th ders" nil t)))
      (desc "helm-mp-3-search not")
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "h !der" nil t)))
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (helm-mp-3-search "th !der" nil t)))
      (desc "helm-mp-3p-search")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3p-search "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char 1)
          (helm-mp-3p-search "h ders" nil t)))
      (desc "helm-mp-3p-search not")
      (expect t
        (with-temp-buffer
          (insert "\nthreshold\nthunder\n")
          (goto-char 1)
          (helm-mp-3p-search "th !der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char 1)
          (helm-mp-3p-search "h !der" nil t)))
      (desc "helm-mp-3-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3-search-backward "h der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3-search-backward "th ders" nil t)))
      (desc "helm-mp-3-search-backward not")
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3-search-backward "h !der" nil t)))
      (expect t
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3-search-backward "th !der" nil t)))
      (desc "helm-mp-3p-search-backward")
      (expect (non-nil)
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3p-search-backward "th der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "fire\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3p-search-backward "h der" nil t)))
      (desc "helm-mp-3p-search-backward not")
      (expect t
        (with-temp-buffer
          (insert "\nthreshold\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3p-search-backward "th !der" nil t)))
      (expect nil
        (with-temp-buffer
          (insert "threshold\nthunder\n")
          (goto-char (point-max))
          (helm-mp-3p-search-backward "h !der" nil t)))
      (desc "helm-mp-1-match")
      (expect (non-nil)
        (helm-mp-1-match "thunder" "th+ r"))
      (desc "helm-mp-2-match")
      (expect (non-nil)
        (helm-mp-2-match "thunder" "h+ r"))
      (expect nil
        (helm-mp-2-match "thunder" "th+ r"))
      (desc "helm-mp-3-match")
      (expect (non-nil)
        (helm-mp-3-match "thunder" "h+ r"))
      (expect (non-nil)
        (helm-mp-3-match "thunder" "th+ r"))
      (expect (non-nil)
        (helm-mp-3-match "thunder" "r th+"))
      (expect nil
        (helm-mp-3-match "thunder" "under hue"))
      (expect (non-nil)
        (helm-mp-3-match "thunder" "r th+ n"))
      (desc "helm-mp-3-match not")
      (expect (non-nil)
        (helm-mp-3-match "threshold" "th !der"))
      (desc "helm-prefix-match")
      (expect (non-nil)
        (helm-prefix-match "fobar" "fo"))
      (expect nil
        (helm-prefix-match "xfobar" "fo"))

      (desc "helm-mp-3-match")
      (expect (non-nil)
        (helm-mp-3-match "thunder" "h der"))
      (expect nil
        (helm-mp-3-match "thunder" "h ders"))
      (desc "helm-mp-3p-match")
      (expect (non-nil)
        (helm-mp-3p-match "thunder" "th der"))
      (expect nil
        (helm-mp-3p-match "thunder" "h der"))
      (desc "helm-mp-3p-match not")
      (expect (non-nil)
        (helm-mp-3p-match "threshold" "th !der"))
      (expect nil
        (helm-mp-3p-match "threshold" "h !der"))
      (desc "with identity match")
      (expect '(identity)
        (assoc-default 'match
                       (car (helm-compile-sources
                             '(((name . "FOO")
                                (candidates-in-buffer)))
                             '(helm-compile-source--candidates-in-buffer
                               helm-compile-source--match-plugin)))))
      (expect '(identity)
        (assoc-default 'match
                       (car (helm-compile-sources
                             '(((name . "FOO")
                                (match identity)))
                             '(helm-compile-source--match-plugin)))))
      (desc "functional")
      (expect '(("FOO" ("thunder")))
        (helm-test-candidates '(((name . "FOO")
                                     (candidates "fire" "thunder")))
                                  "th r"
                                  '(helm-compile-source--match-plugin)))
      (expect '(("FOO" ("one two")))
        (let ((helm-mp-space-regexp "\\\\ "))
          (helm-test-candidates '(((name . "FOO")
                                       (candidates "one two" "three four")))
                                    "e\\ t"
                                    '(helm-compile-source--match-plugin))))
      (expect '(("FOO" ("one two")))
        (let ((helm-mp-space-regexp "  "))
          (helm-test-candidates '(((name . "FOO")
                                       (candidates "one two" "three four")))
                                    "e  t"
                                    '(helm-compile-source--match-plugin))))
      (expect '(("FOO" ("thunder")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "fire\nthunder\nthanks\n"))))
                                     (candidates-in-buffer)))
                                  "th r"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (helm-test-candidates '(((name . "FOO")
                                     (candidates "foobar" "foo")))
                                  "foo"
                                  '(helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foo")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "foo\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foo")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "bar\nfoo\ntest\n"))))
                                     (candidates-in-buffer)))
                                  "foo"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foobar" "foo")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)))
                                  ""
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("foo" "foobar")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "foobar\nfoo\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "foo"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("elisp" "elp")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "elp\nelisp\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "el p"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      (expect '(("FOO" ("elisp" )))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "elp\nelisp\n"))))
                                     (candidates-in-buffer)
                                     (search-from-end)))
                                  "el+ isp"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      ;; prefix multi -> multi
      (expect '(("FOO" ("elisp-info" "info.el")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "info.el\nelisp-info\n"))))
                                     (candidates-in-buffer)
                                     ))
                                  "el info"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      ;; multi not
      (expect '(("FOO" ("info.el")))
        (helm-test-candidates '(((name . "FOO")
                                     (init
                                      . (lambda ()
                                          (with-current-buffer (helm-candidate-buffer 'global)
                                            (insert "info.el\nelisp-info\n"))))
                                     (candidates-in-buffer)
                                     ))
                                  "info !elisp"
                                  '(helm-compile-source--candidates-in-buffer
                                    helm-compile-source--match-plugin)))
      ;; helm-mp-match-source-name
      (expect '(("SourceName" ("foo")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName")
                                       (candidates "foo" "bar")))
                                    "source f"
                                    '(helm-compile-source--match-plugin))))
      (expect '(("SourceName cib" ("foo")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName cib")
                                       (init
                                        . (lambda ()
                                            (with-current-buffer (helm-candidate-buffer 'global)
                                              (insert "foo\nbar\n"))))
                                       (candidates-in-buffer)))
                                    "source f"
                                    '(helm-compile-source--candidates-in-buffer
                                      helm-compile-source--match-plugin))))
      (expect '(("SourceName cib search-from-end" ("bar")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName cib search-from-end")
                                       (init
                                        . (lambda ()
                                            (with-current-buffer (helm-candidate-buffer 'global)
                                              (insert "foo\nbar\n"))))
                                       (search-from-end)
                                       (candidates-in-buffer)))
                                    "source b"
                                    '(helm-compile-source--candidates-in-buffer
                                      helm-compile-source--match-plugin))))
      (expect '(("SourceName" ("foo" "bar")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName")
                                       (candidates "foo" "bar")))
                                    "source ."
                                    '(helm-compile-source--match-plugin))))
      (expect '(("SourceName cib" ("foo" "bar")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName cib")
                                       (init
                                        . (lambda ()
                                            (with-current-buffer (helm-candidate-buffer 'global)
                                              (insert "foo\nbar\n"))))
                                       (candidates-in-buffer)))
                                    "source ."
                                    '(helm-compile-source--candidates-in-buffer
                                      helm-compile-source--match-plugin))))
      (expect '(("SourceName cib search-from-end" ("bar" "foo")))
        (let ((helm-mp-match-source-name t))
          (helm-test-candidates '(((name . "SourceName cib search-from-end")
                                       (init
                                        . (lambda ()
                                            (with-current-buffer (helm-candidate-buffer 'global)
                                              (insert "foo\nbar\n"))))
                                       (search-from-end)
                                       (candidates-in-buffer)))
                                    "source ."
                                    '(helm-compile-source--candidates-in-buffer
                                      helm-compile-source--match-plugin))))
      )))
;; (helm-compile-sources '(((name . "test"))) helm-compile-source-functions)
