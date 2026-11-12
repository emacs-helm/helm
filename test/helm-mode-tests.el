;;; helm-mode-tests.el --- Tests for Helm mode -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2026 Thierry Volpiatto
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for helm-mode.el.

;;; Code:

(require 'ert)
(require 'helm-mode)

(defvar crm-separator)

(ert-deftest helm-mode-test-completion-in-region-should-complete-after-text-before-input-is-deleted ()
  (with-temp-buffer
    (insert "prefix\nfoo")
    ;; Point is at the end of "foo", so there is no suffix to preserve or mark.
    (let ((helm-completion-mark-suffix nil))
      ;; Simulate deleting the preceding line while completion is open,
      ;; then selecting "foobar".
      (cl-letf (((symbol-function 'helm-comp-read)
                 (lambda (&rest _args)
                   (save-excursion
                     (goto-char (point-min))
                     (delete-region (point) (line-beginning-position 2)))
                   "foobar")))
        (helm--completion-in-region #'ignore (copy-marker 8) (point) '("foobar"))
        (should (equal (buffer-string) "foobar"))))))

(ert-deftest helm-mode-test-completion-in-region-should-preserve-suffix-after-text-before-input-is-inserted ()
  (with-temp-buffer
    (insert "prefix\nfoo")
    (goto-char 10)
    ;; Point is inside "foo" (fo|o), so the trailing "o" must be preserved
    ;; and marked after completion.
    (let ((helm-completion-mark-suffix t)
          (mark-suffix nil))
      ;; Simulate inserting a line before the input, then selecting "foobar".
      ;; Capture the suffix-marking callback to run it after completion.
      (cl-letf (((symbol-function 'helm-comp-read)
                 (lambda (&rest _args)
                   (save-excursion
                     (goto-char (point-min))
                     (insert "prefix\n"))
                   "foobar"))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_seconds _repeat function &rest args)
                   (setq mark-suffix (lambda () (apply function args))))))
        (helm--completion-in-region #'ignore 8 (point-max) '("foobar"))
        (should (equal (buffer-string) "prefix\nprefix\nfoobaro"))
        (funcall mark-suffix)
        (should (equal (buffer-substring (point) (mark)) "o"))))))

(ert-deftest helm-mode-test-completion-in-region-cleans-up-after-exit-function-error ()
  (with-temp-buffer
    (let ((helm-before-action-hook nil)
          (helm-completion-mark-suffix nil)
          (completion-extra-properties '(:exit-function user-error)))
      (cl-letf (((symbol-function 'helm-comp-read)
                 (lambda (&rest _args) "foo"))
                ((symbol-function 'lisp--local-variables) #'ignore))
        (should-error (helm--completion-in-region #'ignore 1 1 '("foo"))
                      :type 'user-error)
        (should-not helm-before-action-hook)))))

(ert-deftest helm-mode-test-crm-regexp-separator-is-not-inserted ()
  (with-temp-buffer
    (insert "Range: ")
    (let ((crm-separator "\\.\\.\\.\\?")
          (helm-crm-default-separator nil))
      (helm-completion-in-region--insert-result
       (list "some-string")
       1 (point) (point) 0)
      (should (equal (buffer-string) "some-string")))))

(ert-deftest helm-mode-test-crm-text-property-separator-is-inserted ()
  (with-temp-buffer
    (insert "Range: ")
    (let ((crm-separator (propertize "\\.\\.\\.\\?" 'separator "..."))
          (helm-crm-default-separator nil))
      (helm-completion-in-region--insert-result
       (list "some-string")
       1 (point) (point) 0)
      (should (equal (buffer-string) "some-string...")))))

(ert-deftest helm-mode-test-crm-regexp-derived-separator-is-inserted ()
  (with-temp-buffer
    (insert "Prompt ")
    (let ((crm-separator "[ \t]*:[ \t]*")
          (helm-crm-default-separator ","))
      (helm-completion-in-region--insert-result
       (list "my-branch")
       1 (point) (point) 0)
      (should (equal (buffer-string) "my-branch:")))))

;;; helm-mode-tests.el ends here
