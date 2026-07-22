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
