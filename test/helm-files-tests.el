;;; helm-files-tests.el --- Tests for Helm files -*- lexical-binding: t -*-

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

;; Tests for helm-files.el.

;;; Code:

(require 'ert)
(require 'helm-files)

(ert-deftest helm-files-test-helm-ff--query-replace-in-fname-set-new-name-1 ()
  (should (equal (helm-ff--query-replace-in-fname-set-new-name "foo.txt" "%." "\\@-\\#" 0)
                 "foo-001.txt")))

(ert-deftest helm-files-test-helm-ff--query-replace-in-fname-set-new-name-2 ()
  (should (equal (helm-ff--query-replace-in-fname-set-new-name "foo.txt" "%." "\\#-\\@" 0)
                 "001-foo.txt")))

(ert-deftest helm-files-test-helm-ff--query-replace-in-fname-set-new-name-3 ()
  (should (equal (helm-ff--query-replace-in-fname-set-new-name "foo-bar.txt" "%." "\\@/foo/fiz/" 0)
                 "fiz-bar.txt")))

(ert-deftest helm-files-test-helm-ff--query-replace-in-fname-set-new-name-4 ()
  (should (equal (helm-ff--query-replace-in-fname-set-new-name "foo-bar.txt" "%:4:7" "baz" 0)
                 "foo-baz.txt")))

(ert-deftest helm-files-test-helm-ff--query-replace-in-fname-set-new-name-5 ()
  (should (equal (helm-ff--query-replace-in-fname-set-new-name "foo-bar.txt" "%." "\\@:0:3" 0)
                 "foo.txt")))


;;; helm-files-tests.el ends here
