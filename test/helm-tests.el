;;; helm-tests.el --- Tests for Helm -*- lexical-binding: t -*-

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

;; Tests for helm.

;;; Code:

(when noninteractive
  (load-file (expand-file-name "helm-files-tests.el"
                               (file-name-directory load-file-name)))
  (load-file (expand-file-name "helm-mode-tests.el"
                               (file-name-directory load-file-name)))
  (let ((stats (ert-run-tests-batch "^helm")))
    (when noninteractive
      (kill-emacs (ert-stats-completed-unexpected stats)))))

;;; helm-tests.el ends here
