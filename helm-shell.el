;;; helm-shell.el --- Shell prompt navigation for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Pierre Neidhardt <mail@ambrevar.xyz>

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
;;
;; This is superseded by helm-comint.el.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp)
(require 'helm-comint)

;;;###autoload
(defalias 'helm-shell-prompts 'helm-comint-prompts)

;;;###autoload
(defalias 'helm-shell-prompts-all 'helm-comint-prompts-all)

(provide 'helm-shell)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-shell ends here
