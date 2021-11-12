;;; helm-config.el --- Applications library for `helm.el' -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2021 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
;; Requiring this file is not needed when using a package manager to
;; install helm as this one will take care of loading the autoload
;; file.

;;; Code:

;;; Load the autoload file
;;  It should have been generated either by
;;  the package manager or the make file.

(load "helm-autoloads" nil t)

(provide 'helm-config)

;;; helm-config.el ends here
