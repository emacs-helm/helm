;;; helm-call-source.el --- helm interface for selecting helm sources.

;; Filename: helm-call-source.el
;; Description: helm interface for selecting helm sources.
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com> 
;; Copyright (C) 2013 rubikitch <rubikitch@ruby-lang.org>
;; Version: 1.0
;; Last-Updated: 2013-08-12 03:00:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/helm/blob/master/helm-call-source.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((helm "20130806.1505"))
;;
;; Features that might be required by this library:
;;
;; helm
;;

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
;;  ===========
;;
;; After invoking M-x helm-call-source you can select some helm sources and press RET to start
;; a new helm session with those sources.
;;
;;; Installation:
;;  =============
;;
;; Add a require statement for the library to your init file (~/.emacs): (require 'helm-call-source)

;;; Code:

(require 'helm)

(defvar helm-call-source-buffer "*helm source select*")

(defvar helm-c-source-call-source
  `((name . "Call helm source")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda ()
         (loop for vname in (append (all-completions "helm-source-" obarray)
                                    (all-completions "helm-c-source-" obarray))
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect (cons (format "%s (%s)" name vname) var))))
    (action . (("Invoke helm with selected sources" .
                (lambda (candidate)
                  (helm :sources (helm-marked-candidates) :buffer helm-call-source-buffer)))
               ("Describe variable" . describe-variable)))
    (persistent-action . describe-variable)))

(defun helm-call-source ()
  "Call helm source."
  (interactive)
  (helm 'helm-c-source-call-source nil nil nil nil
        helm-call-source-buffer))
