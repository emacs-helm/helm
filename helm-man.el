;;; helm-man.el --- Man and woman UI

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function Man-getpage-in-background "man.el" (topic))

(defvar helm-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-man-default-action (candidate)
  "Default action for jumping to a woman or man page from helm."
  (let ((wfiles (mapcar 'car (woman-file-name-all-completions candidate))))
    (condition-case err
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
            (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error (kill-buffer) ; Kill woman buffer.
             (Man-getpage-in-background candidate)))))

(defvar helm-source-man-pages
  '((name . "Manual Pages")
    (init . (lambda ()
              (require 'woman)
              (unless helm-man-pages
                (setq helm-man-pages
                      (ignore-errors
                        (woman-file-name "" t)
                        (sort (mapcar 'car woman-topic-all-completions)
                              'string-lessp))))))
    (candidates . helm-man-pages)
    (action  . (("Show with Woman" . helm-man-default-action)))
    ;; Woman does not work OS X
    ;; http://xahlee.org/emacs/modernization_man_page.html
    (action-transformer . (lambda (actions candidate)
                            (if (eq system-type 'darwin)
                                '(("Show with Man" . man))
                                actions)))
    (requires-pattern . 2)))

;;;###autoload
(defun helm-man-woman (arg)
  "Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache."
  (interactive "P")
  (when arg (setq helm-man-pages nil))
  (helm-other-buffer 'helm-source-man-pages "*Helm man woman*"))

(provide 'helm-man)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-man.el ends here
