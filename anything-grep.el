;;; anything-grep.el --- search refinement of grep result with anything
;; $Id: anything-grep.el,v 1.1 2008-10-01 10:58:59 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, unix
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; History:

;; $Log: anything-grep.el,v $
;; Revision 1.1  2008-10-01 10:58:59  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-grep-version "$Id: anything-grep.el,v 1.1 2008-10-01 10:58:59 rubikitch Exp $")
(require 'anything)
(require 'grep)

(defun anything-grep (command pwd)
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer "Run grep (like this): "
				   (if current-prefix-arg
				       default grep-command)
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))
             (read-directory-name "Directory: " default-directory default-directory t)
             ))))
  (let ((name (format "%s [%s]" command pwd)))
    (anything
     `(((name . ,name)
        (init . (lambda ()
                  (anything-candidate-buffer (agrep-create-buffer command pwd))))
        (candidates-in-buffer)
        (action . agrep-goto)
        ;; to inherit faces
        (get-line . buffer-substring))))))


;; (anything-grep "grep -Hin agrep anything-grep.el" default-directory)

(defvar agrep-goto-hook nil)
(defun agrep-goto  (file-line-content)
  (declare (special pwd))
  (string-match ":\\([0-9]+\\):" file-line-content)
  (save-match-data
    (find-file (expand-file-name (substring file-line-content
                                            0 (match-beginning 0))
                                 pwd)))
  (goto-line (string-to-number (match-string 1 file-line-content)))
  (run-hooks 'agrep-goto-hook))

(defvar agrep-font-lock-keywords
  '(;; Highlight grep matches and delete markers
     ("\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
      ;; Refontification does not work after the markers have been
      ;; deleted.  So we use the font-lock-face property here as Font
      ;; Lock does not clear that.
      (2 (list 'face  grep-match-face 'font-lock-face grep-match-face))
      ((lambda (bound))
       (progn
	 ;; Delete markers with `replace-match' because it updates
	 ;; the match-data, whereas `delete-region' would render it obsolete.
	 (replace-match "" t t nil 3)
	 (replace-match "" t t nil 1))))
     ("\\(\033\\[[0-9;]*[mK]\\)"
      ;; Delete all remaining escape sequences
      ((lambda (bound))
       (replace-match "" t t nil 1)))))  

(defun agrep-create-buffer (command pwd)
  (with-current-buffer (get-buffer-create (format " *anything grep:%s*" command))
    (erase-buffer)
    (buffer-disable-undo)
    (font-lock-add-keywords nil agrep-font-lock-keywords 'set)
    (set (make-local-variable 'font-lock-support-mode) nil)
    (set (make-local-variable 'font-lock-maximum-size) nil)

    (let ((process-environment process-environment))
      (when (eq grep-highlight-matches t)
        ;; Modify `process-environment' locally bound in `compilation-start'
        (setenv "GREP_OPTIONS" (concat (getenv "GREP_OPTIONS") " --color=always"))
        ;; for GNU grep 2.5.1
        (setenv "GREP_COLOR" "01;31")
        ;; for GNU grep 2.5.1-cvs
        (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:ml=:cx=:ne"))
      (call-process-shell-command command nil (current-buffer)))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (current-buffer)))
;; (display-buffer (agrep-create-buffer "grep --color=always -Hin agrep anything-grep.el" default-directory))
;; (anything '(((name . "test") (init . (lambda () (anything-candidate-buffer (get-buffer " *anything grep:grep --color=always -Hin agrep anything-grep.el*")) )) (candidates-in-buffer) (get-line . buffer-substring))))

(provide 'anything-grep)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-grep.el")
;;; anything-grep.el ends here
