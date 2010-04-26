;;;; anything-multi-sources.el --- concatenate candidates in multi souces

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, experimental
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-multi-sources.el

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
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-multi-sources RET
;;

;;; Code:

(eval-when-compile (require 'cl))
(defgroup anything-multi-sources nil
  "anything-multi-sources"
  :group 'emacs)

(defvar anything-multi-source-delimiter-candidates
  '("anything-multi-source-delimiter-candidates--3e3r3x903oj09fnreiojioxe"))
(defvar anything-multi-source-delimiter-name
  "anything-multi-source-delimiter-name--339024u3fz09e90390ir309330923")
(defvar anything-multi-source-delimiter
  `((name . ,anything-multi-source-delimiter-name)
    (init . anything-multi-source-delimiter-init)
    (match identity)
    (disable-shortcuts)
    (volatile)
    (candidates . anything-multi-source-delimiter-candidates)))

(defun anything-multi-source-delimiter-init ()
  (setq anything-source-in-each-line-flag t))

(defun ams-delete-delimiter-source ()
  (delete-region (point) (progn (forward-line 3)  (point))))
(defun anything-multi-sources-update-function ()
  (setq cursor-type t)
  (let ((pos 1)
        (push-line
         (lambda ()
           (push (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                 concat-headers)))
        (first-source
         (lambda ()
           (some (lambda (source)
                   (if (equal (assoc-default 'name source)
                              (buffer-substring (point-at-bol) (point-at-eol)))
                       source))
                 (anything-get-sources))))
        (rewrite-first-source-name
         (lambda ()
           (overlay-put (make-overlay (line-beginning-position)
                                      (line-end-position))
                        'display (mapconcat 'identity
                                            ;;omit separator
                                            (reverse (cdr concat-headers)) 
                                            " + "))))
        in-multi-source delimiter-flag concat-header-pos concat-headers
        multi-source-start multi-source-end)
    (while (or (eq pos 1)
               (progn (forward-line 1)
                      (setq pos (anything-get-next-header-pos))))
      (goto-char pos)
      (setq pos 0)                      ;reset
      (funcall push-line)
      (setq delimiter-flag
            (equal (car concat-headers) anything-multi-source-delimiter-name))
      (when (and (not delimiter-flag)   ;delete header
                 in-multi-source)
        (delete-region (1- (point)) (progn (forward-line 1) (point))))
      (when delimiter-flag
        (cond ((not in-multi-source)
               (setq concat-header-pos (point))
               (setq concat-headers nil)
               (save-excursion
                 (forward-line 1) ; anything-get-next-header-pos workaround
                 (ignore-errors
                   (goto-char (anything-get-next-header-pos))
                   (funcall push-line)))
               (forward-line 0)
               (ams-delete-delimiter-source)
               (if (equal (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                          anything-multi-source-delimiter-name)
                   (ams-delete-delimiter-source)
                 (save-excursion (forward-line 1) (setq multi-source-start (point)))
                 (setq in-multi-source (not in-multi-source))))
              (t
               (save-excursion
                 (forward-line -1) (setq multi-source-end (point))
                 (goto-char (1- multi-source-start))
                 (ams-sort-candidates
                  multi-source-start multi-source-end (funcall first-source)))
               (ams-delete-delimiter-source)
               (save-excursion
                 (goto-char concat-header-pos)
                 (funcall rewrite-first-source-name))
               (setq in-multi-source (not in-multi-source))))))))

(defun ams-sort-candidates (s e source)
  (sort-numeric-fields 1 s e))

(add-hook 'anything-update-hook 'anything-multi-sources-update-function)
(provide 'anything-multi-sources)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-multi-sources.el")
;;; anything-multi-sources.el ends here
