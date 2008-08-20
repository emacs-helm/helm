;;; anything-gtags.el --- GNU GLOBAL anything.el interface
;; $Id: anything-gtags.el,v 1.7 2008-08-20 19:00:36 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: global, languages
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-gtags.el

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

;; * `anything-gtags-select' is `anything' interface of `gtags-find-tag'.
;; * `anything-c-source-gtags-select' is a source for `gtags-find-tag'.
;; * Replace *GTAGS SELECT* buffer with `anything' interface.

;;; History:

;; $Log: anything-gtags.el,v $
;; Revision 1.7  2008-08-20 19:00:36  rubikitch
;; *** empty log message ***
;;
;; Revision 1.6  2008/08/20 18:58:42  rubikitch
;; preselect entry of current line of source code.
;;
;; Revision 1.5  2008/08/19 21:50:00  rubikitch
;; adjust to new `search' spec.
;;
;; Revision 1.4  2008/08/18 17:20:23  rubikitch
;; save c source buffer's position
;; silence byte compiler
;;
;; Revision 1.3  2008/08/16 10:26:56  rubikitch
;; adjust to argument change of `anything-candidates-in-buffer-1'
;;
;; Revision 1.2  2008/08/14 20:47:14  rubikitch
;; ag-hijack-gtags-select-mode: cleanup
;;
;; Revision 1.1  2008/08/13 14:17:41  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-c-source-gtags-select
  '((name . "GTAGS")
    (init
     . (lambda ()
         (call-process-shell-command
          "global -c" nil (anything-candidates-buffer 'global))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-goto-tag candidate ""))))))
;; (setq anything-sources (list anything-c-source-gtags-select))

(defun anything-gtags-select ()
  "Tag jump using gtags and `anything'."
  (interactive)
  (anything '(anything-c-source-gtags-select) nil "Find Tag: "))

;;;; `gtags-select-mode' replacement
(defvar anything-gtags-hijack-gtags-select-mode t
    "Use `anything' instead of `gtags-select-mode'.")
(defun aggs-candidate-display (s e)
  ;; 16 = length of symbol
  (buffer-substring-no-properties (+ s 16) e))
(defun ag-hijack-gtags-select-mode ()
  ;; `save': C source file / `buffer': gtags-select-mode buffer
  ;; They are defined at `gtags-goto-tag'.
  (declare (special save buffer))
  (let* ((anything-candidate-number-limit 9999)
         (pwd (with-current-buffer buffer (expand-file-name default-directory)))
         (basename (substring (with-current-buffer save buffer-file-name)
                              (length pwd)))
         (lineno (with-current-buffer save
                   (save-restriction
                     (widen)
                     (line-number-at-pos)))))
    (anything
     '(((name . "GTAGS SELECT")
        (init
         . (lambda ()
             ;; It's needed because `anything' saves
             ;; *GTAGS SELECT* buffer's position,
             (save-window-excursion
               (switch-to-buffer save)
               (setq anything-current-position (cons (point) (window-start))))
             (anything-candidates-buffer buffer)))
        (candidates-in-buffer
         . (lambda ()
             (anything-candidates-in-buffer
              #'aggs-candidate-display)))
        (display-to-real
         . (lambda (c) (if (string-match "^ " c) (concat "_ " c) c)))
        (action
         ("Goto the location"
          . (lambda (c) (aggs-select-it c t))))
        (persistent-action . aggs-select-it)
        (cleanup . (lambda () (kill-buffer buffer)))))
     nil nil nil (format "\\(\\(%d\\) +%s\\)" lineno (regexp-quote basename) ))))

(defun aggs-select-it (candidate &optional delete)
  (with-temp-buffer
    (declare (special pwd buffer))
    ;; `pwd' is defined at `ag-hijack-gtags-select-mode'.
    (setq default-directory pwd)
    (insert candidate "\n")
    (forward-line -1)
    (gtags-select-it nil)
    ;; `buffer' is defined at `gtags-goto-tag'.
    (and delete (kill-buffer buffer))))


(defadvice switch-to-buffer (around anything-gtags activate)
  "Use `anything' instead of `gtags-select-mode' when `anything-gtags-hijack-gtags-select-mode' is non-nil."
  (unless (and anything-gtags-hijack-gtags-select-mode
           (string-match "*GTAGS SELECT*"
                         (if (bufferp buffer) (buffer-name buffer) buffer)))
    ad-do-it))
;; (progn (ad-disable-advice 'switch-to-buffer 'around 'anything-gtags) (ad-update 'switch-to-buffer)) 

(defadvice gtags-select-mode (around anything-gtags activate)
  "Use `anything' instead of `gtags-select-mode' when `anything-gtags-hijack-gtags-select-mode' is non-nil."
  (if anything-gtags-hijack-gtags-select-mode
      (ag-hijack-gtags-select-mode)
    ad-do-it))
;; (progn (ad-disable-advice 'gtags-select-mode 'around 'anything-gtags) (ad-update 'gtags-select-mode)) 

(provide 'anything-gtags)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-gtags.el")
;;; anything-gtags.el ends here
