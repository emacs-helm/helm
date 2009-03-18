;;; anything-gtags.el --- GNU GLOBAL anything.el interface
;; $Id: anything-gtags.el,v 1.15 2009-03-18 17:31:39 rubikitch Exp $

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-gtags-select'
;;    Tag jump using gtags and `anything'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; History:

;; $Log: anything-gtags.el,v $
;; Revision 1.15  2009-03-18 17:31:39  rubikitch
;; Apply SUGAWARA's patch to suppress filename output when `anything-gtags-classify' is non-nil.
;;
;; Revision 1.14  2009/01/27 09:51:34  rubikitch
;; * Push context when jumping with `anything-gtags-select'.
;; * New variable: `anything-gtags-enable-initial-pattern'.
;;
;; Revision 1.13  2008/12/20 22:11:04  rubikitch
;; Fixed an error in Emacs23 by Andy Stewart. Thanks.
;;
;; Revision 1.12  2008/10/24 07:14:14  rubikitch
;; use `ad-get-arg'
;;
;; Revision 1.11  2008/09/06 06:01:07  rubikitch
;; Classify candidates by file name using meta source.
;; If `anything-gtags-classify' is non-nil, classification is enabled.
;;
;; Revision 1.10  2008/08/24 20:45:07  rubikitch
;; silence byte compiler
;;
;; Revision 1.9  2008/08/24 08:22:48  rubikitch
;; Rename `anything-candidates-buffer' -> `anything-candidate-buffer'
;;
;; Revision 1.8  2008/08/23 23:01:53  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2008/08/20 19:00:36  rubikitch
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

(require 'anything)
(require 'gtags)

(defvar anything-gtags-enable-initial-pattern nil
  "If non-nil, initial input of `anything-gtags-select' is current symbol.")

(defvar anything-c-source-gtags-select
  '((name . "GTAGS")
    (init
     . (lambda ()
         (call-process-shell-command
          "global -c" nil (anything-candidate-buffer 'global))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate ""))))))
;; (setq anything-sources (list anything-c-source-gtags-select))

(defun anything-gtags-select ()
  "Tag jump using gtags and `anything'."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything '(anything-c-source-gtags-select)
              (if anything-gtags-enable-initial-pattern initial-pattern)
              "Find Tag: " nil)))

;;;; `gtags-select-mode' replacement
(defvar anything-gtags-hijack-gtags-select-mode t
  "Use `anything' instead of `gtags-select-mode'.")
(defvar anything-gtags-classify nil)
(defvar aggs-base-source
  '((candidates-in-buffer)
    (get-line . aggs-candidate-display)
    (display-to-real
     . (lambda (c) (if (string-match "^ " c) (concat "_ " c) c)))
    (action
     ("Goto the location"
      . (lambda (c) (aggs-select-it c t))))
    (persistent-action . aggs-select-it)
    (cleanup . (lambda () (kill-buffer buffer)))))

(defun aggs-candidate-display (s e)
  ;; 16 = length of symbol
  (buffer-substring-no-properties (+ s 16) e))
(defun aggs-set-anything-current-position ()
  ;; It's needed because `anything' saves
  ;; *GTAGS SELECT* buffer's position,
  (save-window-excursion
    (switch-to-buffer save)
    (setq anything-current-position (cons (point) (window-start)))))

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
                     (line-number-at-pos))))
         (sources (if anything-gtags-classify
                      '(((name . "GTAGS SELECT meta source")
                         (init . aggs-meta-source-init)))
                    `(((name . "GTAGS SELECT")
                       (init
                        . (lambda ()
                            (aggs-set-anything-current-position)
                            (anything-candidate-buffer buffer)))
                       ,@aggs-base-source)))))
    (anything
     sources
     nil nil nil (format "\\(\\(%d\\) +%s\\)" lineno (regexp-quote basename) ))))

(defun aggs-candidate-buffer-by-filename (filename)
  (get-buffer-create (concat "*anything gtags*" filename)))
(defun aggs-meta-source-init ()
  (aggs-set-anything-current-position)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (files prev-filename)
      (loop 
       while (re-search-forward " [0-9]+ \\([^ ]+\\) " (point-at-eol) t)
       for filename = (match-string 1)
       for bol = (point-at-bol)
       for eol = (point-at-eol)
       do
       (with-current-buffer (aggs-candidate-buffer-by-filename filename)
         (unless (equal prev-filename filename)
           (setq files (cons filename files))
           (erase-buffer))
	 (let ((pos (point)))
	   (insert-buffer-substring buffer bol eol)
	   (goto-char pos)
	   (while (re-search-forward filename nil t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   (goto-char (point-max)))
	 (insert "\n"))
       (forward-line 1)
       (setq prev-filename filename))
      (anything-set-sources
       (loop for file in (nreverse files) collect
             (append `((name . ,file)
                       (init . (lambda ()
                                 (anything-candidate-buffer
                                  ,(aggs-candidate-buffer-by-filename file)))))
                     aggs-base-source)))
      (anything-funcall-foreach 'init))))
           

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
                         (if (bufferp (ad-get-arg 0))
                             (buffer-name (ad-get-arg 0))
                               (or (ad-get-arg 0) ""))))
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
