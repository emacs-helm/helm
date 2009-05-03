;;; anything-show-completion.el --- Show selection in buffer for anything completion
;; $Id: anything-show-completion.el,v 1.9 2009-05-03 22:01:32 rubikitch Exp $

;; Copyright (C) 2009  hchbaw
;; Copyright (C) 2009  rubikitch

;; Original Author: hchbaw
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, convenience, complete
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-show-completion.el

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
;; `anything' is also useful for in-buffer completion such as
;; `anything-lisp-complete-symbol'. But users must see *anything*
;; buffer when completing. It forces us to move our eyes away from
;; code temporarily and we feel stressful.
;;
;; With this plug-in, current selection (`anything-get-selection') is
;; displayed at point. This plug-in is automatically detected by user
;; program such as anything-complete.el .
;;
;; This program is based on an idea by hchbaw.
;;   http://d.hatena.ne.jp/hchbaw/20090416/1239878984

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-show-completion-face'
;;    *Face of anything-show-completion.
;;    default = anything-selection-face
;;  `anything-show-completion-activate'
;;    *Set nil to turn off anything-show-completion.
;;    default = t

;;; For developers:
;;
;; To enable anything-show-completion for user-defined function, use
;; `use-anything-show-completion'. It accepts function and length of
;; prefix (= current completing target) as a sexp. It must be used
;; with soft-require.
;;
;; Example:
;;   (when (require 'anything-show-completion nil t)
;;     (use-anything-show-completion 'rct-complete-symbol--anything
;;                                   '(length pattern)))
;;
;; Example in souce code:
;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-complete.el
;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-rcodetools.el

;;; Installation:
;;
;; Put anything-show-completion.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-show-completion)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-show-completion RET
;;


;;; History:

;; $Log: anything-show-completion.el,v $
;; Revision 1.9  2009-05-03 22:01:32  rubikitch
;; asc-display-function: split-window hack is effective only if one window is displayed.
;;
;; Revision 1.8  2009/05/03 21:47:41  rubikitch
;; set `anything-display-function'
;;
;; Revision 1.7  2009/04/20 12:21:28  rubikitch
;; Fixed an error when `anything' is invoked for the first time.
;;
;; Revision 1.6  2009/04/18 16:11:33  rubikitch
;; Removed a mess
;;
;; Revision 1.5  2009/04/18 16:11:01  rubikitch
;; * Fixed a typo.
;; * New function: `anything-show-completion-install'
;;
;; Revision 1.4  2009/04/18 10:05:15  rubikitch
;; copyright
;;
;; Revision 1.3  2009/04/18 10:02:10  rubikitch
;; doc
;;
;; Revision 1.2  2009/04/18 09:21:14  rubikitch
;; `use-anything-show-completion' as a function.
;; It enables us to affect multiple commands with `dolist'.
;;
;; Revision 1.1  2009/04/17 17:07:35  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-show-completion-version "$Id: anything-show-completion.el,v 1.9 2009-05-03 22:01:32 rubikitch Exp $")
(require 'anything)
(defgroup anything-show-completion nil
  "anything-show-completion"
  :group 'anything)

(defvar asc-overlay nil)
(defcustom anything-show-completion-face anything-selection-face
  "*Face of anything-show-completion."
  :type 'face  
  :group 'anything-show-completion)
(defcustom anything-show-completion-activate t
  "*Set nil to turn off anything-show-completion."
  :type 'boolean  
  :group 'anything-show-completion)

(defun asc-initialize-maybe ()
  (unless asc-overlay
    (setq asc-overlay (make-overlay (point-min) (point-min)))
    (overlay-put asc-overlay 'face anything-show-completion-face)
    (asc-cleanup)))

(defun asc-cleanup ()
  (delete-overlay asc-overlay))
(add-hook 'anything-cleanup-hook 'asc-cleanup)

(asc-initialize-maybe)

(defun asc-overlay-activate-p ()
  "Return non-nil if `anything' is being used for any completionic purposes."
  (and anything-show-completion-activate (overlay-buffer asc-overlay)))

(defadvice anything-mark-current-line (after anything-show-completion activate)
  "Display the `anything-get-selection' contents as an overlay at the
current (point)."
  (anything-aif (and (asc-overlay-activate-p)
                     (with-anything-window
                       (not (equal (anything-buffer-get) anything-action-buffer)))
                     (anything-get-selection))
      (with-current-buffer anything-current-buffer
        (asc-display-overlay it))))

(defun asc-display-overlay (selection)
  (overlay-put asc-overlay 'display selection)
  (move-overlay asc-overlay
                (- (point) (eval (overlay-get asc-overlay 'prefix-length-sexp)))
                (point)
                anything-current-buffer))

;;; Entry point
(defun use-anything-show-completion (function prefix-length-sexp)
  "Setup a before advice of FUNCTION to show the `anything-get-selection' contents as an overlay at point.

PREFIX-LENGTH-SEXP is an expression to denote the length of prefix (completing target).
It is evaluated in `asc-display-overlay'."
  (eval `(defadvice ,function (around anything-show-completion activate)
           (anything-show-completion-install ',prefix-length-sexp)
           (let ((anything-display-function 'asc-display-function)) ad-do-it))))

(defun anything-show-completion-install (prefix-length-sexp)
  (asc-initialize-maybe)
  (move-overlay asc-overlay (point) (point) (current-buffer))
  (overlay-put asc-overlay 'prefix-length-sexp prefix-length-sexp))



(defun asc-point-at-upper-half-of-window-p (pt)
  (<= (+ (count-screen-lines (window-start) pt)
        (if header-line-format 1 0)
        (if (zerop (current-column)) 0 0))
      (- (/ (window-height) 2)
         (if header-line-format 1 0))))
 
;; (global-set-key "\C-x\C-z" (lambda () (interactive) (message "%s" (asc-point-at-upper-half-of-window-p (point)))))

(defun asc-display-function (buf)
  (if (not (one-window-p))
      (pop-to-buffer buf)
    (let* ((cursor-upper-p (asc-point-at-upper-half-of-window-p (point))) 
           (half (/ (window-height) 2))
           (new-w (save-excursion
                    (let (split-window-keep-point)
                      (split-window-vertically)))))
      (switch-to-buffer-other-window buf))))

(provide 'anything-show-completion)
;; (asc-display-function anything-buffer)
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-show-completion.el")
;;; anything-show-completion.el ends here
