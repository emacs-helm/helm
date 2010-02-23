;;;; anything-menu.el --- menu command using anything interface
;; $Id: anything-menu.el,v 1.1 2010-02-23 09:44:09 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: menu, tools, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-menu.el

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

;;; Installation:
;;
;; Put anything-menu.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-menu)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-menu RET
;;


;;; History:

;; $Log: anything-menu.el,v $
;; Revision 1.1  2010-02-23 09:44:09  rubikitch
;; initial
;;

;;; Code:

(defvar anything-menu-version "$Id: anything-menu.el,v 1.1 2010-02-23 09:44:09 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup anything-menu nil
  "anything-menu"
  :group 'emacs)

(provide 'anything-menu)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-menu.el")
;;; anything-menu.el ends here
