;;; anything-config.el --- predefined configurations for anything

;; Copyright (C) 2007  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; This package provides predefined configurations for anything.el
;; You can pick the ones you like and use in your own configuration
;; like this:
;;
;;   (require 'anything-config)  ; loads anything.el too
;;
;;   (setq anything-sources (list anything-source-emacs-commands
;;                                anything-source-locate ...)
;;
;;   (push anything-actions-buffer anything-type-actions)
;;;
;;   ...
;;

(require 'anything)

;;----------------------------------------------------------------------
;;
;; Predefined Sources
;;
;;----------------------------------------------------------------------

(defvar anything-source-emacs-commands
  `((name . "Emacs Commands") 
    (candidates . ,(let (commands) 
                     (mapatoms (lambda (a) 
                                 (if (commandp a) 
                                     (push (symbol-name a) 
                                           commands)))) 
                     (sort commands 'string-lessp))) 
    (action . (lambda (command-name) 
                (call-interactively (intern command-name)))))
  "Source for completing and invoking Emacs commands.")


(defvar anything-source-locate 
  '((name . "Locate")        
    (candidates . (lambda ()
                    (start-process "locate-process" nil
                                   "locate" "-i" "-r"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3))
  "Source for retrieving files matching the current input pattern
  with locate.")


(defvar anything-source-tracker-search 
  '((name . "Tracker Search") 
    (candidates . (lambda () 
                    (start-process "tracker-search-process" nil 
                                   "tracker-search" 
                                   anything-pattern))) 
    (type . file) 
    (requires-pattern . 3)) 
  "Source for retrieving files matching the current input pattern 
with the tracker desktop search.")


;;----------------------------------------------------------------------
;;
;; Predefined Type Actions
;;
;;----------------------------------------------------------------------

(defvar anything-actions-buffer
  '(buffer . (("Switch to Buffer" . switch-to-buffer)
              ("Pop to Buffer"    . pop-to-buffer)
              ("Display Buffer"   . display-buffer)
              ("Kill Buffer"      . kill-buffer)))
  "Actions for type `buffer'.")


(provide 'anything-config.el)
;;; anything-config.el.el ends here
