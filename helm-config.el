;;; helm-config.el --- Applications library for `helm.el' -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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


;;; Require
;;
;;
(require 'easymenu)
(require 'helm-aliases)
(require 'async-bytecomp nil t)


(defgroup helm-config nil
  "Various configurations for Helm."
  :group 'helm)

(defcustom helm-command-prefix-key "C-x c"
  "The key `helm-command-prefix' is bound to in the global map."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (when (and (boundp var) (symbol-value var))
      (define-key (current-global-map)
          (read-kbd-macro (symbol-value var)) nil))
    (when key
      (define-key (current-global-map)
          (read-kbd-macro key) 'helm-command-prefix))
    (set var key)))

(defcustom helm-minibuffer-history-key "C-r"
  "The key `helm-minibuffer-history' is bound to in minibuffer local maps."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (cl-dolist (map '(minibuffer-local-completion-map
                      minibuffer-local-filename-completion-map
                      minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                      minibuffer-local-isearch-map
                      minibuffer-local-map
                      minibuffer-local-must-match-filename-map ; Older Emacsen
                      minibuffer-local-must-match-map
                      minibuffer-local-ns-map))
      (when (and (boundp map) (keymapp (symbol-value map)))
        (when (and (boundp var) (symbol-value var))
          (define-key (symbol-value map)
              (read-kbd-macro (symbol-value var)) nil))
        (when key
          (define-key (symbol-value map)
              (read-kbd-macro key) 'helm-minibuffer-history))))
    (set var key)))

;;; Command Keymap
;;
;;
(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-apropos)
    (define-key map (kbd "e")         'helm-etags-select)
    (define-key map (kbd "l")         'helm-locate)
    (define-key map (kbd "s")         'helm-surfraw)
    (define-key map (kbd "r")         'helm-regexp)
    (define-key map (kbd "m")         'helm-man-woman)
    (define-key map (kbd "t")         'helm-top)
    (define-key map (kbd "/")         'helm-find)
    (define-key map (kbd "i")         'helm-semantic-or-imenu)
    (define-key map (kbd "<tab>")     'helm-lisp-completion-at-point)
    (define-key map (kbd "p")         'helm-list-emacs-process)
    (define-key map (kbd "C-x r b")   'helm-filtered-bookmarks)
    (define-key map (kbd "M-y")       'helm-show-kill-ring)
    (define-key map (kbd "C-c <SPC>") 'helm-all-mark-rings)
    (define-key map (kbd "C-x C-f")   'helm-find-files)
    (define-key map (kbd "f")         'helm-multi-files)
    (define-key map (kbd "C-:")       'helm-eval-expression-with-eldoc)
    (define-key map (kbd "C-,")       'helm-calcul-expression)
    (define-key map (kbd "M-x")       'helm-M-x)
    (define-key map (kbd "M-s o")     'helm-occur)
    (define-key map (kbd "M-g s")     'helm-do-grep)
    (define-key map (kbd "c")         'helm-colors)
    (define-key map (kbd "F")         'helm-select-xfont)
    (define-key map (kbd "8")         'helm-ucs)
    (define-key map (kbd "C-c f")     'helm-recentf)
    (define-key map (kbd "C-c g")     'helm-google-suggest)
    (define-key map (kbd "h i")       'helm-info-at-point)
    (define-key map (kbd "h r")       'helm-info-emacs)
    (define-key map (kbd "h g")       'helm-info-gnus)
    (define-key map (kbd "C-x C-b")   'helm-buffers-list)
    (define-key map (kbd "C-x r i")   'helm-register)
    (define-key map (kbd "C-c C-x")   'helm-run-external-command)
    (define-key map (kbd "b")         'helm-resume)
    map))

;; Don't override the keymap we just defined with an empty
;; keymap.  This also protect bindings changed by the user.
(defvar helm-command-prefix)
(define-prefix-command 'helm-command-prefix)
(fset 'helm-command-prefix helm-command-map)
(setq  helm-command-prefix helm-command-map)


;;; Menu
;;
;;
(easy-menu-add-item
 nil '("Tools")
 '("Helm"
   ["Find any Files/Buffers" helm-multi-files t]
   ["Helm Everywhere (Toggle)" helm-mode t]
   ["Helm resume" helm-resume t]
   "----"
   ("Files"
    ["Find files" helm-find-files t]
    ["Recent Files" helm-recentf t]
    ["Locate" helm-locate t]
    ["Search Files with find" helm-find t]
    ["Bookmarks" helm-filtered-bookmarks t])
   ("Buffers"
    ["Find buffers" helm-buffers-list t])
   ("Commands"
    ["Emacs Commands" helm-M-x t]
    ["Externals Commands" helm-run-external-command t])
   ("Help"
    ["Helm Apropos" helm-apropos t])
   ("Info"
    ["Info at point" helm-info-at-point t]
    ["Emacs Manual index" helm-info-emacs t]
    ["Gnus Manual index" helm-info-gnus t])
   ("Org"
    ["Org keywords" helm-org-keywords t]
    ["Org headlines" helm-org-headlines t])
   ("Tools"
    ["Occur" helm-occur t]
    ["Grep" helm-do-grep t]
    ["Etags" helm-etags-select t]
    ["Lisp complete at point" helm-lisp-completion-at-point t]
    ["Browse Kill ring" helm-show-kill-ring t]
    ["Browse register" helm-register t]
    ["Mark Ring" helm-all-mark-rings t]
    ["Regexp handler" helm-regexp t]
    ["Colors & Faces" helm-colors t]
    ["Show xfonts" helm-select-xfont t]
    ["Ucs Symbols" helm-ucs t]
    ["Imenu" helm-imenu t]
    ["Semantic or Imenu" helm-semantic-or-imenu t]
    ["Google Suggest" helm-google-suggest t]
    ["Eval expression" helm-eval-expression-with-eldoc t]
    ["Calcul expression" helm-calcul-expression t]
    ["Man pages" helm-man-woman t]
    ["Top externals process" helm-top t]
    ["Emacs internals process" helm-list-emacs-process t])
   "----"
   ["Preferred Options" helm-configuration t])
 "Spell Checking")

(easy-menu-add-item nil '("Tools") '("----") "Spell Checking")


;;;###autoload
(defun helm-configuration ()
  "Customize `helm'."
  (interactive)
  (customize-group "helm"))


;;; Fontlock
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(with-helm-after-update-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-temp-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-window\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-quittable\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-current-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-show-completion\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-default-directory\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-display-same-window\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-restore-variables\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(define-helm-type-attribute\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-multi-key-defun\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-while-no-input\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-aif\\)\\>" 1 font-lock-keyword-face))))


;;; Compatibility emacs-24.4+
;; Inlined from Emacs trunk.
(eval-and-compile
  (unless (fboundp 'function-put)
    (defalias 'function-put
      ;; We don't want people to just use `put' because we can't conveniently
      ;; hook into `put' to remap old properties to new ones.  But for now, there's
      ;; no such remapping, so we just call `put'.
      #'(lambda (f prop value) (put f prop value))
      "Set function F's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, F can only be a symbol, not a lambda expression.")))


;;; Load the autoload file
;;  It should have been generated either by
;;  package.el or the make file.

(load "helm-autoloads" nil t)

(provide 'helm-config)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-config.el ends here
