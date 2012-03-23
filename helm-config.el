;;; helm-config.el --- Applications library for `helm.el'

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'cl)
(require 'helm)
(require 'helm-utils)
(require 'helm-plugin)
(require 'helm-buffers)
(require 'helm-net)
(require 'helm-external)
(require 'helm-files)
(require 'helm-locate)
(require 'helm-elisp)
(require 'helm-eshell)
(require 'helm-regexp)
(require 'helm-grep)
(require 'helm-mode)
(require 'helm-ring)
(require 'helm-command)
(require 'helm-eval)
(require 'helm-tags)
(require 'helm-adaptative)
(require 'helm-imenu)
(require 'helm-bookmark)
(require 'helm-org)
(require 'helm-info)
(require 'helm-man)
(require 'helm-color)
(require 'helm-font)
(require 'helm-sys)
(require 'helm-misc)
(require 'helm-help)
(require 'helm-match-plugin)
(require 'helm-elscreen nil t)
(require 'helm-w3m nil t)
(require 'helm-firefox nil t)
(require 'helm-bmkext nil t)
(require 'helm-apt nil t)
(require 'helm-gentoo nil t)
(require 'helm-bbdb nil t)
(require 'helm-emms nil t)
(require 'yaoddmuse nil t)
(require 'helm-call-tree nil t)
(require 'helm-descbinds nil t)
(require 'helm-slime nil t)


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


;;; Helm-command-map
;;
;;
(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-c-apropos)
    (define-key map (kbd "e")         'helm-c-etags-select)
    (define-key map (kbd "l")         'helm-locate)
    (define-key map (kbd "s")         'helm-surfraw)
    (define-key map (kbd "r")         'helm-regexp)
    (define-key map (kbd "w")         'helm-w3m-bookmarks)
    (define-key map (kbd "x")         'helm-firefox-bookmarks)
    (define-key map (kbd "#")         'helm-emms)
    (define-key map (kbd "m")         'helm-man-woman)
    (define-key map (kbd "t")         'helm-top)
    (define-key map (kbd "i")         'helm-imenu)
    (define-key map (kbd "<tab>")     'helm-lisp-completion-at-point)
    (define-key map (kbd "p")         'helm-list-emacs-process)
    (define-key map (kbd "C-x r b")   'helm-c-pp-bookmarks)
    (define-key map (kbd "M-y")       'helm-show-kill-ring)
    (define-key map (kbd "C-c <SPC>") 'helm-all-mark-rings)
    (define-key map (kbd "C-x C-f")   'helm-find-files)
    (define-key map (kbd "f")         'helm-for-files)
    (define-key map (kbd "C-:")       'helm-eval-expression-with-eldoc)
    (define-key map (kbd "C-,")       'helm-calcul-expression)
    (define-key map (kbd "M-x")       'helm-M-x)
    (define-key map (kbd "C-x C-w")   'helm-write-file)
    (define-key map (kbd "C-x i")     'helm-insert-file)
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
    (define-key map (kbd "C-c C-b")   'helm-browse-code)
    (define-key map (kbd "C-x r i")   'helm-register)
    (define-key map (kbd "C-c C-x")   'helm-c-run-external-command)
    map))

;; Don't override the keymap we just defined with an empty
;; keymap.  This also protect bindings changed by the user.
(defvar helm-command-prefix)
;;;###autoload
(define-prefix-command 'helm-command-prefix)
(fset 'helm-command-prefix helm-command-map)
(setq  helm-command-prefix helm-command-map)


;;; Menu
;;
;;
(easy-menu-add-item nil '("Tools") 
 '("Helm"
  ["Find any Files/Buffers" helm-for-files t]
  ["Helm Everywhere (Toggle)" helm-mode t]
  "----"
  ("Files"
   ["Find files" helm-find-files t]
   ["Recent Files" helm-recentf t]
   ["Locate" helm-locate t]
   ["Bookmarks" helm-c-pp-bookmarks t])
  ("Buffers"
   ["Find buffers" helm-buffers-list t])
  ("Commands"
   ["Emacs Commands" helm-M-x t]
   ["Externals Commands" helm-c-run-external-command t])
  ("Help"
   ["Helm Apropos" helm-c-apropos t])
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
   ["Etags" helm-c-etags-select t]
   ["Lisp complete at point" helm-lisp-completion-at-point t]
   ["Browse Kill ring" helm-show-kill-ring t]
   ["Browse register" helm-register t]
   ["Browse code" helm-browse-code t]
   ["Mark Ring" helm-all-mark-rings t]
   ["Regexp handler" helm-regexp t]
   ["Colors & Faces" helm-colors t]
   ["Show xfonts" helm-select-xfont t]
   ["Ucs Symbols" helm-ucs t]
   ["Imenu" helm-imenu t]
   ["Google Suggest" helm-google-suggest t]
   ["Eval expression" helm-eval-expression-with-eldoc t]
   ["Calcul expression" helm-calcul-expression t]
   ["Man pages" helm-man-woman t]
   ["Top externals process" helm-top t]
   ["Emacs internals process" helm-list-emacs-process t])
  "----"
  ["Prefered Options" helm-configuration t])
 "Spell Checking")

(easy-menu-add-item nil '("Tools") '("----") "Spell Checking")

;;;###autoload
(defun helm-configuration ()
  "Customize `helm'."
  (interactive)
  (customize-group "helm"))


;;; Helm map add ons
;;
;;
(define-key helm-map (kbd "C-x C-f") 'helm-quit-and-find-file)
(define-key helm-map (kbd "M-m")     'helm-toggle-all-marks)
(define-key helm-map (kbd "C-w")     'helm-yank-text-at-point)


;;; Type Attributes
;;
;;
(define-helm-type-attribute 'buffer
    `((action
       ("Switch to buffer" . helm-c-switch-to-buffer)
       ,(and (locate-library "popwin") '("Switch to buffer in popup window" . popwin:popup-buffer))
       ("Switch to buffer other window" . switch-to-buffer-other-window)
       ("Switch to buffer other frame" . switch-to-buffer-other-frame)
       ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . helm-find-buffer-on-elscreen))
       ("Query replace regexp" . helm-c-buffer-query-replace-regexp)
       ("Query replace" . helm-c-buffer-query-replace)
       ("View buffer" . view-buffer)
       ("Display buffer"   . display-buffer)
       ("Grep buffers (C-u grep all buffers)" . helm-c-zgrep-buffers)
       ("Revert buffer(s)" . helm-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . helm-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . helm-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (helm-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (candidate-transformer helm-c-skip-boring-buffers
                             helm-c-transform-buffer-display-string))
  "Buffer or buffer name.")

(define-helm-type-attribute 'file
    `((action
       ("Find file" . helm-find-many-files)
       ,(and (locate-library "popwin") '("Find file in popup window" . popwin:find-file))
       ("Find file as root" . helm-find-file-as-root)
       ("Find file other window" . find-file-other-window)
       ("Find file other frame" . find-file-other-frame)
       ("Open dired in file's directory" . helm-c-open-dired)
       ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
       ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
       ("Pdfgrep File(s)" . helm-ff-pdfgrep)
       ("Checksum File" . helm-ff-checksum)
       ("Ediff File" . helm-find-files-ediff-files)
       ("Ediff Merge File" . helm-find-files-ediff-merge-files)
       ("View file" . view-file)
       ("Insert file" . insert-file)
       ("Delete file(s)" . helm-delete-marked-files)
       ("Open file externally (C-u to choose)" . helm-c-open-file-externally)
       ("Open file with default tool" . helm-c-open-file-with-default-tool)
       ("Find file in hex dump" . hexl-find-file))
      (persistent-help . "Show this file")
      (action-transformer helm-c-transform-file-load-el
                          helm-c-transform-file-browse-url)
      (candidate-transformer helm-c-w32-pathname-transformer
                             helm-c-skip-current-file
                             helm-c-skip-boring-files
                             helm-c-shorten-home-path))
  "File name.")

(let ((actions '(("Describe command" . describe-function)
                 ("Add command to kill ring" . helm-c-kill-new)
                 ("Go to command's definition" . find-function)
                 ("Debug on entry" . debug-on-entry)
                 ("Cancel debug on entry" . cancel-debug-on-entry)
                 ("Trace function" . trace-function)
                 ("Trace function (background)" . trace-function-background)
                 ("Untrace function" . untrace-function))))
  (define-helm-type-attribute 'command
      `((action ("Call interactively" . helm-c-call-interactively)
                ,@actions)
        (coerce . helm-c-symbolify)
        (persistent-action . describe-function))
    "Command. (string or symbol)")

  (define-helm-type-attribute 'function
      `((action . ,actions)
        (action-transformer helm-c-transform-function-call-interactively)
        (candidate-transformer helm-c-mark-interactive-functions)
        (coerce . helm-c-symbolify))
    "Function. (string or symbol)"))

(define-helm-type-attribute 'variable
    '((action ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . helm-c-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . helm-c-set-variable))
      (coerce . helm-c-symbolify))
  "Variable.")

(define-helm-type-attribute 'bookmark
    `((coerce . helm-bookmark-get-bookmark-from-name)
      (action
       ("Jump to bookmark" . helm-c-bookmark-jump)
       ("Jump to BM other window" . bookmark-jump-other-window)
       ("Bookmark edit annotation" . bookmark-edit-annotation)
       ("Bookmark show annotation" . bookmark-show-annotation)
       ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
       ,@(and (locate-library "bookmark-extensions")
              `(("Edit Bookmark" . bmkext-edit-bookmark)))
       ("Rename bookmark" . bookmark-rename)
       ("Relocate bookmark" . bookmark-relocate))
      (keymap . ,helm-c-bookmark-map)
      (mode-line . helm-bookmark-mode-line-string))
  "Bookmark name.")

(define-helm-type-attribute 'line
    '((display-to-real . helm-c-display-to-real-line)
      (action ("Go to Line" . helm-c-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'file-line
    `((filtered-candidate-transformer helm-c-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . helm-c-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'timer
    '((real-to-display . helm-c-timer-real-to-display)
      (action ("Cancel Timer" . cancel-timer)
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


(provide 'helm-config)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-config.el ends here
