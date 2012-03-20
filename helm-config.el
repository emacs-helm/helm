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
(require 'helm-vars)
(require 'helm-utils)
(require 'helm-buffers)
(require 'helm-net)
(require 'helm-files)
(require 'helm-elisp)
(require 'helm-grep)
(require 'helm-mode)
(require 'helm-ring)
(require 'helm-command)
(require 'helm-eval)
(eval-when-compile (require 'org)) ; Shut up byte compiler about org-directory.
(eval-when-compile (require 'semantic nil t))
(require 'helm-match-plugin)



;;; Declare external functions
;;
;;
(declare-function gnus-dired-attach "ext:gnus-dired.el" (files-to-attach))
(declare-function image-dired-display-image "image-dired.el" (file &optional original-size))
(declare-function image-dired-update-property "image-dired.el" (prop value))
(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function Man-getpage-in-background "man.el" (topic))
(declare-function simple-call-tree-analyze "ext:simple-call-tree.el" (&optional test))
(declare-function yaoddmuse-update-pagename "ext:yaoddmuse.el" (&optional unforced))
(declare-function yaoddmuse-get-library-list "ext:yaoddmuse.el" (&optional dirs string))
(declare-function org-get-current-options "ext:org-exp.el")
(declare-function emms-streams "ext:emms-streams")
(declare-function emms-stream-delete-bookmark "ext:emms-streams")
(declare-function emms-stream-add-bookmark "ext:emms-streams" (name url fd type))
(declare-function emms-stream-save-bookmarks-file "ext:emms-streams")
(declare-function emms-stream-quit "ext:emms-streams")
(declare-function with-current-emms-playlist "ext:emms" (&rest body))
(declare-function emms-playlist-tracks-in-region "ext:emms" (beg end))
(declare-function emms-playlist-first "ext:emms")
(declare-function emms-playlist-mode-play-smart "ext:emms-playlist-mode")
(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")
(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))
(declare-function elscreen-find-screen-by-buffer "ext:elscreen.el" (buffer &optional create))
(declare-function elscreen-find-file "ext:elscreen.el" (filename))
(declare-function elscreen-goto "ext:elscreen.el" (screen))
(declare-function semantic-format-tag-summarize "ext:format.el" (tag &optional parent color) t)
(declare-function semantic-tag-components "ext:tag.el" (tag) t)
(declare-function semantic-go-to-tag "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-type "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-class "ext:tag-file.el" (tag) t)
(declare-function bbdb "ext:bbdb-com")
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-redisplay-one-record "ext:bbdb-com")
(declare-function bbdb-record-net "ext:bbdb-com" (string) t)
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-dwim-net-address "ext:bbdb-com")
(declare-function bbdb-records "ext:bbdb-com"
                  (&optional dont-check-disk already-in-db-buffer))
(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eldoc-current-symbol "eldoc")
(declare-function eldoc-get-fnsym-args-string "eldoc" (sym &optional index))
(declare-function eldoc-get-var-docstring "eldoc" (sym))
(declare-function eldoc-fnsym-in-current-sexp "eldoc")
(declare-function find-library-name "find-func.el" (library))
(declare-function secure-hash "ext:fns.c" (algorithm object &optional start end binary))
(declare-function w32-shell-execute "ext:w32fns.c" (operation document &optional parameters show-flag))
(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))



;;; General internal variables
;;
;; Some internals variable that need to be loaded
;; here to avoid compiler warnings.
(defvar helm-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")


;;; Helm-command-map
;;
;;
(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<SPC>")     'helm-execute-helm-command)
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
(easy-menu-define nil global-map
  "`helm' menu"
  '("Helm"
    ["All helm commands" helm-execute-helm-command t]
    ["Find any Files/Buffers" helm-for-files t]
    ["Helm Everywhere (Toggle)" helm-mode t]
    "----"
    ("Files:"
     ["Find files" helm-find-files t]
     ["Recent Files" helm-recentf t]
     ["Locate" helm-locate t]
     ["Bookmarks" helm-c-pp-bookmarks t])
    ("Buffers:"
     ["Find buffers" helm-buffers-list t])
    ("Commands:"
     ["Emacs Commands" helm-M-x t]
     ["Externals Commands" helm-c-run-external-command t])
    ("Help:"
     ["Helm Apropos" helm-c-apropos t])
    ("Info:"
     ["Info at point" helm-info-at-point t]
     ["Emacs Manual index" helm-info-emacs t]
     ["Gnus Manual index" helm-info-gnus t])
    ("Org:"
     ["Org keywords" helm-org-keywords t]
     ["Org headlines" helm-org-headlines t])
    ("Tools:"
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
    ["Prefered Options" helm-configuration t]))

;;; Helm map add ons
;;
;;
(define-key helm-map (kbd "C-x C-f") 'helm-quit-and-find-file)
(define-key helm-map (kbd "M-m")     'helm-toggle-all-marks)
(define-key helm-map (kbd "C-w")     'helm-yank-text-at-point)


;;; Specialized keymaps
;;
;;
(defvar helm-c-etags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-etags-help)
    map)
  "Keymap used in Etags.")

(defvar helm-c-ucs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-backspace>") 'helm-c-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'helm-c-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'helm-c-ucs-persistent-forward)
    (define-key map (kbd "<C-return>")    'helm-c-ucs-persistent-insert)
    (define-key map (kbd "C-c ?")         'helm-c-ucs-help)
    map)
  "Keymap for `helm-ucs'.")

(defvar helm-c-bookmark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-c-bookmark-run-jump-other-window)
    (define-key map (kbd "C-d")   'helm-c-bookmark-run-delete)
    (when (locate-library "bookmark-extensions")
      (define-key map (kbd "M-e") 'helm-c-bmkext-run-edit))
    (define-key map (kbd "C-c ?") 'helm-c-bookmark-help)
    (delq nil map))
  "Generic Keymap for emacs bookmark sources.")

(defvar helm-esh-on-file-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")    'helm-esh-help)
    map)
  "Keymap for `helm-find-files-eshell-command-on-file'.")

(defvar helm-eshell-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") 'helm-next-line)
    map)
  "Keymap for `helm-eshell-history'.")


;;; Embeded documentation.
;;
;;
(defun helm-c-list-preconfigured-helm ()
  "Collect preconfigured helm functions in this file."
  (loop with doc
        with sym
        for entry in (cdr (assoc
                           (file-truename (locate-library "helm-config"))
                           load-history))
        if (and (consp entry)
                (eq (car entry) 'defun)
                (string-match "^Preconfigured.+$"
                              (setq doc (or (documentation (setq sym (cdr entry)))
                                            ""))))
        collect (cons sym (match-string 0 doc))))

(defun helm-c-format-preconfigured-helm ()
  (mapcar (lambda (x) (format "\\[%s] : %s\n" (car x) (cdr x)))
          (helm-c-list-preconfigured-helm)))

;;; Global help message - Used by `helm-help'
;;
;;
(setq helm-help-message
      (lambda ()
        (concat
         "\\<helm-map>"
         "`helm' is QuickSilver-like candidate-selection framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

If you have any problems, press C-c C-x C-b!!
Feel free to send bug reports. I'll fix them.
The steps are described in the beginning of helm.el file.

== Basic Operations ==
C-p, Up: Previous Line
C-n, Down : Next Line
M-v, PageUp : Previous Page
C-v, PageDown : Next Page
Enter : Execute first (default) action / Select
M-< : First Line
M-> : Last Line
M-PageUp, C-M-S-v, C-M-y : Previous Page (other-window)
M-PageDown, C-M-v : Next Page (other-window)

Tab, C-i : Show action list
Left : Previous Source
Right, C-o : Next Source
C-k : Delete pattern
C-z : Persistent Action (Execute action with helm session kept)
C-c C-x C-b: Send a bug report

== Shortcuts For 2nd/3rd Action ==
\\[helm-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[helm-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[helm-toggle-visible-mark] : Toggle Visible Mark
\\[helm-prev-visible-mark] : Previous Mark
\\[helm-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[helm-toggle-resplit-window] : Toggle vertical/horizontal split helm window
\\[helm-quit-and-find-file] : Drop into `find-file'
\\[helm-delete-current-selection] : Delete Selected Item (visually)
\\[helm-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[helm-yank-selection] : Yank Selected Item Into Pattern
\\[helm-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[helm-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[helm-resume] revives last `helm' session.
It is very useful, so you should bind any key.

Single source is executed by \\[helm-call-source].

== Preconfigured `helm' ==
Preconfigured `helm' is commands that uses `helm' interface.
You can use them without configuration.

"
         (apply 'concat (helm-c-format-preconfigured-helm))
         "
Enjoy!")))

;;; `helm-buffer-list' help
;;
;;
(defvar helm-c-buffer-help-message
  "== Helm Buffer ==
\nTips:
You can enter a partial name of major-mode (e.g lisp, sh) to narrow down buffers.
Enter then a space and a pattern to narrow down to buffers matching this pattern.
\nSpecific commands for `helm-buffers-list':
\\<helm-c-buffer-map>
\\[helm-buffer-run-zgrep]\t\t->Grep Buffer(s) works as zgrep too. (C-u grep all buffers but non--file buffers).
\\[helm-buffer-switch-other-window]\t\t->Switch other window.
\\[helm-buffer-switch-other-frame]\t\t->Switch other frame.
\\[helm-buffer-run-query-replace-regexp]\t\t->Query replace regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\t->Query replace in marked buffers.
\\[helm-buffer-switch-to-elscreen]\t\t->Find buffer in Elscreen.
\\[helm-buffer-run-ediff]\t\t->Ediff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\t->Ediff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[helm-buffer-diff-persistent]\t\t->Toggle Diff buffer with saved file without quitting.
\\[helm-buffer-revert-persistent]\t\t->Revert buffer without quitting.
\\[helm-buffer-save-persistent]\t\t->Save buffer without quitting.
\\[helm-buffer-run-kill-buffers]\t\t->Delete marked buffers and quit.
\\[helm-toggle-all-marks]\t\t->Toggle all marks.
\\[helm-mark-all]\t\t->Mark all.
\\[helm-c-buffer-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-c-buffer-help ()
  "Help command for helm buffers."
  (interactive)
  (let ((helm-help-message helm-c-buffer-help-message))
    (helm-help)))

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "== Helm Find Files ==
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.
- Enter `/' at end of pattern to quickly reach root of your file system.
- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
- You can complete with partial basename \(e.g \"fb\" will complete \"foobar\"\).
- Use `C-u C-z' to watch an image.
- To browse images directories turn on `helm-follow-mode' and navigate with arrow keys.
- When entered ediff, hitting `C-g' will ask you to use locate to find the file to ediff with.

\nSpecific commands for `helm-find-files':
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\t->Run Locate on basename of candidate (C-u to specify locate db).
\\[helm-ff-run-grep]\t\t->Run Grep (C-u Recursive).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\t->Run zgrep (C-u Recursive).
\\[helm-ff-run-etags]\t\t->Run Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[helm-ff-run-rename-file]\t\t->Rename File (C-u Follow).
\\[helm-ff-run-copy-file]\t\t->Copy File (C-u Follow).
\\[helm-ff-run-byte-compile-file]\t\t->Byte Compile File (C-u Load).
\\[helm-ff-run-load-file]\t\t->Load File.
\\[helm-ff-run-symlink-file]\t\t->Symlink File.
\\[helm-ff-run-hardlink-file]\t\t->Hardlink file.
\\[helm-ff-run-delete-file]\t\t->Delete File.
\\[helm-ff-run-kill-buffer-persistent]\t\t->Kill buffer candidate without quitting.
\\[helm-ff-persistent-delete]\t\t->Delete file without quitting.
\\[helm-ff-run-switch-to-eshell]\t\t->Switch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\t->Eshell command on file (C-u Run on all marked files at once).
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\t->Complete file name at point.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[helm-ff-rotate-left-persistent]\t\t->Rotate Image Left.
\\[helm-ff-rotate-right-persistent]\t\t->Rotate Image Right.
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-switch-to-history]\t\t->Switch to helm find-files history.
\\[helm-ff-properties-persistent]\t\t->Show file properties in a tooltip.
\\[helm-mark-all]\t\t->Mark all visibles candidates.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-unmark-all]\t\t->Unmark all candidates, visibles and invisibles.
\\[helm-ff-run-gnus-attach-files]\t\t->Gnus attach files to message buffer.
\\[helm-ff-run-print-file]\t\t->Print file, (C-u to refresh printers list).
\\[helm-enlarge-window]\t\t->Enlarge helm window.
\\[helm-narrow-window]\t\t->Narrow helm window.
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename/fullpath.
\\[helm-send-bug-report-from-helm]\t\t->Send Bug report.
\\[helm-ff-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-ff-help ()
  "Help command for `helm-find-files'."
  (interactive)
  (let ((helm-help-message helm-ff-help-message))
    (helm-help)))

;;; Help for `helm-c-read-file-name'
;;
;;
(defvar helm-read-file-name-help-message
  "== Helm read file name Map ==\
\nSpecific commands for helm-c-read-file-name:
\\<helm-c-read-file-map>
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-next-source]\t->Goto next source.
\\[helm-previous-source]\t->Goto previous source.
\\[helm-read-file-name-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-read-file-name-help ()
  (interactive)
  (let ((helm-help-message helm-read-file-name-help-message))
    (helm-help)))

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "== Helm Generic files Map ==\
\nSpecific commands for helm locate and others files sources:
\\<helm-generic-files-map>
\\[helm-ff-run-grep]\t\t->Run grep (C-u recurse).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-delete-file]\t\t->Delete file.
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-properties-persistent]\t\t->Show file properties.
\\[helm-yank-text-at-point]\t\t->Yank text at point.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\nLocate tips:
You can add after writing search pattern any of the locate command line options.
e.g -b, -e, -n <number>...etc.
See Man locate for more infos.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-generic-file-help ()
  (interactive)
  (let ((helm-help-message helm-generic-file-help-message))
    (helm-help)))

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "== Helm Grep Map ==\
\nHelm Grep tips:
You can start grep with a prefix arg to recurse in subdirectories.
You can use wild card when selecting files (e.g *.el)
You can grep in many differents directories by marking files or wild cards.
You can save your results in a grep-mode buffer, see below.

\nSpecific commands for Helm Grep:
\\<helm-c-grep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-c-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-c-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-c-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-c-grep-run-save-buffer]\t\t->Save to a `grep-mode' enabled buffer.
\\[helm-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-grep-help ()
  (interactive)
  (let ((helm-help-message helm-grep-help-message))
    (helm-help)))

;;; Pdf grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "== Helm PdfGrep Map ==\
\nSpecific commands for Pdf Grep:
\\<helm-c-pdfgrep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-pdfgrep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-pdfgrep-help ()
  (interactive)
  (let ((helm-help-message helm-pdfgrep-help-message))
    (helm-help)))

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "== Helm Etags Map ==\
\nSpecific commands for Etags:
\\<helm-c-etags-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-etags-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-etags-help ()
  "The help function for etags."
  (interactive)
  (let ((helm-help-message helm-etags-help-message))
    (helm-help)))

;;; Ucs help
;;
;;
(defvar helm-c-ucs-help-message
  "== Helm Ucs ==
\nSpecific commands for `helm-ucs':
\\<helm-c-ucs-map>
\\[helm-c-ucs-persistent-insert]\t->Insert char.
\\[helm-c-ucs-persistent-forward]\t->Forward char.
\\[helm-c-ucs-persistent-backward]\t->Backward char.
\\[helm-c-ucs-persistent-delete]\t->Delete char backward.
\\[helm-c-ucs-help]\t\t->Show this help.

\n== Helm Map ==
\\{helm-map}")

(defun helm-c-ucs-help ()
  "Help command for `helm-ucs'."
  (interactive)
  (let ((helm-help-message helm-c-ucs-help-message))
    (helm-help)))

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "== Helm bookmark name Map ==\
\nSpecific commands for bookmarks:
\\<helm-c-bookmark-map>
\\[helm-c-bookmark-run-jump-other-window]\t\t->Jump other window.
\\[helm-c-bookmark-run-delete]\t\t->Delete bookmark.
\\[helm-c-bmkext-run-edit]\t\t->Edit bookmark (only for bmkext).
\\[helm-c-bookmark-help]\t\t->Run this help.
\n== Helm Map ==
\\{helm-map}")

(defun helm-c-bookmark-help ()
  "Help command for bookmarks."
  (interactive)
  (let ((helm-help-message helm-bookmark-help-message))
    (helm-help)))

;;; Eshell command on file help
;;
;;
(defvar helm-c-esh-help-message
  "== Helm eshell on file ==
\nTips:

- Passing extra args after filename:

Normally your command or alias will be called with file as argument.

e.g <command> 'candidate_file'

But you can also pass an argument or more after 'candidate_file' like this:

<command> %s [extra_args]\n

'candidate_file' will be inserted at '%s' and your command will look at this:

<command> 'candidate_file' [args]

- Specify many files as args (marked files):

e.g <command> file1 file2 ...

Please restart and use a prefix arg to call `helm-find-files-eshell-command-on-file'.
Otherwise your command will be called many times like this:

<command> file1 <command> file2 etc...

\nSpecific commands for `helm-find-files-eshell-command-on-file':
\\<helm-esh-on-file-map>
\\[helm-esh-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

(defun helm-esh-help ()
  "Help command for `helm-find-files-eshell-command-on-file'."
  (interactive)
  (let ((helm-help-message helm-c-esh-help-message))
    (helm-help)))


;;; Mode line strings
;;
;;
(defvar helm-buffer-mode-line-string
  '("Buffer(s)"
    "\\<helm-c-buffer-map>\
\\[helm-c-buffer-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string
  "\\<helm-find-files-map>\
\\[helm-ff-help]:Help, \
\\[helm-send-bug-report-from-helm]:BugReport, \
\\<helm-map>\
\\[helm-select-action]:Acts, \
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string
  "\\<helm-c-read-file-map>\
\\[helm-read-file-name-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string
  "\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string
  "\\<helm-c-grep-map>\
\\[helm-grep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string
  "\\<helm-c-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string
  "\\<helm-c-etags-map>\
\\[helm-etags-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-c-etags-select'.")


(defvar helm-c-ucs-mode-line-string
  "\\<helm-c-ucs-map>\
\\[helm-c-ucs-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct."
  "String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)"
    "\\<helm-c-bookmark-map>\
\\[helm-c-bookmark-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-occur-mode-line
  "\\<helm-map>\
\\[helm-help]:Help,\
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport.")


;;; Toggle all marks.
;;
;;
;;;###autoload
(defun helm-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (with-helm-window
    (save-excursion
      (goto-char (helm-get-previous-header-pos))
      (helm-next-line)
      (let* ((next-head (helm-get-next-header-pos))
             (end       (and next-head
                             (save-excursion
                               (goto-char next-head)
                               (forward-line -1)
                               (point))))
             (maxpoint  (or end (point-max))))
        (while (< (point) maxpoint)
          (helm-mark-current-line)
          (let* ((prefix (get-text-property (point-at-bol) 'display))
                 (cand   (helm-get-selection))
                 (bn     (and (helm-file-completion-source-p)
                              (helm-c-basename cand)))
                 (src    (assoc-default 'name (helm-get-current-source))))
            (when (and (not (helm-this-visible-mark))
                       (not (or (string= prefix "[?]")
                                (string= prefix "[@]"))))
              ;; Don't mark possibles directories ending with . or ..
              ;; autosave files/links and non--existent file.
              (unless
                  (and (or (helm-file-completion-source-p)
                           (equal src "Files from Current Directory"))
                       (or (string-match "^\\.#.*\\|^#.*#$\\|\\.$" bn)
                           ;; We need to test here when not using a transformer
                           ;; that tag prefix (i.e on tramp)
                           (not (file-exists-p cand))))
                (helm-make-visible-mark))))
          (forward-line 1) (end-of-line))))
    (helm-mark-current-line)
    (message "%s candidates marked" (length helm-marked-candidates))))

;;;###autoload
(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-window
    (let ((len (length helm-marked-candidates)))
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (message "%s candidates unmarked" len))))

;;;###autoload
(defun helm-toggle-all-marks ()
  "Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session"
  (interactive)
  (let ((marked (helm-marked-candidates)))
    (if (and (>= (length marked) 1)
             (with-helm-window helm-visible-mark-overlays))
        (helm-unmark-all)
        (helm-mark-all))))


;;;; <File>
;;
;;
;;; File name history
(defvar helm-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match helm-c-match-on-basename)
    (type . file)))

;;; Files in current dir
;;
;;
(defvar helm-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (directory-files (helm-c-current-directory)))))
    ;; volatile is not needed, I think.
    (type . file)))

(defun helm-c-highlight-files (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize (file-name-nondirectory i)
                            'face 'helm-ff-directory
                            'help-echo (expand-file-name i))
        else
        collect (propertize (file-name-nondirectory i)
                            'face 'helm-ff-file
                            'help-echo (expand-file-name i))))

(defvar helm-c-source-files-in-current-dir+
  `((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (directory-files (helm-c-current-directory) t))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (candidate-transformer helm-c-highlight-files)
    ;; volatile is not needed, I think.
    (type . file)))


;;; File Cache
(defvar helm-c-file-cache-initialized-p nil)

(defvar helm-c-file-cache-files nil)

(defvar helm-c-source-file-cache
  `((name . "File Cache")
    (init
     . (lambda ()
         (require 'filecache nil t)
         (unless helm-c-file-cache-initialized-p
           (setq helm-c-file-cache-files
                 (loop for item in file-cache-alist append
                       (destructuring-bind (base &rest dirs) item
                         (loop for dir in dirs collect
                               (concat dir base)))))
           (defadvice file-cache-add-file (after file-cache-list activate)
             (add-to-list 'helm-c-file-cache-files (expand-file-name file)))
           (setq helm-c-file-cache-initialized-p t))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (candidates . helm-c-file-cache-files)
    (match helm-c-match-on-basename)
    (type . file)))


;;; Locate
;;
;;
;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

(defun helm-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `helm-ff-default-directory' or
fall back to `default-directory' if FROM-FF is nil."
  (when helm-ff-locate-db-filename
    (cond ((and helm-ff-default-directory
                from-ff
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                helm-ff-default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 helm-ff-default-directory)))
          ((and (not from-ff)
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 default-directory))))))

(defun helm-locate-1 (&optional localdb init from-ff)
  "Generic function to run Locate.
if LOCALDB is non--nil search and use a local locate db file.
INIT is a string to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (helm-locate-with-db
   (and localdb
        (or (helm-ff-find-locatedb from-ff)
            (helm-c-read-file-name
             "LocateDBFiles: "
             :initial-input (or helm-ff-default-directory
                                default-directory)
             :marked-candidates t
             :preselect helm-locate-db-file-regexp
             :test #'(lambda (x)
                       (if helm-locate-db-file-regexp
                           ;; Select only locate db files and directories
                           ;; to allow navigation.
                           (or (string-match
                                helm-locate-db-file-regexp x)
                               (file-directory-p x))
                           x)))))
   init))
;; (helm-locate-1 t)

(defun helm-locate-with-db (&optional db initial-input)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (when (and db (stringp db)) (setq db (list db)))
  (unless helm-c-locate-command
    (setq helm-c-locate-command
          (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es -i -r %s")
            (t "locate %s"))))
  (let ((helm-c-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format "locate -d %s"
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (loop for i in db
                                       unless (file-directory-p i)
                                       collect i) ":"))
              helm-c-locate-command)
             helm-c-locate-command)))
    (helm :sources 'helm-c-source-locate
          :buffer "*helm locate*"
          :input initial-input
          :keymap helm-generic-files-map)))
;; (helm-locate-with-db "~/locate.db")

(defun helm-c-locate-init ()
  "Initialize async locate process for `helm-c-source-locate'."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (line-number-mode "%l") " "
          (:eval (propertize "(Locate Process Running) "
                  'face '((:foreground "red"))))))
  (prog1
      (start-process-shell-command "locate-process" nil
                                   (format helm-c-locate-command
                                           helm-pattern))
    (set-process-sentinel (get-process "locate-process")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (with-helm-window
                                  (force-mode-line-update nil)
                                  (helm-update-move-first-line)))))))

(defvar helm-c-source-locate
  `((name . "Locate")
    (candidates . helm-c-locate-init)
    (type . file)
    (requires-pattern . 3)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . helm-generic-file-mode-line-string)
    (delayed))
  "Find files matching the current input pattern with locate.")

(defun helm-c-locate-read-file-name (prompt &optional init)
  "Search a file with locate and return it's filename.
Use argument PROMPT and INIT for `helm' arguments
prompt and input."
  (helm :sources
        '((name . "Locate")
          (candidates . helm-c-locate-init)
          (action . identity)
          (requires-pattern . 3)
          (candidate-number-limit . 9999)
          (mode-line . helm-generic-file-mode-line-string)
          (delayed))
        :prompt prompt
        :input init
        :buffer "*helm locate rfn*"))


;; Yank text at point.
;;
;;
;; Internal
(defvar helm-yank-point nil)

;;;###autoload
(defun helm-yank-text-at-point ()
  "Yank text at point in minibuffer."
  (interactive)
  (let (input)
    (flet ((insert-in-minibuffer (word)
             (with-selected-window (minibuffer-window)
               (let ((str helm-pattern))
                 (delete-minibuffer-contents)
                 (set-text-properties 0 (length word) nil word)
                 (insert (concat str word))))))
      (with-helm-current-buffer
        ;; Start to initial point if C-w have never been hit.
        (unless helm-yank-point (setq helm-yank-point (point)))
        (and helm-yank-point (goto-char helm-yank-point))
        (forward-word 1)
        (setq input (buffer-substring-no-properties helm-yank-point (point)))
        (setq helm-yank-point (point))) ; End of last forward-word
      (insert-in-minibuffer input))))

(defun helm-reset-yank-point ()
  (setq helm-yank-point nil))

(add-hook 'helm-after-persistent-action-hook 'helm-reset-yank-point)
(add-hook 'helm-cleanup-hook 'helm-reset-yank-point)


;;; Recentf files
;;
;;
(defvar helm-c-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . recentf-list)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match helm-c-match-on-basename)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

;;; ffap
(eval-when-compile (require 'ffap))
(defvar helm-c-source-ffap-guesser
  `((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (helm-aif
                        (with-helm-current-buffer
                          (ffap-guesser))
                        (list it))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)))

;;; ffap with line number
(defun helm-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (helm-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defun helm-c-ffap-line-candidates ()
  (with-helm-current-buffer
    (helm-attrset 'ffap-line-location (helm-c-ffap-file-line-at-point)))
  (helm-aif (helm-attr 'ffap-line-location)
      (destructuring-bind (file . line) it
        (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `helm-c-source-ffap-line'.
(defun helm-c-ffap-line-goto-line ()
  (when (car (helm-attr 'ffap-line-location))
    (unwind-protect
         (ignore-errors
           (with-selected-window
               (get-buffer-window
                (get-file-buffer (car (helm-attr 'ffap-line-location))))
             (helm-goto-line (cdr (helm-attr 'ffap-line-location)))))
      (helm-attrset 'ffap-line-location nil))))
(add-hook 'helm-after-action-hook 'helm-c-ffap-line-goto-line)
(add-hook 'helm-after-persistent-action-hook 'helm-c-ffap-line-goto-line)

(defvar helm-c-source-ffap-line
  `((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . helm-c-ffap-line-candidates)
    (keymap . ,helm-map)
    (type . file)))

;;; list of files gleaned from every dired buffer
(defun helm-c-files-in-all-dired-candidates ()
  (save-excursion
    (mapcan
     (lambda (dir)
       (cond ((listp dir)               ;filelist
              dir)
             ((equal "" (file-name-nondirectory dir)) ;dir
              (directory-files dir t))
             (t                         ;wildcard
              (file-expand-wildcards dir t))))
     (delq nil
           (mapcar (lambda (buf)
                     (set-buffer buf)
                     (when (eq major-mode 'dired-mode)
                       (if (consp dired-directory)
                           (cdr dired-directory) ;filelist
                           dired-directory))) ;dir or wildcard
                   (buffer-list))))))
;; (dired '("~/" "~/.emacs-custom.el" "~/.emacs.bmk"))

(defvar helm-c-source-files-in-all-dired
  '((name . "Files in all dired buffer.")
    (candidates . helm-c-files-in-all-dired-candidates)
    (type . file)))


;;;; <info>
;;; Info pages
(defvar helm-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-c-info-pages
      helm-c-info-pages
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
            topics)
        (require 'info)
        (with-temp-buffer
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1) topics))
          (kill-buffer))
        (setq helm-c-info-pages topics))))

(defvar helm-c-source-info-pages
  `((name . "Info Pages")
    (init . helm-c-info-pages-init)
    (candidates . helm-c-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))


;;; Man and woman UI
;;
;;
(defvar helm-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-man-default-action (candidate)
  "Default action for jumping to a woman or man page from helm."
  (let ((wfiles (woman-file-name-all-completions candidate)))
    (condition-case err
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
            (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error (kill-buffer) ; Kill woman buffer.
             (let ((Man-notify-method 'meek))
               (Man-getpage-in-background candidate))))))

(defvar helm-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if helm-c-man-pages
                        helm-c-man-pages
                        ;; XEmacs doesn't have a woman :)
                        (setq helm-c-man-pages
                              (ignore-errors
                                (require 'woman)
                                (woman-file-name "")
                                (sort (mapcar 'car woman-topic-all-completions)
                                      'string-lessp))))))
    (action  ("Show with Woman" . helm-c-man-default-action))
    ;; Woman does not work OS X
    ;; http://xahlee.org/emacs/modernization_man_page.html
    (action-transformer . (lambda (actions candidate)
                            (if (eq system-type 'darwin)
                                '(("Show with Man" . man))
                                actions)))
    (requires-pattern . 2)))


;;; LaCarte
(defvar helm-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte)))
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (delete '(nil) (lacarte-get-overall-menu-item-alist)))))
    (candidate-number-limit . 9999)
    (action . helm-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")


;;; Bookmarks
;;
;;
;; Bind some faces for bookmarks.
(defvar helm-c-bookmarks-face1 'helm-ff-directory)
(defvar helm-c-bookmarks-face2 'helm-ff-file)
(defvar helm-c-bookmarks-face3 'helm-bookmarks-su-face)

(eval-when-compile (require 'bookmark))
(defvar helm-c-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

;;; bookmark-set
(defvar helm-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar helm-c-source-bm
  '((name . "Visible Bookmarks")
    (init . helm-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun helm-c-bm-init ()
  "Init function for `helm-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (helm-candidate-buffer 'global)))
        (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                           '< :key 'overlay-start))
          (let ((start (overlay-start bm))
                (end (overlay-end bm))
                (annotation (or (overlay-get bm 'annotation) "")))
            (unless (< (- end start) 1) ; org => (if (< (- end start) 2)
              (let ((str (format "%5d: [%s]: %s\n"
                                 (line-number-at-pos start)
                                 annotation
                                 (buffer-substring start (1- end)))))
                (with-current-buffer buf (insert str))))))))))

;;; Special bookmarks
(defvar helm-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :ssh t)))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :su t)))
    (filtered-candidate-transformer helm-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defvar helm-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . (lambda () (helm-c-collect-bookmarks :local t)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun* helm-c-collect-bookmarks (&key local su sudo ssh)
  (let* ((lis-all (bookmark-all-names))
         (lis-loc (cond (local (loop for i in lis-all
                                     unless (string-match "^(ssh)\\|^(su)" i)
                                     collect i))
                        (su (loop for i in lis-all
                                  when (string-match "^(su)" i)
                                  collect i))
                        (sudo (loop for i in lis-all
                                    when (string-match "^(sudo)" i)
                                    collect i))
                        (ssh (loop for i in lis-all
                                   when (string-match "^(ssh)" i)
                                   collect i)))))
    (sort lis-loc 'string-lessp)))

(defun helm-c-bookmark-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." helm-su-or-sudo) i)
        (throw 'break t)))))

(defun helm-c-highlight-bookmark-su (files source)
  (if (helm-c-bookmark-root-logged-p)
      (helm-c-highlight-bookmark files source)
      (helm-c-highlight-not-logged files source)))

(defun helm-c-highlight-not-logged (files source)
  (loop for i in files
        collect (propertize i 'face helm-c-bookmarks-face3)))

(defun helm-c-highlight-bookmark (bookmarks source)
  "Used as `candidate-transformer' to colorize bookmarks.
Work both with standard Emacs bookmarks and bookmark-extensions.el."
  (loop for i in bookmarks
        for isfile        = (bookmark-get-filename i)
        for bufp          = (and (fboundp 'bmkext-get-buffer-name)
                                 (bmkext-get-buffer-name i))
        for handlerp      = (and (fboundp 'bookmark-get-handler)
                                 (bookmark-get-handler i))
        for isw3m         = (and (fboundp 'bmkext-w3m-bookmark-p)
                                 (bmkext-w3m-bookmark-p i))
        for isgnus        = (and (fboundp 'bmkext-gnus-bookmark-p)
                                 (bmkext-gnus-bookmark-p i))
        for isman         = (and (fboundp 'bmkext-man-bookmark-p) ; Man
                                 (bmkext-man-bookmark-p i))
        for iswoman       = (and (fboundp 'bmkext-woman-bookmark-p) ; Woman
                                 (bmkext-woman-bookmark-p i))
        for handlerp      = (bookmark-get-handler i)
        for isannotation  = (bookmark-get-annotation i)
        for isabook       = (string= (bookmark-prop-get i 'type) "addressbook")
        for isinfo        = (eq handlerp 'Info-bookmark-jump)
        ;; Add a * if bookmark have annotation
        if (and isannotation (not (string-equal isannotation "")))
        do (setq i (concat "*" i))
        collect (cond (;; info buffers
                       isinfo
                       (propertize i 'face 'helm-bmkext-info 'help-echo isfile))
                      (;; w3m buffers
                       isw3m
                       (propertize i 'face 'helm-bmkext-w3m 'help-echo isfile))
                      (;; gnus buffers
                       isgnus
                       (propertize i 'face 'helm-bmkext-gnus 'help-echo isfile))
                      (;; Man Woman
                       (or iswoman isman)
                       (propertize i 'face 'helm-bmkext-man 'help-echo isfile))
                      (;; Addressbook
                       isabook
                       (propertize i 'face '((:foreground "Tomato"))))
                      (;; directories
                       (and isfile (file-directory-p isfile))
                       (propertize i 'face helm-c-bookmarks-face1 'help-echo isfile))
                      (;; regular files
                       t
                       (propertize i 'face 'helm-bmkext-file 'help-echo isfile)))))

(defun helm-c-bookmark-jump (candidate)
  "Jump to bookmark from keyboard."
  (let ((current-prefix-arg helm-current-prefix-arg))
    (bookmark-jump candidate)))

;;;###autoload
(defun helm-c-bookmark-run-jump-other-window ()
  "Jump to bookmark from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bookmark-jump-other-window))

;;;###autoload
(defun helm-c-bookmark-run-delete ()
  "Delete bookmark from keyboard."
  (interactive)
  (when (y-or-n-p "Delete bookmark?")
    (helm-c-quit-and-execute-action 'helm-delete-marked-bookmarks)))


;;; Sources to filter bookmark-extensions bookmarks.
;;
;;
;; Dependency: http://mercurial.intuxication.org/hg/emacs-bookmark-extension
;; If you want to enable google-maps in addressbook you will need
;; Julien Danjou google-maps-el package available here:
;; http://julien.danjou.info/google-maps-el.html

(defun helm-c-bmkext-filter-setup-alist (fn &rest args)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (loop
        with alist = (if args
                         (apply #'(lambda (x) (funcall fn x)) args)
                         (funcall fn))
        for i in alist
        for b = (car i)
        collect b into sa
        finally return (sort sa 'string-lessp)))

;;;###autoload
(defun helm-c-bmkext-run-edit ()
  "Run `bmkext-edit-bookmark' from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bmkext-edit-bookmark))

;;; Addressbook.
;;
;;
(defvar helm-c-source-bmkext-addressbook
  '((name . "Bookmark Addressbook")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bmkext-addressbook-setup-alist)
    (persistent-action
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark--jump-via bmk 'pop-to-buffer))))
    (persistent-help . "Show contact - Prefix with C-u to append")
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (action . (("Show Contact(s)"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (current-prefix-arg (or helm-current-prefix-arg
                                                   (> (length contacts) 1))))
                      (bookmark-jump
                       (helm-bookmark-get-bookmark-from-name (car contacts)))
                      (helm-aif (cdr contacts)
                          (loop for bmk in it do
                                (bookmark-jump
                                 (helm-bookmark-get-bookmark-from-name bmk)))))))
               ("Send Mail"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (bmk      (helm-bookmark-get-bookmark-from-name
                                      (car contacts)))
                           (append   (message-buffers)))
                      (if append
                          (addressbook-set-mail-buffer1 bmk 'append)
                          (addressbook-set-mail-buffer1 bmk))
                      (setq contacts (cdr contacts))
                      (when contacts
                        (loop for bmk in contacts do
                              (addressbook-set-mail-buffer1 bmk 'append))))))
               ("Edit Bookmark"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (addressbook-bookmark-edit
                       (assoc bmk bookmark-alist)))))
               ("Insert Email at point"
                . (lambda (candidate)
                    (let* ((bmk   (helm-bookmark-get-bookmark-from-name
                                   candidate))
                           (mlist (split-string
                                   (assoc-default
                                    'email (assoc bmk bookmark-alist))
                                   ", ")))
                      (insert
                       (if (> (length mlist) 1)
                           (helm-comp-read
                            "Insert Mail Address: " mlist :must-match t)
                           (car mlist))))))
               ("Show annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-show-annotation bmk))))
               ("Edit annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-edit-annotation bmk))))
               ("Show Google map"
                . (lambda (candidate)
                    (let* ((bmk (helm-bookmark-get-bookmark-from-name
                                 candidate))
                           (full-bmk (assoc bmk bookmark-alist)))
                      (addressbook-google-map full-bmk))))))))


(defun helm-c-bmkext-addressbook-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-addressbook-alist-only))

;; W3m bookmarks from bookmark-extensions.
(defvar helm-c-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-w3m-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))

;; Images
(defvar helm-c-source-bookmark-images
  '((name . "Bookmark Images")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-images-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (helm-c-bmkext-filter-setup-alist 'bmkext-image-file-alist-only))

;; Woman Man
(defvar helm-c-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-man-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (helm-c-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (helm-c-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))

;; Gnus
(defvar helm-c-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-gnus-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (helm-c-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))

;; Info
(defvar helm-c-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-info-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (helm-c-bmkext-filter-setup-alist 'bmkext-info-alist-only))

;; Local Files&directories
(defvar helm-c-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-local-files-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (helm-c-bmkext-filter-setup-alist 'bmkext-local-file-alist-only))

;; Su Files&directories
(defvar helm-c-source-bookmark-su-files&dirs
  '((name . "Bookmark Root-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-su-files-setup-alist)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark-su)
    (type . bookmark)))

(defun helm-c-bookmark-su-files-setup-alist ()
  "Specialized filter function for bookmarks su/sudo files."
  (declare (special bmkext-su-or-sudo-regexp))
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for issu = (and istramp
                        (string-match bmkext-su-or-sudo-regexp isfile))
        if issu
        collect i))

;; Ssh Files&directories
(defvar helm-c-source-bookmark-ssh-files&dirs
  '((name . "Bookmark Ssh-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . helm-c-bookmark-ssh-files-setup-alist)
    (filtered-candidate-transformer . helm-c-adaptive-sort)
    (type . bookmark)))

(defun helm-c-bookmark-ssh-files-setup-alist ()
  "Specialized filter function for bookmarks ssh files."
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for isssh = (and istramp
                         (string-match "/ssh:" isfile))
        if isssh
        collect i))



;;; Firefox bookmarks
;;
;;
;; You will have to set firefox to import bookmarks in his html file bookmarks.html.
;; (only for firefox versions >=3)
;; To achieve that, open about:config in firefox and double click on this line to enable value
;; to true:
;; user_pref("browser.bookmarks.autoExportHTML", false);
;; You should have now:
;; user_pref("browser.bookmarks.autoExportHTML", true);
;; NOTE: This is also working in the same way for mozilla aka seamonkey.

(defvar helm-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ \"]*")
(defvar helm-firefox-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")

(defun helm-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let* ((moz-dir (concat (getenv "HOME") "/.mozilla/firefox/"))
         (moz-user-dir
          (with-current-buffer (find-file-noselect (concat moz-dir "profiles.ini"))
            (goto-char (point-min))
            (prog1
                (when (search-forward "Path=" nil t)
                  (buffer-substring-no-properties (point) (point-at-eol)))
              (kill-buffer)))))
    (file-name-as-directory (concat moz-dir moz-user-dir))))

(defun helm-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (helm-get-firefox-user-init-dir) "bookmarks.html"))

(defun helm-html-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
        (forward-line 0)
        (when (re-search-forward url-regexp nil t)
          (setq url (match-string 0)))
        (when (re-search-forward bmk-regexp nil t)
          (setq title (match-string 1)))
        (push (cons title url) bookmarks-alist)
        (forward-line)))
    (nreverse bookmarks-alist)))

(defvar helm-c-firefox-bookmarks-alist nil)
(defvar helm-c-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq helm-c-firefox-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     (helm-guess-firefox-bookmark-file)
                     helm-firefox-bookmark-url-regexp
                     helm-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-c-firefox-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-firefox-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-c-browse-url
                     (helm-c-firefox-bookmarks-get-value candidate))))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (helm-c-w3m-bookmarks-get-value elm))))))))


(defun helm-c-firefox-bookmarks-get-value (elm)
  (assoc-default elm helm-c-firefox-bookmarks-alist))

(defun helm-c-highlight-firefox-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face '((:foreground "YellowGreen"))
                 'help-echo (helm-c-firefox-bookmarks-get-value i))))



;;; W3m bookmark - helm interface.
;;
;;
;; Some users have the emacs-w3m library in load-path
;; without having the w3m executable :-;
;; So check if w3m program is present before trying to load
;; emacs-w3m.
(eval-when-compile
  (when (executable-find "w3m")
    (require 'w3m-bookmark nil t)))

(defvar w3m-bookmark-file "~/.w3m/bookmark.html")
(defvar helm-w3m-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")
(defvar helm-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar helm-c-w3m-bookmarks-alist nil)
(defvar helm-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq helm-c-w3m-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     w3m-bookmark-file
                     helm-w3m-bookmark-url-regexp
                     helm-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-c-w3m-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-w3m-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate)))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (helm-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Externally"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (helm-c-w3m-browse-bookmark candidate t)
                               (helm-c-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[helm-execute-persistent-action]: Open URL with Firefox"))
  "Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/")


(defun helm-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm helm-c-w3m-bookmarks-alist))))

(defun helm-c-w3m-browse-bookmark (elm &optional use-external new-tab)
  (let* ((fn  (if use-external 'helm-c-browse-url 'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (helm-c-w3m-bookmarks-get-value elm) arg)))

(defun helm-c-highlight-w3m-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face 'helm-w3m-bookmarks-face
                 'help-echo (helm-c-w3m-bookmarks-get-value i))))


(defun helm-c-w3m-delete-bookmark (elm)
  "Delete w3m bookmark from `w3m-bookmark-file'."
  (with-current-buffer
      (find-file-literally w3m-bookmark-file)
    (goto-char (point-min))
    (when (re-search-forward elm nil t)
      (beginning-of-line)
      (delete-region (point)
                     (line-end-position))
      (delete-blank-lines))
    (save-buffer)
    (kill-buffer)))

(defun helm-c-w3m-rename-bookmark (elm)
  "Rename w3m bookmark in `w3m-bookmark-file'."
  (let* ((old-title (replace-regexp-in-string ">" "" elm))
         (new-title (read-string "NewTitle: " old-title)))
    (with-current-buffer
        (find-file-literally w3m-bookmark-file)
      (goto-char (point-min))
      (when (re-search-forward (concat elm "<") nil t)
        (goto-char (1- (point)))
        (delete-char (- (length old-title)))
        (insert new-title))
      (save-buffer)
      (kill-buffer))))


;;;; <Library>
;;; Elisp library scan
;;
;;
(defvar helm-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (helm-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library"
             . (lambda (candidate) (find-file (find-library-name candidate))))
     ("Find library other window"
      . (lambda (candidate)
          (find-file-other-window (find-library-name candidate))))
     ("Load library"
      . (lambda (candidate) (load-library candidate))))))

(defun helm-c-elisp-library-scan-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (helm-c-elisp-library-scan-list)))
    (with-current-buffer helm-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun helm-c-elisp-library-scan-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        ;; File regexp that suffix match `load-file-rep-suffixes'.
        (match-regexp (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match match-regexp name)
              (add-to-list 'names name)))))
    names))


;;;; <Programming>
;;
;;

;;; Imenu
;;
;;
(defvar helm-c-imenu-delimiter " / ")

(defvar helm-c-imenu-index-filter nil)
(make-variable-buffer-local 'helm-c-imenu-index-filter)

(defvar helm-c-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-c-cached-imenu-alist)

(defvar helm-c-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-c-cached-imenu-candidates)

(defvar helm-c-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-c-cached-imenu-tick)

(eval-when-compile (require 'imenu))
(setq imenu-auto-rescan t)

(defun helm-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan
       (lambda (sub)
         (if (consp (cdr sub))
             (mapcar
              (lambda (subentry)
                (concat (car entry) helm-c-imenu-delimiter subentry))
              (helm-imenu-create-candidates sub))
             (list (concat (car entry) helm-c-imenu-delimiter (car sub)))))
       (cdr entry))
      (list entry)))

(defvar helm-c-source-imenu
  '((name . "Imenu")
    (init . (lambda () (require 'imenu)))
    (candidates . helm-c-imenu-candidates)
    (persistent-action . (lambda (elm)
                           (helm-c-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . helm-c-imenu-default-action))
  "See (info \"(emacs)Imenu\")")


(defun helm-c-imenu-candidates ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-c-cached-imenu-tick tick)
          helm-c-cached-imenu-candidates
          (setq imenu--index-alist nil)
          (setq helm-c-cached-imenu-tick tick
                helm-c-cached-imenu-candidates
                (ignore-errors
                  (mapcan
                   'helm-imenu-create-candidates
                   (setq helm-c-cached-imenu-alist
                         (let ((index (imenu--make-index-alist)))
                           (if helm-c-imenu-index-filter
                               (funcall helm-c-imenu-index-filter index)
                               index))))))
          (setq helm-c-cached-imenu-candidates
                (mapcar #'(lambda (x)
                            (if (stringp x)
                                x
                                (car x)))
                        helm-c-cached-imenu-candidates))))))

(setq imenu-default-goto-function 'imenu-default-goto-function)
(defun helm-c-imenu-default-action (elm)
  "The default action for `helm-c-source-imenu'."
  (let ((path (split-string elm helm-c-imenu-delimiter))
        (alist helm-c-cached-imenu-alist))
    (dolist (elm path)
      (setq alist (assoc elm alist)))
    (imenu alist)))



;;; Ctags
;;
;;
(defvar helm-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
    makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
    scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun helm-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode helm-c-ctags-modes)
             (helm-current-buffer-is-modified))
    (with-current-buffer (helm-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" helm-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) "
                   helm-buffer-file-name)
           (format "ctags -e -u -f- --fields=n %s " helm-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring
                          lineno-start
                          (1- (search-forward "," (point-at-eol) t)))
            do
            (beginning-of-line)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar helm-c-source-ctags
  '((name . "Exuberant ctags")
    (init . helm-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")


;;; Etags
;;
;;
;; helm-etags.el is deprecated, if this file is found,
;; warn user at compile time.
(eval-when-compile
  (when (locate-library "helm-etags.el")
    (display-warning
     '(helm-config)
     "You are using obsolete library `helm-etags.el' and should remove it."
     :warning)))

(defvar helm-c-etags-tag-file-dir nil
  "Etags file directory.")
(defvar helm-c-etags-mtime-alist nil
  "Store the last modification time of etags files here.")
(defvar helm-c-etags-cache (make-hash-table :test 'equal)
  "Cache content of etags files used here for faster access.")

(defun helm-c-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (helm-c-etags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      ;; Set tag file directory.
      (setq helm-c-etags-tag-file-dir current-dir)
      (expand-file-name helm-c-etags-tag-file-name current-dir))))

(defun helm-c-etags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name
                            helm-c-etags-tag-file-name dir)))
             (and (stringp tag-path)
                  (file-regular-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
          until (file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `helm-c-etags-tag-file-search-limit'.
          if (= count helm-c-etags-tag-file-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun helm-c-source-etags-header-name (x)
  "Create header name for this helm etags session."
  (concat "Etags in "
          (with-helm-current-buffer
            (helm-c-etags-get-tag-file))))

(defmacro helm-c-etags-create-buffer (file)
  "Create the `helm-buffer' based on contents of etags tag FILE."
  `(let* ((tag-fname ,file)
          max
          (split (with-current-buffer (find-file-noselect tag-fname)
                   (prog1
                       (split-string (buffer-string) "\n" 'omit-nulls)
                     (setq max (line-number-at-pos (point-max)))
                     (kill-buffer))))
          (progress-reporter (make-progress-reporter "Loading tag file..." 0 max)))
     (loop
           with fname
           with cand
           for i in split for count from 0
           for elm = (unless (string-match "^\x0c" i)
                       (helm-aif (string-match "\177" i)
                           (substring i 0 it)
                         i))
           do (cond ((and elm (string-match "^\\(.+\\),[0-9]+" elm))
                     (setq fname (match-string 1 elm)))
                    (elm (setq cand (concat fname ": " elm)))
                    (t (setq cand nil)))
           when cand do (progn
                          (insert (concat cand "\n"))
                          (progress-reporter-update progress-reporter count)))))

(defun helm-c-etags-init ()
  "Feed `helm-buffer' using `helm-c-etags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfile (helm-c-etags-get-tag-file)))
    (when tagfile
      (with-current-buffer (helm-candidate-buffer 'global)
        (helm-aif (gethash tagfile helm-c-etags-cache)
            ;; An entry is present in cache, insert it.
            (insert it)
          ;; No entry, create a new buffer using content of tag file (slower).
          (helm-c-etags-create-buffer tagfile)
          ;; Store content of buffer in cache.
          (puthash tagfile (buffer-string) helm-c-etags-cache)
          ;; Store or set the last modification of tag file.
          (helm-aif (assoc tagfile helm-c-etags-mtime-alist)
              ;; If an entry exists modify it.
              (setcdr it (helm-c-etags-mtime tagfile))
            ;; No entry create a new one.
            (add-to-list 'helm-c-etags-mtime-alist
                         (cons tagfile (helm-c-etags-mtime tagfile)))))))))

(defvar helm-c-source-etags-select
  '((name . "Etags")
    (header-name . helm-c-source-etags-header-name)
    (init . helm-c-etags-init)
    (candidates-in-buffer)
    (search . (helm-c-etags-search-fn))
    (mode-line . helm-etags-mode-line-string)
    (action . helm-c-etags-default-action)
    (persistent-action . (lambda (candidate)
                           (helm-c-etags-default-action candidate)
                           (helm-match-line-color-current-line))))
  "Helm source for Etags.")

(defun helm-c-etags-search-fn (pattern)
  "Search function for `helm-c-source-etags-select'."
  (re-search-forward
   (if helm-c-etags-use-regexp-search
       (format helm-c-etags-search-regexp pattern)
       pattern)
   nil t))

(defun helm-c-etags-default-action (candidate)
  "Helm default action to jump to an etags entry."
  (let* ((split (split-string candidate ": "))
         (fname (expand-file-name
                 (car split) helm-c-etags-tag-file-dir))
         (elm   (cadr split)))
    (find-file fname)
    (goto-char (point-min))
    (search-forward elm nil t)
    (goto-char (match-beginning 0))))

(defun helm-c-etags-mtime (file)
  "Last modification time of etags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun helm-c-etags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file helm-c-etags-mtime-alist))))
    (and last-modif
         (/= last-modif (helm-c-etags-mtime file)))))



;;; Semantic
;;
;;
(defvar helm-semantic-candidates nil)

(defun helm-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply
     'append
     (mapcar
      (lambda (tag)
        (if (listp tag)
            (let ((type (semantic-tag-type tag))
                  (class (semantic-tag-class tag)))
              (if (or (and (stringp type)
                           (or (string= type "class")
                               (string= type "namespace")))
                      (eq class 'function)
                      (eq class 'variable))
                  (cons (cons (concat (make-string (* depth 2) ?\s)
                                      (semantic-format-tag-summarize tag nil t))
                              tag)
                        (helm-semantic-construct-candidates
                         (semantic-tag-components tag) (1+ depth)))))))
      tags))))

(defun helm-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate helm-semantic-candidates))))
    (semantic-go-to-tag tag)))

(defvar helm-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq helm-semantic-candidates
                    (ignore-errors (helm-semantic-construct-candidates
                                    (semantic-fetch-tags) 0)))))
    (candidates . (lambda ()
                    (if helm-semantic-candidates
                        (mapcar 'car helm-semantic-candidates))))
    (persistent-action . (lambda (elm)
                           (helm-semantic-default-action elm)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . helm-semantic-default-action)
    "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))



;;; Helm interface of `simple-call-tree.el'.
;;
;; <http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el>
;;
;; Function is called by
(defvar helm-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . helm-c-simple-call-tree-functions-callers-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defvar helm-c-simple-call-tree-tick nil)
(make-variable-buffer-local 'helm-c-simple-call-tree-tick)
(defun helm-c-simple-call-tree-analyze-maybe ()
  (unless (eq (buffer-chars-modified-tick) helm-c-simple-call-tree-tick)
    (simple-call-tree-analyze)
    (setq helm-c-simple-call-tree-tick (buffer-chars-modified-tick))))

(defun helm-c-simple-call-tree-init-base (function message)
  (require 'simple-call-tree)
  (with-no-warnings
    (when (helm-current-buffer-is-modified)
      (helm-c-simple-call-tree-analyze-maybe)
      (let ((list (funcall function simple-call-tree-alist)))
        (with-current-buffer (helm-candidate-buffer 'local)
          (dolist (entry list)
            (let ((funcs (concat "  " (mapconcat #'identity (cdr entry) "\n  "))))
              (insert (car entry) message
                      (if (string= funcs "  ")
                          "  no functions."
                          funcs)
                      "\n\n"))))))))

(defun helm-c-simple-call-tree-functions-callers-init ()
  (helm-c-simple-call-tree-init-base 'simple-call-tree-invert
                                     " is called by\n"))

(defun helm-c-simple-call-tree-candidates ()
  (with-current-buffer (helm-candidate-buffer)
    (split-string (buffer-string) "\n\n")))

(defvar helm-c-simple-call-tree-related-functions nil)
(defvar helm-c-simple-call-tree-function-index 0)
(defun helm-c-simple-call-tree-persistent-action (candidate)
  (unless (eq last-command 'helm-execute-persistent-action)
    (setq helm-c-simple-call-tree-related-functions
          (delete "no functions."
                  (split-string
                   (replace-regexp-in-string "  \\| is called by\\| calls "
                                             "" candidate)
                   "\n")))
    (setq helm-c-simple-call-tree-function-index -1))
  (incf helm-c-simple-call-tree-function-index)
  (helm-c-simple-call-tree-find-definition candidate))

(defun helm-c-simple-call-tree-find-definition (candidate)
  (find-function
   (intern
    (nth (mod helm-c-simple-call-tree-function-index
              (length helm-c-simple-call-tree-related-functions))
         helm-c-simple-call-tree-related-functions))))


;;; Function calls
(defvar helm-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . helm-c-simple-call-tree-callers-functions-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun helm-c-simple-call-tree-callers-functions-init ()
  (helm-c-simple-call-tree-init-base 'identity " calls \n"))


;;;; <Color and Face>
;;

;;; Customize Face
;;
;;
(defvar helm-c-source-customize-face
  '((name . "Customize Face")
    (init . (lambda ()
              (unless (helm-candidate-buffer)
                (save-selected-window
                  (list-faces-display))
                (helm-candidate-buffer (get-buffer "*Faces*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line))))))
    (requires-pattern . 3))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defvar helm-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (helm-candidate-buffer)
                         (save-selected-window
                           (list-colors-display))
                         (helm-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" . (lambda (candidate)
                      (kill-new (helm-c-colors-get-name candidate))))
     ("Copy RGB" . (lambda (candidate)
                     (kill-new (helm-c-colors-get-rgb candidate))))
     ("Insert Name" . (lambda (candidate)
                        (with-helm-current-buffer
                          (insert (helm-c-colors-get-name candidate)))))
     ("Insert RGB" . (lambda (candidate)
                       (with-helm-current-buffer
                         (insert (helm-c-colors-get-rgb candidate))))))))

(defun helm-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun helm-c-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-min))
     (buffer-string))))


;;;; <Search Engine>
;;; Tracker desktop search
(defvar helm-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;; Spotlight (MacOS X desktop search)
(defvar helm-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates
     . (lambda () (start-process "mdfind-process" nil "mdfind" helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Picklist
(defvar helm-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
    (type . file)))


;;; Latex completion
(defun helm-c-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (declare (special LaTeX-math-menu))
  (loop for i in (cddr LaTeX-math-menu)
        for elm = (loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-c-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-helm-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . helm-c-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar helm-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar helm-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar helm-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar helm-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar helm-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar helm-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Helm yaoddmuse
;;
;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar helm-yaoddmuse-use-cache-file nil)
(defvar helm-c-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar helm-c-yaoddmuse-ew-cache nil)

(defun helm-yaoddmuse-get-candidates ()
  (declare (special yaoddmuse-pages-hash))
  (if helm-yaoddmuse-use-cache-file
      (ignore-errors
        (unless helm-c-yaoddmuse-ew-cache
          (load helm-c-yaoddmuse-cache-file)
          (setq helm-c-yaoddmuse-ew-cache
                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
        helm-c-yaoddmuse-ew-cache)
      (yaoddmuse-update-pagename t)
      (gethash "EmacsWiki" yaoddmuse-pages-hash)))

(defvar helm-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . helm-yaoddmuse-get-candidates)
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page"
                . (lambda (candidate)
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window"
                . (lambda (candidate)
                    (if (one-window-p)
                        (split-window-vertically))
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff"
                . (lambda (candidate)
                    (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL"
                . (lambda (candidate)
                    (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                    (message "Have copy page %s's URL to yank." candidate)))
               ("Create page"
                . (lambda (candidate)
                    (yaoddmuse-edit "EmacsWiki" helm-input)))
               ("Update cache"
                . (lambda (candidate)
                    (if helm-yaoddmuse-use-cache-file
                        (progn
                          (helm-yaoddmuse-cache-pages t)
                          (setq helm-c-yaoddmuse-ew-cache
                                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                        (yaoddmuse-update-pagename))))))
    (action-transformer helm-c-yaoddmuse-action-transformer))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defvar helm-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (helm-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library and Browse"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory (find-library-name candidate))
                     nil t)))
               ("Post library"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory
                      (find-library-name candidate))))))))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defun helm-c-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp"
                         . (lambda (elm)
                             (install-elisp-from-emacswiki elm)))))
      actions))

;;;###autoload
(defun helm-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (declare (special yaoddmuse-pages-hash))
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file helm-c-yaoddmuse-cache-file)
    (erase-buffer)
    (insert "(puthash \"EmacsWiki\" '(")
    (loop for i in (gethash "EmacsWiki" yaoddmuse-pages-hash)
          do
          (insert (concat "(\"" (car i) "\") ")))
    (insert ") yaoddmuse-pages-hash)\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (when (or current-prefix-arg
              load)
      (load helm-c-yaoddmuse-cache-file))))

(defun helm-yaoddmuse-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer helm-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))


;;; Eev anchors
(defvar helm-c-source-eev-anchor
  '((name . "Anchors")
    (candidates
     . (lambda ()
         (ignore-errors
           (with-helm-current-buffer
             (loop initially (goto-char (point-min))
                   while (re-search-forward
                          (format ee-anchor-format "\\([^\.].+\\)") nil t)
                   for anchor = (match-string-no-properties 1)
                   collect (cons (format "%5d:%s"
                                         (line-number-at-pos (match-beginning 0))
                                         (format ee-anchor-format anchor))
                                 anchor))))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . ee-to)))))


;;; Org headlines
;;
;;
(defvar helm-c-source-org-headline
  `((name . "Org HeadLine")
    (headline
     ,@(mapcar
        (lambda (num)
          (format "^\\*\\{%d\\} \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
                  num))
        (number-sequence 1 8)))
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)
    (persistent-action . (lambda (elm)
                           (helm-c-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to Line" . helm-c-action-line-goto)
           ("Refile to this Headline" . helm-c-org-headline-refile)
           ("Insert Link to This Headline"
            . helm-c-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun helm-c-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (helm-goto-line (car lineno-and-content))
     (and (looking-at org-complex-heading-regexp)
          (org-make-link-string (concat "*" (match-string 4)))))))

(defun helm-c-org-headline-refile (lineno-and-content)
  "Refile current org entry to LINENO-AND-CONTENT."
  (with-helm-current-buffer
    (org-cut-subtree)
    (helm-goto-line (car lineno-and-content))
    (org-end-of-subtree t t)
    (let ((org-yank-adjusted-subtrees t))
      (org-yank))))


;;; Org keywords
;;
;;
(defvar helm-c-source-org-keywords
  '((name . "Org Keywords")
    (init . helm-c-org-keywords-init)
    (candidates . helm-c-org-keywords-candidates)
    (action . helm-c-org-keywords-insert)
    (persistent-action . helm-c-org-keywords-show-help)
    (persistent-help . "Show an example and info page to describe this keyword.")
    (keywords-examples)
    (keywords)))

(defvar helm-c-org-keywords-info-location
  '(("#+TITLE:" . "(org)Export options")
    ("#+AUTHOR:" . "(org)Export options")
    ("#+DATE:" . "(org)Export options")
    ("#+EMAIL:" . "(org)Export options")
    ("#+DESCRIPTION:" . "(org)Export options")
    ("#+KEYWORDS:" . "(org)Export options")
    ("#+LANGUAGE:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+OPTIONS:" . "(org)Export options")
    ("#+BIND:" . "(org)Export options")
    ("#+LINK_UP:" . "(org)Export options")
    ("#+LINK_HOME:" . "(org)Export options")
    ("#+LATEX_HEADER:" . "(org)Export options")
    ("#+EXPORT_SELECT_TAGS:" . "(org)Export options")
    ("#+EXPORT_EXCLUDE_TAGS:" . "(org)Export options")
    ("#+INFOJS_OPT" . "(org)Javascript support")
    ("#+BEGIN_HTML" . "(org)Quoting HTML tags")
    ("#+BEGIN_LaTeX" . "(org)Quoting LaTeX code")
    ("#+ORGTBL" . "(org)Radio tables")
    ("#+HTML:" . "(org)Quoting HTML tags")
    ("#+LaTeX:" . "(org)Quoting LaTeX code")
    ("#+BEGIN:" . "(org)Dynamic blocks") ;clocktable columnview
    ("#+BEGIN_EXAMPLE" . "(org)Literal examples")
    ("#+BEGIN_QUOTE" . "(org)Paragraphs")
    ("#+BEGIN_VERSE" . "(org)Paragraphs")
    ("#+BEGIN_SRC" . "(org)Literal examples")
    ("#+CAPTION" . "(org)Tables in HTML export")
    ("#+LABEL" . "(org)Tables in LaTeX export")
    ("#+ATTR_HTML" . "(org)Links")
    ("#+ATTR_LaTeX" . "(org)Images in LaTeX export")))

(defun helm-c-org-keywords-init ()
  (unless (helm-attr 'keywords-examples)
    (require 'org)
    (helm-attrset 'keywords-examples
                  (append
                   (mapcar
                    (lambda (x)
                      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                      (cons (match-string 2 x) (match-string 1 x)))
                    (org-split-string (org-get-current-options) "\n"))
                   (mapcar 'list org-additional-option-like-keywords)))
    (helm-attrset 'keywords (mapcar 'car (helm-attr 'keywords-examples)))))

(defun helm-c-org-keywords-candidates ()
  (and (or (eq (buffer-local-value 'major-mode helm-current-buffer) 'org-mode)
           (eq (buffer-local-value 'major-mode helm-current-buffer) 'message-mode))
       (helm-attr 'keywords)))

(defun helm-c-org-keywords-insert (keyword)
  (cond ((and (string-match "BEGIN" keyword)
              (helm-region-active-p))
         (let ((beg (region-beginning))
               (end (region-end)))
           (goto-char end)
           (insert "\n#+" (replace-regexp-in-string
                           "BEGIN" "END" keyword) "\n")
           (goto-char beg)
           (insert "#+" keyword " ")
           (save-excursion (insert "\n"))))
        ((string-match "BEGIN" keyword)
         (insert "#+" keyword " ")
         (save-excursion
           (insert "\n#+" (replace-regexp-in-string
                           "BEGIN" "END" keyword) "\n")))
        (t (insert "#+" keyword " "))))

(defun helm-c-org-keywords-show-help (keyword)
  (info (or (assoc-default (concat "#+" keyword) helm-c-org-keywords-info-location)
            "(org)In-buffer settings"))
  (search-forward (concat "#+" keyword) nil t)
  (helm-persistent-highlight-point)
  (message "%s" (or (cdr (assoc keyword (helm-attr 'keywords-examples))) "")))



;;; bbdb
;;
;;
(defvar bbdb-records)
(defvar bbdb-buffer-name)

(defun helm-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun helm-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `helm-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " helm-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
      actions))

(defun helm-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar helm-c-bbdb-name nil
  "Only for internal use.")

(defvar helm-c-source-bbdb
  '((name . "BBDB")
    (candidates . helm-c-bbdb-candidates)
    (action ("Send a mail" . helm-c-bbdb-compose-mail)
     ("View person's data" . helm-c-bbdb-view-person-action))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq helm-c-bbdb-name helm-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                            candidates)))
    (action-transformer . (lambda (actions candidate)
                            (helm-c-bbdb-create-contact actions candidate))))
  "Needs BBDB.

http://bbdb.sourceforge.net/")

(defun helm-c-bbdb-view-person-action (candidate)
  "View BBDB data of single CANDIDATE or marked candidates."
  (helm-aif (helm-marked-candidates)
      (let ((bbdb-append-records (length it)))
        (dolist (i it)
          (bbdb-redisplay-one-record (helm-c-bbdb-get-record i))))
    (bbdb-redisplay-one-record (helm-c-bbdb-get-record candidate))))

(defun helm-c-bbdb-collect-mail-addresses ()
  "Return a list of all mail addresses of records in bbdb buffer."
  (with-current-buffer bbdb-buffer-name
    (loop for i in bbdb-records
          if (bbdb-record-net (car i))
          collect (bbdb-dwim-net-address (car i)))))

(defun helm-c-bbdb-compose-mail (candidate)
  "Compose a mail with all records of bbdb buffer."
  (helm-c-bbdb-view-person-action candidate)
  (let* ((address-list (helm-c-bbdb-collect-mail-addresses))
         (address-str  (mapconcat 'identity address-list ",\n    ")))
    (compose-mail address-str)))


;;; Emms
;;
;;
(defun helm-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from helm."
  (declare (special emms-stream-list))
  (let* ((cur-buf helm-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name     (read-from-minibuffer "Description: "
                                         (nth 0 bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (nth 1 bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (nth 2 bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (car (last bookmark))))))
    (save-window-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-add-bookmark name url (string-to-number fd) type)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (helm-c-switch-to-buffer cur-buf)))))

(defun helm-emms-stream-delete-bookmark (candidate)
  "Delete emms-streams bookmarks from helm."
  (let* ((cands   (helm-marked-candidates))
         (bmks    (loop for bm in cands collect
                        (car (assoc bm emms-stream-list))))
         (bmk-reg (mapconcat 'regexp-quote bmks "\\|^")))
    (when (y-or-n-p (format "Really delete radios\n -%s: ? "
                            (mapconcat 'identity bmks "\n -")))
      (save-window-excursion
        (emms-streams)
        (goto-char (point-min))
        (loop while (re-search-forward bmk-reg nil t)
              do (progn (beginning-of-line)
                        (emms-stream-delete-bookmark))
              finally do (progn
                           (emms-stream-save-bookmarks-file)
                           (emms-stream-quit)))))))

(defvar helm-c-source-emms-streams
  '((name . "Emms Streams")
    (init . (lambda ()
              (emms-stream-init)))
    (candidates . (lambda ()
                    (declare (special emms-stream-list))
                    (mapcar 'car emms-stream-list)))
    (action . (("Play" . (lambda (elm)
                           (declare (special emms-stream-list))
                           (let* ((stream (assoc elm emms-stream-list))
                                  (fn (intern (concat "emms-play-" (symbol-name (car (last stream))))))
                                  (url (second stream)))
                             (funcall fn url))))
               ("Delete" . helm-emms-stream-delete-bookmark)
               ("Edit" . helm-emms-stream-edit-bookmark)))
    (filtered-candidate-transformer . helm-c-adaptive-sort)))

;; Don't forget to set `emms-source-file-default-directory'
(defvar helm-c-source-emms-dired
  '((name . "Music Directory")
    (candidates . (lambda ()
                    (cddr (directory-files emms-source-file-default-directory))))
    (action .
     (("Play Directory" . (lambda (item)
                            (emms-play-directory
                             (expand-file-name
                              item
                              emms-source-file-default-directory))))
      ("Open dired in file's directory" . (lambda (item)
                                            (helm-c-open-dired
                                             (expand-file-name
                                              item
                                              emms-source-file-default-directory))))))
    (filtered-candidate-transformer . helm-c-adaptive-sort)))


(defun helm-c-emms-files-modifier (candidates source)
  (let ((current-playlist (with-current-emms-playlist
                            (loop with cur-list = (emms-playlist-tracks-in-region
                                                   (point-min) (point-max))
                                  for i in cur-list
                                  for name = (assoc-default 'name i)
                                  when name
                                  collect name))))
    (loop for i in candidates
          if (member (cdr i) current-playlist)
          collect (cons (propertize (car i)
                                    'face 'helm-emms-playlist)
                        (cdr i)) into lis
          else collect i into lis
          finally return (reverse lis))))

(defun helm-c-emms-play-current-playlist ()
  "Play current playlist."
  (with-current-emms-playlist
    (emms-playlist-first)
    (emms-playlist-mode-play-smart)))

(defvar helm-c-source-emms-files
  '((name . "Emms files")
    (candidates . (lambda ()
                    (loop for v being the hash-values in emms-cache-db
                          for name      = (assoc-default 'name v)
                          for artist    = (or (assoc-default 'info-artist v) "unknown")
                          for genre     = (or (assoc-default 'info-genre v) "unknown")
                          for tracknum  = (or (assoc-default 'info-tracknumber v) "unknown")
                          for song      = (or (assoc-default 'info-title v) "unknown")
                          for info      = (concat artist " - " genre " - " tracknum ": " song)
                          unless (string-match "^\\(http\\|mms\\):" name)
                          collect (cons info name))))
    (filtered-candidate-transformer . helm-c-emms-files-modifier)
    (candidate-number-limit . 9999)
    (action . (("Play file" . emms-play-file)
               ("Add to Playlist and play (C-u clear current)"
                . (lambda (candidate)
                    (when helm-current-prefix-arg
                      (emms-playlist-current-clear))
                    (emms-playlist-new)
                    (mapc 'emms-add-playlist-file (helm-marked-candidates))
                    (unless emms-player-playing-p
                      (helm-c-emms-play-current-playlist))))))))



;;; Jabber Contacts (jabber.el)
(defun helm-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                    (cons (symbol-name item) item)) jids))))))

(defvar helm-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (helm-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (helm-c-jabber-online-contacts)))))))))



;;; Call source.
(defvar helm-source-select-buffer "*helm source select*")
(defvar helm-c-source-call-source
  `((name . "Call helm source")
    (candidate-number-limit)
    (candidates
     . (lambda ()
         (loop for vname in (all-completions "helm-c-source-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect
               (cons (format "%s `%s'"
                             name (propertize vname 'face 'font-lock-variable-name-face))
                     var))))
    (action
     . (("Invoke helm with selected source"
         .
         (lambda (candidate)
           (setq helm-candidate-number-limit 9999)
           (helm candidate nil nil nil nil
                 helm-source-select-buffer)))
        ("Describe variable" . describe-variable)
        ("Find variable" . find-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))

(defun helm-call-source-from-helm ()
  "Call helm source within `helm' session."
  (interactive)
  (setq helm-input-idle-delay 0)
  (helm-set-sources '(helm-c-source-call-source)))

;;; Execute Preconfigured helm.
(defvar helm-c-source-helm-commands
  '((name . "Preconfigured Helm")
    (candidates . helm-c-helm-commands-candidates)
    (type . command)
    (candidate-number-limit)))

(defun helm-c-helm-commands-candidates ()
  (loop for (cmd . desc) in (helm-c-list-preconfigured-helm)
        collect (cons (if (where-is-internal cmd nil t)
                          (substitute-command-keys (format "M-x %s (\\[%s]) : %s" cmd cmd desc))
                          (substitute-command-keys (format "\\[%s] : %s" cmd desc)))
                      cmd)))


;;; Helm browse code.
(defun helm-c-browse-code-get-line (beg end)
  "Select line if it match the regexp corresponding to current `major-mode'.
Line is parsed for BEG position to END position."
  (let ((str-line (buffer-substring beg end))
        (regexp   (assoc-default major-mode
                                 helm-c-browse-code-regexp-alist))
        (num-line (if (string= helm-pattern "") beg (1- beg))))
    (when (and regexp (string-match regexp str-line))
      (format "%4d:%s" (line-number-at-pos num-line) str-line))))


(defvar helm-c-source-browse-code
  '((name . "Browse code")
    (init . (lambda ()
              (helm-candidate-buffer helm-current-buffer)
              (with-helm-current-buffer
                (jit-lock-fontify-now))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (get-line . helm-c-browse-code-get-line)
    (type . line)
    (recenter)))


;; Do many actions for input
(defvar helm-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (action-transformer . helm-create--actions))
  "Do many create actions from `helm-pattern'.
See also `helm-create--actions'.")

(defun helm-create-from-helm ()
  "Run `helm-create' from `helm' as a fallback."
  (interactive)
  (helm-run-after-quit 'helm-create nil helm-pattern))

(defun helm-create--actions (&rest ignored)
  "Default actions for `helm-create' / `helm-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append helm-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . helm-c-switch-to-buffer)
             ("New buffer other window" . switch-to-buffer-other-window)
             ("Bookmark Set" . bookmark-set)
             ("Set Register" .
              (lambda (x) (set-register (read-char "Register: ") x)))
             ("Insert Linkd star" . linkd-insert-star)
             ("Insert Linkd Tag" . linkd-insert-tag)
             ("Insert Linkd Link" . linkd-insert-link)
             ("Insert Linkd Lisp" . linkd-insert-lisp)
             ("Insert Linkd Wiki" . linkd-insert-wiki)
             ("Google Search" . google)))))


;; Minibuffer History
;;
;;
(defvar helm-c-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name)
                     (format "%s (%s)" name minibuffer-history-variable)))
    (candidates
     . (lambda ()
         (let ((history (loop for i in
                              (symbol-value minibuffer-history-variable)
                              unless (string= "" i) collect i)))
           (if (consp (car history))
               (mapcar 'prin1-to-string history)
               history))))
    (migemo)
    (action . (lambda (candidate)
                (delete-minibuffer-contents)
                (insert candidate)))))


;;; Elscreen
;;
;;
(defun helm-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (helm-require-or-error 'elscreen 'helm-find-buffer-on-elscreen)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun helm-elscreen-find-file (file)
  (helm-require-or-error 'elscreen 'helm-elscreen-find-file)
  (elscreen-find-file file))

(defvar helm-c-source-elscreen
  '((name . "Elscreen")
    (candidates
     . (lambda ()
         (if (cdr (elscreen-get-screen-to-name-alist))
             (sort
              (loop for sname in (elscreen-get-screen-to-name-alist)
                    append (list (format "[%d] %s" (car sname) (cdr sname))))
              #'(lambda (a b) (compare-strings a nil nil b nil nil))))))
    (action
     . (("Change Screen" .
                         (lambda (candidate)
                           (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
        ("Kill Screen(s)" .
                          (lambda (candidate)
                            (dolist (i (helm-marked-candidates))
                              (elscreen-goto (- (aref i 1) (aref "0" 0)))
                              (elscreen-kill))))
        ("Only Screen" .
                       (lambda (candidate)
                         (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                         (elscreen-kill-others)))))))


;;;; <System>

;;; Top (process)
(defvar helm-c-top-command "COLUMNS=%s top -b -n 1"
  "Top command (batch mode). %s is replaced with `frame-width'.")
(defvar helm-c-source-top
  '((name . "Top (Press C-c C-u to refresh)")
    (init . helm-c-top-init)
    (candidates-in-buffer)
    (display-to-real . helm-c-top-display-to-real)
    (persistent-action . helm-c-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (action
     ("kill (TERM)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -TERM %s" pid))))
     ("kill (KILL)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -KILL %s" pid))))
     ("Copy PID" . (lambda (pid) (kill-new pid))))))

(defun helm-c-top-sh (cmd)
  (message "Executed %s\n%s" cmd (shell-command-to-string cmd)))

(defun helm-c-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-c-top-sh (format "kill -TERM %s" pid))
  (helm-force-update))

(defun helm-c-top-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     (format helm-c-top-command
             (- (frame-width) (if helm-enable-digit-shortcuts 4 0)))
     nil (current-buffer))))

(defun helm-c-top-display-to-real (line)
  (car (split-string line)))

;;; X RandR resolution change
;;
;;
;;; FIXME I do not care multi-display.

(defun helm-c-xrandr-info ()
  "Return a pair with current X screen number and current X display name."
  (with-temp-buffer
    (call-process "xrandr" nil (current-buffer) nil
                  "--current")
    (let (screen output)
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
          (setq screen (match-string 2))))
      (when (re-search-forward "^\\(.*\\) connected" nil t)
        (setq output (match-string 1)))
      (list screen output))))

(defun helm-c-xrandr-screen ()
  "Return current X screen number."
  (car (helm-c-xrandr-info)))

(defun helm-c-xrandr-output ()
  "Return current X display name."
  (cadr (helm-c-xrandr-info)))

(defvar helm-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (helm-c-xrandr-screen) "-q")
           (goto-char 1)
           (loop with modes = nil
                 while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 for mode = (match-string 1)
                 unless (member mode modes)
                 collect mode into modes
                 finally return modes))))
    (action
     ("Change Resolution"
      . (lambda (mode)
          (call-process "xrandr" nil nil nil
                        "--screen" (helm-c-xrandr-screen)
                        "--output" (helm-c-xrandr-output)
                        "--mode" mode))))))

;;; Xfont selection
;;
;;
(defun helm-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar helm-c-xfonts-cache nil)
(defvar helm-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless helm-c-xfonts-cache
                (setq helm-c-xfonts-cache
                      (x-list-fonts "*")))))
    (candidates . helm-c-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . helm-c-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar helm-c-ucs-max-len 0)
(defun helm-c-calculate-ucs-max-len ()
  "Calculate the length of longest `ucs-names' candidate."
  (loop with count = 0
        for (n . v) in (ucs-names)
        for len = (length n)
        if (> len count)
        do (setq count len)
        finally return count))

(defun helm-c-ucs-init ()
  "Initialize an helm buffer with ucs symbols.
Only math* symbols are collected."
  (unless (> helm-c-ucs-max-len 0)
    (setq helm-c-ucs-max-len
          (helm-c-calculate-ucs-max-len)))
  (with-current-buffer (helm-candidate-buffer
                        (get-buffer-create "*helm ucs*"))
    ;; `ucs-names' fn will not run again, data is cached in
    ;; var `ucs-names'.
    (loop for (n . v) in (ucs-names)
          for len = (length n)
          for diff = (+ (- helm-c-ucs-max-len len) 2)
          unless (string= "" n)
          do (progn (insert (concat
                             n ":"
                             (make-string
                              diff ? )))
                    (ucs-insert v)
                    (insert "\n")))))

(defun helm-c-ucs-forward-char (candidate)
  (with-helm-current-buffer
    (forward-char 1)))

(defun helm-c-ucs-backward-char (candidate)
  (with-helm-current-buffer
    (forward-char -1)))

(defun helm-c-ucs-delete-backward (candidate)
  (with-helm-current-buffer
    (delete-char -1)))

(defun helm-c-ucs-insert-char (candidate)
  (with-helm-current-buffer
    (insert
     (replace-regexp-in-string
      " " ""
      (cadr (split-string candidate ":"))))))

(defun helm-c-ucs-persistent-insert ()
  (interactive)
  (helm-attrset 'action-insert 'helm-c-ucs-insert-char)
  (helm-execute-persistent-action 'action-insert))

(defun helm-c-ucs-persistent-forward ()
  (interactive)
  (helm-attrset 'action-forward 'helm-c-ucs-forward-char)
  (helm-execute-persistent-action 'action-forward))

(defun helm-c-ucs-persistent-backward ()
  (interactive)
  (helm-attrset 'action-back 'helm-c-ucs-backward-char)
  (helm-execute-persistent-action 'action-back))

(defun helm-c-ucs-persistent-delete ()
  (interactive)
  (helm-attrset 'action-delete 'helm-c-ucs-delete-backward)
  (helm-execute-persistent-action 'action-delete))

(defvar helm-c-source-ucs
  '((name . "Ucs names")
    (init . helm-c-ucs-init)
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (mode-line . helm-c-ucs-mode-line-string)
    (action . (("Insert" . helm-c-ucs-insert-char)
               ("Forward char" . helm-c-ucs-forward-char)
               ("Backward char" . helm-c-ucs-backward-char)
               ("Delete char backward" . helm-c-ucs-delete-backward))))
  "Source for collecting `ucs-names' math symbols.")


;;; Emacs process
;;
;;
(defvar helm-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))

;;; World time
;;
;;
(defvar helm-c-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (let ((helm-buffer (helm-candidate-buffer 'global)))
                (with-current-buffer helm-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)))



;;; Helm interface for Debian/Ubuntu packages (apt-*)
;;
;;
(defvar helm-c-source-apt
  '((name . "APT")
    (init . helm-c-apt-init)
    (candidates-in-buffer)
    (candidate-transformer helm-c-apt-candidate-transformer)
    (display-to-real . helm-c-apt-display-to-real)
    (requires-pattern . 2)
    (update . helm-c-apt-refresh)
    (action
     ("Show package description" . helm-c-apt-cache-show)
     ("Install package" . helm-c-apt-install)
     ("Reinstall package" . helm-c-apt-reinstall)
     ("Remove package" . helm-c-apt-uninstall)
     ("Purge package" . helm-c-apt-purge))
    (persistent-action . helm-c-apt-persistent-action)
    (persistent-help . "Show package description")))

(defvar helm-c-apt-query "emacs")
(defvar helm-c-apt-search-command "apt-cache search '%s'")
(defvar helm-c-apt-show-command "apt-cache show '%s'")
(defvar helm-c-apt-installed-packages nil)
(defvar helm-c-apt-all-packages nil)
(defvar helm-c-apt-input-history nil)

(defun helm-c-apt-refresh ()
  "Refresh installed candidates list."
  (setq helm-c-apt-installed-packages nil)
  (setq helm-c-apt-all-packages nil))

(defun helm-c-apt-persistent-action (candidate)
  "Persistent action for APT source."
  (helm-c-apt-cache-show candidate))

(defun helm-c-apt-candidate-transformer (candidates)
  "Show installed CANDIDATES and the ones to deinstall in a different color."
  (loop for cand in candidates
        for name = (helm-c-apt-display-to-real cand)
        collect (cond ((string= (assoc-default
                                 name helm-c-apt-installed-packages)
                                "deinstall")
                       (propertize cand 'face 'helm-apt-deinstalled))
                      ((string= (assoc-default
                                 name helm-c-apt-installed-packages)
                                "install")
                       (propertize cand 'face 'helm-apt-installed))
                      (t cand))))

(defun helm-c-apt-init ()
  "Initialize list of debian packages."
  (let ((query ""))
    (unless (and helm-c-apt-installed-packages
                 helm-c-apt-all-packages)
      (message "Loading package list...")
      (setq helm-c-apt-installed-packages
            (with-temp-buffer
              (call-process-shell-command "dpkg --get-selections"
                                          nil (current-buffer))
              (loop for i in (split-string (buffer-string) "\n" t)
                    for p = (split-string i)
                    collect (cons (car p) (cadr p)))))
      (setq helm-c-apt-all-packages
            (with-current-buffer
                (helm-candidate-buffer
                 (get-buffer-create (format "*helm-apt*")))
              (erase-buffer)
              (call-process-shell-command
               (format helm-c-apt-search-command query)
               nil (current-buffer))))
      (message "Loading package list done")
      (sit-for 0.5))))

(defun helm-c-apt-display-to-real (line)
  "Return only name of a debian package.
LINE is displayed like:
package name - description."
  (car (split-string line " - ")))

(defun helm-c-shell-command-if-needed (command)
  "Run shell command COMMAND to describe package.
If a buffer named COMMAND already exists, just switch to it."
  (let ((buf (get-buffer command)))
    (helm-c-switch-to-buffer (get-buffer-create command))
    (unless buf (insert (shell-command-to-string command)))))

(defun helm-c-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (helm-c-shell-command-if-needed
   (format helm-c-apt-show-command package)))

(defun helm-c-apt-install (package)
  "Run 'apt-get install' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'install))

(defun helm-c-apt-reinstall (package)
  "Run 'apt-get install --reinstall' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'reinstall))

(defun helm-c-apt-uninstall (package)
  "Run 'apt-get remove' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'uninstall))

(defun helm-c-apt-purge (package)
  "Run 'apt-get purge' shell command on PACKAGE."
  (helm-c-apt-generic-action :action 'purge))

(defun* helm-c-apt-generic-action (&key action)
  "Run 'apt-get ACTION'.
Support install, remove and purge actions."
  (ansi-term (getenv "SHELL") "helm apt")
  (term-line-mode)
  (let ((command   (case action
                     ('install   "sudo apt-get install ")
                     ('reinstall "sudo apt-get install --reinstall ")
                     ('uninstall "sudo apt-get remove ")
                     ('purge     "sudo apt-get purge ")
                     (t          (error "Unknow action"))))
        (beg       (point))
        end
        (cand-list (mapconcat #'(lambda (x) (format "'%s'" x))
                              (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command cand-list))
    (setq end (point))
    (if (y-or-n-p (format "%s package" (symbol-name action)))
        (progn
          (setq helm-c-external-commands-list nil)
          (setq helm-c-apt-installed-packages nil)
          (term-char-mode) (term-send-input))
        (delete-region beg end) (term-send-eof) (kill-buffer))))

;; (helm-c-apt-install "jed")


;;; Helm UI for gentoo portage.
;;
;;
(defvar helm-c-gentoo-use-flags nil)
(defvar helm-c-gentoo-buffer "*helm-gentoo-output*")
(defvar helm-c-cache-gentoo nil)
(defvar helm-c-cache-world nil)
(defvar helm-c-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create helm-c-gentoo-buffer)
              (unless helm-c-cache-gentoo
                (helm-c-gentoo-setup-cache))
              (unless helm-c-cache-world
                (setq helm-c-cache-world (helm-c-gentoo-get-world)))
              (helm-c-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-c-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (helm-c-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm helm-c-cache-world)
                                       (helm-c-gentoo-eshell-action elm "genlop -qe")
                                       (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (let ((urls (helm-c-gentoo-get-url elm)))
                                        (browse-url (helm-comp-read "Url: " urls :must-match t)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm helm-c-cache-world)
                                           (helm-c-gentoo-eshell-action elm "genlop -qi")
                                           (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (helm-c-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (helm-c-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (helm-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (helm-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (helm-c-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (helm-c-gentoo-default-action elm "equery" "files")))
               ("Refresh" . (lambda (elm)
                              (helm-c-gentoo-setup-cache)
                              (setq helm-c-cache-world (helm-c-gentoo-get-world))))))))


(defun* helm-gentoo-install (candidate &key action)
  (setq helm-c-external-commands-list nil)
  (ansi-term (getenv "SHELL") "Gentoo emerge")
  (term-line-mode)
  (let ((command (case action
                   ('install "sudo emerge -av ")
                   ('uninstall "sudo emerge -avC ")
                   (t (error "Unknow action"))))
        (elms (mapconcat 'identity (helm-marked-candidates) " "))
        (beg (point)) end)
    (goto-char (point-max))
    (insert (concat command elms))
    (setq end (point))
    (term-char-mode) (term-send-input)))

(defun helm-c-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `helm-c-gentoo-buffer'."
  (if (member elm helm-c-cache-world)
      (progn
        (helm-c-switch-to-buffer helm-c-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
      (message "No infos on packages not yet installed")))

(defvar helm-c-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless helm-c-gentoo-use-flags
                (helm-c-gentoo-setup-use-flags-cache))
              (helm-c-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer helm-c-highlight-local-use)
    (action . (("Description"
                . (lambda (elm)
                    (helm-c-switch-to-buffer helm-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))
               ("Enable"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -E")))
               ("Disable"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -D")))
               ("Remove"
                . (lambda (elm)
                    (helm-c-gentoo-eshell-action elm "*sudo -p Password: euse -P")))
               ("Show which dep use this flag"
                . (lambda (elm)
                    (helm-c-switch-to-buffer helm-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))))))



(defun helm-c-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*helm-gentoo*"))
         (buf (helm-candidate-buffer 'portage-buf)))
    (with-current-buffer buf
      (dolist (i helm-c-cache-gentoo)
        (insert (concat i "\n"))))))

(defun helm-c-gentoo-setup-cache ()
  "Set up `helm-c-cache-gentoo'"
  (setq helm-c-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun helm-c-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name helm-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
        (eshell-command (format "%s %s" command elm)))))

(defun helm-c-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*helm-gentoo-use*"))
         (buf (helm-candidate-buffer 'use-buf)))
    (with-current-buffer buf
      (dolist (i helm-c-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun helm-c-gentoo-setup-use-flags-cache ()
  "Setup `helm-c-gentoo-use-flags'"
  (setq helm-c-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun helm-c-gentoo-get-url (elm)
  "Return a list of urls from eix output."
  (loop
        with url-list = (split-string
                         (with-temp-buffer
                           (call-process "eix" nil t nil
                                         elm "--format" "<homepage>\n")
                           (buffer-string)))
        with all
        for i in url-list
        when (and (string-match "^http://.*" i)
                  (not (member i all)))
        collect i into all
        finally return all))

(defun helm-c-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun helm-c-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))


(defun helm-c-highlight-world (eix)
  "Highlight all installed package."
  (loop for i in eix
        if (member i helm-c-cache-world)
        collect (propertize i 'face 'helm-gentoo-match-face)
        else
        collect i))

(defun helm-c-highlight-local-use (use-flags)
  (let ((local-uses (helm-c-gentoo-get-local-use)))
    (loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'helm-gentoo-match-face)
          else
          collect i)))



;;; Helm ratpoison UI
;;
;;
(defvar helm-c-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . helm-c-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-c-ratpoison-commands-execute))
    (display-to-real . helm-c-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

(defun helm-c-ratpoison-commands-init ()
  (unless (helm-candidate-buffer)
    (with-current-buffer (helm-candidate-buffer 'global)
      ;; with ratpoison prefix key
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "<ratpoison> \\1: \\2"))
      (goto-char (point-max))
      ;; direct binding
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help top"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "\\1: \\2")))))

(defun helm-c-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-c-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))


;;; Eshell completion.
;;
;; Enable like this in .emacs:
;;
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))
;;
(defvar helm-c-source-esh
  '((name . "Eshell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the helm one. (Fixed in Emacs24)
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates . helm-esh-get-candidates)
    (action . helm-ec-insert))
  "Helm source for Eshell completion.")

;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing at point."
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (insert (helm-quote-whitespace candidate)))

(defun helm-esh-get-candidates ()
  "Get candidates for eshell completion using `pcomplete'."
  (catch 'pcompleted
    (let* ((pcomplete-stub)
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list))
      (with-helm-current-buffer
        (loop with table  = (pcomplete-completions)
              with entry  = (condition-case nil
                                ;; On Emacs24 `try-completion' return
                                ;; pattern when more than one result.
                                ;; Otherwise Emacs23 return nil, which
                                ;; is wrong, in this case use pattern
                                ;; to behave like Emacs24.
                                (or (try-completion helm-pattern
                                                    (pcomplete-entries))
                                    helm-pattern)
                              ;; In Emacs23 `pcomplete-entries' may fail
                              ;; with error, so try this instead.
                              (error
                               nil
                               (let ((fc (car (last
                                               (pcomplete-parse-arguments)))))
                                 ;; Check if last arg require fname completion.
                                 (and (file-name-directory fc) fc))))
              for i in (all-completions pcomplete-stub table)
              for file-cand = (and entry
                                   (if (file-remote-p i) i
                                       (expand-file-name
                                        i (file-name-directory entry))))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand)))
              collect file-cand into ls
              else collect i into ls
              finally return
              (if (and entry (not (string= entry "")) (file-exists-p entry))
                  (append (list (expand-file-name entry default-directory)) ls)
                  ls))))))

;;; Eshell history.
;;
;;
(defvar helm-c-source-eshell-history
  `((name . "Eshell history")
    (init . (lambda ()
              (let (eshell-hist-ignoredups)
                ;; Write the content's of ring to file.
                (eshell-write-history eshell-history-file-name t)
                (with-current-buffer (helm-candidate-buffer 'global)
                  (insert-file-contents eshell-history-file-name)))
              ;; Same comment as in `helm-c-source-esh'
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates-in-buffer)
    (keymap . ,helm-eshell-history-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (eshell-kill-input)
                (insert candidate))))
  "Helm source for Eshell history.")


;;; Run Externals commands within Emacs with helm completion
;;
;;
(defvar helm-external-command-history nil)

(defun helm-c-external-commands-list-1 (&optional sort)
  "Returns a list of all external commands the user can execute.
If `helm-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `helm-c-external-commands-list'."
  (if helm-c-external-commands-list
      helm-c-external-commands-list
      (setq helm-c-external-commands-list
            (loop
                  with paths = (split-string (getenv "PATH") path-separator)
                  with completions = ()
                  for dir in paths
                  when (and (file-exists-p dir) (file-accessible-directory-p dir))
                  for lsdir = (loop for i in (directory-files dir t)
                                    for bn = (file-name-nondirectory i)
                                    when (and (not (member bn completions))
                                              (not (file-directory-p i))
                                              (file-executable-p i))
                                    collect bn)
                  append lsdir into completions
                  finally return (if sort (sort completions 'string-lessp) completions)))))

(defun helm-run-or-raise (exe &optional file)
  "Generic command that run asynchronously EXE.
If EXE is already running just jump to his window if `helm-raise-command'
is non--nil.
When FILE argument is provided run EXE with FILE.
In this case EXE must be provided as \"EXE %s\"."
  (lexical-let* ((real-com (car (split-string (replace-regexp-in-string
                                               "%s" "" exe))))
                 (proc     (if file (concat real-com " " file) real-com)))
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command real-com))
            (error "Error: %s is already running" real-com))
        (when (loop for i in helm-c-external-commands-list thereis real-com)
          (message "Starting %s..." real-com)
          (if file
              (start-process-shell-command
               proc nil (format exe (shell-quote-argument
                                     (if (eq system-type 'windows-nt)
                                         (helm-w32-prepare-filename file)
                                         file))))
              (start-process-shell-command proc nil real-com))
          (set-process-sentinel
           (get-process proc)
           #'(lambda (process event)
               (when (and (string= event "finished\n")
                          helm-raise-command
                          (not (helm-c-get-pid-from-process-name real-com)))
                 (shell-command  (format helm-raise-command "emacs")))
               (message "%s process...Finished." process))))
        (setq helm-c-external-commands-list
              (cons real-com
                    (delete real-com helm-c-external-commands-list))))))



;;; Generic action functions
;;
;;
(defun helm-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun helm-delete-marked-bookmarks (ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (helm-marked-candidates))
    (bookmark-delete (helm-bookmark-get-bookmark-from-name i)
                     'batch)))

(helm-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(helm-document-attribute 'before-jump-hook "type . file-line / line"
  "Function to call before jumping to the target location.")
(helm-document-attribute 'after-jump-hook "type . file-line / line"
  "Function to call after jumping to the target location.")
(helm-document-attribute 'adjust "type . file-line"
  "Search around line matching line contents.")
(helm-document-attribute 'recenter "type . file-line / line"
  "`recenter' after jumping.")
(helm-document-attribute 'target-file "type . line"
  "Goto line of target-file.")

(defun helm-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-c-stringify cmd-or-name)
              (delete (helm-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-c-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
        (setq this-command cmd)
        (call-interactively cmd))))

;;;###autoload
(defun helm-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (helm-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (helm-c-set-variable 'hh)


;;; Actions Transformers
;;
;;
;;; Files
(defun helm-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (member (file-name-extension candidate) '("el" "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
      actions))

(defun helm-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file or URL.  Else return ACTIONS unmodified."
  (let ((browse-action '("Browse with Browser" . browse-url)))
    (cond ((string-match "^http\\|^ftp" candidate)
           (cons browse-action actions))
          ((string-match "\\.html?$" candidate)
           (append actions (list browse-action)))
          (t actions))))

;;; Function
(defun helm-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
      (append actions '(("Call Interactively"
                         .
                         helm-c-call-interactively)))
      actions))

;;;; S-Expressions
(defun helm-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
      actions))


;;; Functions
(defun helm-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern-soft function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))


;;; Adaptive Sorting of Candidates
;;
;;
;; Internal
(defvar helm-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar helm-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

;; Should run at beginning of `helm-initial-setup'.
(add-hook 'helm-before-initialize-hook #'(lambda ()
                                           (when helm-c-use-adaptative-sorting
                                             (setq helm-c-adaptive-done nil))))

;; Should run at beginning of `helm-exit-minibuffer'.
(add-hook 'helm-before-action-hook #'(lambda ()
                                       (when helm-c-use-adaptative-sorting
                                         (helm-c-adaptive-store-selection))))

;; Should run at beginning of `helm-select-action'.
(add-hook 'helm-select-action-hook #'(lambda ()
                                       (when helm-c-use-adaptative-sorting
                                         (helm-c-adaptive-store-selection))))

(defun helm-c-source-use-adaptative-p (&optional source-name)
  "Return current source only if it use adaptative history, nil otherwise."
  (when helm-c-use-adaptative-sorting
    (let* ((source (or source-name (helm-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (when (member 'helm-c-adaptive-sort adapt-source) source)
          (when (eq adapt-source 'helm-c-adaptive-sort) source)))))

(defun helm-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless helm-c-adaptive-done
    (setq helm-c-adaptive-done t)
    (let ((source (helm-c-source-use-adaptative-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name helm-c-adaptive-history)
                                (progn
                                  (push (list source-name) helm-c-adaptive-history)
                                  (car helm-c-adaptive-history))))
               (selection (helm-get-selection))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)

                                                ;; move entry to the beginning of the
                                                ;; list, so that it doesn't get
                                                ;; trimmed when the history is
                                                ;; truncated
                                                (setcdr source-info
                                                        (delete found (cdr source-info)))
                                                found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc helm-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons helm-pattern 0)

                                              ;; move entry to the beginning of the
                                              ;; list, so if two patterns used the
                                              ;; same number of times then the one
                                              ;; used last appears first in the list
                                              (setcdr selection-info
                                                      (delete found (cdr selection-info)))
                                              found))
                                        (cdr selection-info)))
                               (cadr selection-info))))

          ;; increase usage count
          (setcdr pattern-info (1+ (cdr pattern-info)))

          ;; truncate history if needed
          (if (> (length (cdr selection-info)) helm-c-adaptive-history-length)
              (setcdr selection-info
                      (subseq (cdr selection-info) 0 helm-c-adaptive-history-length))))))))

(defun helm-c-adaptative-maybe-load-history ()
  (when (and helm-c-use-adaptative-sorting
             (file-readable-p helm-c-adaptive-history-file))
    (load-file helm-c-adaptive-history-file)))

(add-hook 'emacs-startup-hook 'helm-c-adaptative-maybe-load-history)
(add-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

(defun helm-c-adaptive-save-history (&optional arg)
  "Save history information to file given by `helm-c-adaptive-history-file'."
  (interactive "p")
  (when helm-c-use-adaptative-sorting
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; History entries used for helm adaptive display.\n")
      (prin1 `(setq helm-c-adaptive-history ',helm-c-adaptive-history)
             (current-buffer))
      (insert ?\n)
      (write-region (point-min) (point-max) helm-c-adaptive-history-file nil
                    (unless arg 'quiet)))))

(defun helm-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`helm-sources' or a type in `helm-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name helm-c-adaptive-history)))
    (if source-info
        (let ((usage
               ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
               ;; pairs
               (mapcar (lambda (candidate-info)
                         (let ((count 0))
                           (dolist (pattern-info (cdr candidate-info))
                             (if (not (equal (car pattern-info)
                                             helm-pattern))
                                 (incf count (cdr pattern-info))

                                 ;; if current pattern is equal to the previously
                                 ;; used one then this candidate has priority
                                 ;; (that's why its count is boosted by 10000) and
                                 ;; it only has to compete with other candidates
                                 ;; which were also selected with the same pattern
                                 (setq count (+ 10000 (cdr pattern-info)))
                                 (return)))
                           (cons (car candidate-info) count)))
                       (cdr source-info)))
              sorted)
          (if (and usage (consp usage))
              ;; sort the list in descending order, so candidates with highest
              ;; priorty come first
              (progn
                (setq usage (sort usage (lambda (first second)
                                          (> (cdr first) (cdr second)))))

                ;; put those candidates first which have the highest usage count
                (dolist (info usage)
                  (when (member* (car info) candidates
                                 :test 'helm-c-adaptive-compare)
                    (push (car info) sorted)
                    (setq candidates (remove* (car info) candidates
                                              :test 'helm-c-adaptive-compare))))

                ;; and append the rest
                (append (reverse sorted) candidates nil))
              (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `helm-c-reset-adaptative-history'"
                       helm-c-adaptive-history-file)
              (sit-for 1)
              candidates))
        ;; if there is no information stored for this source then do nothing
        candidates)))

;;;###autoload
(defun helm-c-reset-adaptative-history ()
  "Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `helm-c-adaptive-history'? ")
    (setq helm-c-adaptive-history nil)
    (delete-file helm-c-adaptive-history-file)))

(defun helm-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
             x)
         (if (listp y)
             (cdr y)
             y)))



;;; Outliner
;;
;;
(defvar helm-outline-goto-near-line-flag t)
(defvar helm-outline-using nil)
(defun helm-after-update-hook--outline ()
  (if (and (eq helm-outline-using t)
           (eq helm-outline-goto-near-line-flag t))
      (helm-outline-goto-near-line)))
(add-hook 'helm-after-update-hook 'helm-after-update-hook--outline)

(defun helm-outline-goto-near-line ()
  (with-helm-window
    ;; TODO need consideration whether to update position by every input.
    (when t ; (equal helm-pattern "")
      (helm-goto-line 2)
      (let ((lineno (with-helm-current-buffer
                      (line-number-at-pos (car helm-current-position)))))
        (block exit
          (while (<= (progn (skip-chars-forward " ")
                            (or (number-at-point) lineno))
                     lineno)
            (forward-line 1)
            (when (eobp)
              (forward-line -1)
              (return-from exit))))
        (forward-line -1)
        (and (bobp) (forward-line 1))
        (and (helm-pos-header-line-p) (forward-line -2))
        (helm-mark-current-line)))))



;;; Plug-in
;;
;;
;; Plug-in: info-index
(defun* helm-c-info-init (&optional (file (helm-attr 'info-file)))
  (let (result)
    (unless (helm-candidate-buffer)
      (save-window-excursion
        (info file)
        (let (Info-history
              (tobuf (helm-candidate-buffer 'global))
              (infobuf (current-buffer))
              s e)
          (dolist (node (or (helm-attr 'index-nodes) (Info-index-nodes)))
            (Info-goto-node node)
            (goto-char (point-min))
            (while (search-forward "\n* " nil t)
              (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
                '(save-current-buffer (buffer-substring-no-properties (point-at-bol) (point-at-eol)) result)
                (setq s (point-at-bol)
                      e (point-at-eol))
                (with-current-buffer tobuf
                  (insert-buffer-substring infobuf s e)
                  (insert "\n"))))))))))

(defun helm-c-info-goto (node-line)
  (Info-goto-node (car node-line))
  (helm-goto-line (cdr node-line)))

(defun helm-c-info-display-to-real (line)
  (and (string-match
        ;; This regexp is stolen from Info-apropos-matches
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (helm-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

(defun helm-c-make-info-source (source file)
  `(,@source
    (name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . helm-c-info-init)
    (display-to-real . helm-c-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . helm-c-info-goto))))

(defun helm-compile-source--info-index (source)
  (helm-aif (helm-interpret-value (assoc-default 'info-index source))
      (helm-c-make-info-source source it)
    source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--info-index)

(helm-document-attribute 'info-index "info-index plugin"
  "Create a source of info index very easily.

ex. (defvar helm-c-source-info-wget '((info-index . \"wget\"))")

(helm-document-attribute 'index-nodes "info-index plugin (optional)"
  "Index nodes of info file.

If it is omitted, `Info-index-nodes' is used to collect index nodes.
Some info files are missing index specification.

ex. See `helm-c-source-info-screen'.")

;; Plug-in: candidates-file
(defun helm-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init helm-p-candidats-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                     (cond ((null orig-init) nil)
                           ((functionp orig-init) (list orig-init))
                           (t orig-init))))
        (candidates-in-buffer)
        ,@source)
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--candidates-file)

(defun helm-p-candidats-file-init ()
  (destructuring-bind (file &optional updating)
      (helm-mklist (helm-attr 'candidates-file))
    (setq file (helm-interpret-value file))
    (with-current-buffer (helm-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(helm-document-attribute 'candidates-file "candidates-file plugin"
  "Use a file as the candidates buffer.

1st argument is a filename, string or function name or variable name.
If optional 2nd argument is non-nil, the file opened with `auto-revert-mode'.")

;; Plug-in: headline
(defun helm-compile-source--helm-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . helm-headline-init)
                (get-line . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)
                (persistent-help . "Show this line")))
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--helm-headline)

(defun helm-headline-init ()
  (when (and (helm-current-buffer-is-modified)
             (with-helm-current-buffer
               (eval (or (helm-attr 'condition) t))))
    (helm-headline-make-candidate-buffer
     (helm-interpret-value (helm-attr 'headline))
     (helm-interpret-value (helm-attr 'subexp)))))

(helm-document-attribute 'headline "Headline plug-in"
  "Regexp string for helm-headline to scan.")
(helm-document-attribute 'condition "Headline plug-in"
  "A sexp representing the condition to use helm-headline.")
(helm-document-attribute 'subexp "Headline plug-in"
  "Display (match-string-no-properties subexp).")

;; Le Wang: Note on how `helm-head-line-get-candidates' works with a list
;; of regexps.
;;
;;   1. Create list of ((title . start-of-match) . hiearchy)
;;   2. Sort this list by start-of-match.
;;   3. Go through sorted list and return titles that reflect full hiearchy.
;;
;; It's quite brilliantly written.
;;


(defun helm-headline-get-candidates (regexp subexp)
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (if (functionp regexp) (setq regexp (funcall regexp)))
      (let (hierarchy curhead)
        (flet ((matched ()
                 (if (numberp subexp)
                     (cons (match-string-no-properties subexp) (match-beginning subexp))
                     (cons (buffer-substring (point-at-bol) (point-at-eol))
                           (point-at-bol))))
               (hierarchies (headlines)
                 (1+ (loop for (_ . hierarchy) in headlines
                           maximize hierarchy)))
               (vector-0-n (v n)
                 (loop for i from 0 to hierarchy
                       collecting (aref curhead i)))
               (arrange (headlines)
                 (unless (null headlines) ; FIX headlines empty bug!
                   (loop with curhead = (make-vector (hierarchies headlines) "")
                         for ((str . pt) . hierarchy) in headlines
                         do (aset curhead hierarchy str)
                         collecting
                         (cons
                          (format "H%d:%s" (1+ hierarchy)
                                  (mapconcat 'identity (vector-0-n curhead hierarchy) " / "))
                          pt)))))
          (if (listp regexp)
              (arrange
               (sort
                (loop for re in regexp
                      for hierarchy from 0
                      do (goto-char (point-min))
                      appending
                      (loop
                            while (re-search-forward re nil t)
                            collect (cons (matched) hierarchy)))
                (lambda (a b) (> (cdar b) (cdar a)))))
              (loop while (re-search-forward regexp nil t)
                    collect (matched))))))))


(defun helm-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (helm-candidate-buffer 'local)
    (loop for (content . pos) in (helm-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-helm-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun helm-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window helm-current-buffer) (point))))


;; Plug-in: persistent-help
(defun helm-compile-source--persistent-help (source)
  (append source '((header-line . helm-persistent-help-string))))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--persistent-help)

(defun helm-persistent-help-string ()
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (or (helm-interpret-value (helm-attr 'persistent-help))
               (helm-aif (or (assoc-default 'persistent-action
                                            (helm-get-current-source))
                             (assoc-default 'action
                                            (helm-get-current-source)))
                   (cond ((symbolp it) (symbol-name it))
                         ((listp it) (or (ignore-errors (caar it))  ""))))
               "")
           " (keeping session)")))

(helm-document-attribute 'persistent-help "persistent-help plug-in"
  "A string to explain persistent-action of this source.
It also accepts a function or a variable name.")

;;; (helm '(((name . "persistent-help test")(candidates "a")(persistent-help . "TEST"))))

;; Plug-in: Type customize
(defun helm-c-uniq-list (lst)
  "Like `remove-duplicates' in CL.
But cut deeper duplicates and test by `equal'. "
  (reverse (remove-duplicates (reverse lst) :test 'equal)))
(defvar helm-additional-type-attributes nil)
(defun helm-c-arrange-type-attribute (type spec)
  "Override type attributes by `define-helm-type-attribute'.

The SPEC is like source. The symbol `REST' is replaced
with original attribute value.

 Example: Set `play-sound-file' as default action
   (helm-c-arrange-type-attribute 'file
      '((action (\"Play sound\" . play-sound-file)
         REST ;; Rest of actions (find-file, find-file-other-window, etc...)."
  (add-to-list 'helm-additional-type-attributes
               (cons type
                     (loop with typeattr = (assoc-default
                                            type helm-type-attributes)
                           for (attr . value) in spec
                           if (listp value)
                           collect (cons attr
                                         (helm-c-uniq-list
                                          (loop for v in value
                                                if (eq v 'REST)
                                                append
                                                (assoc-default attr typeattr)
                                                else
                                                collect v)))
                           else
                           collect (cons attr value)))))
(put 'helm-c-arrange-type-attribute 'lisp-indent-function 1)

(defun helm-compile-source--type-customize (source)
  (helm-aif (assoc-default (assoc-default 'type source)
                           helm-additional-type-attributes)
      (append it source)
    source))
(add-to-list 'helm-compile-source-functions
             'helm-compile-source--type-customize t)

;; Plug-in: default-action
(defun helm-compile-source--default-action (source)
  (helm-aif (assoc-default 'default-action source)
      (append `((action ,it ,@(remove it (assoc-default 'action source))))
              source)
    source))
(add-to-list 'helm-compile-source-functions
             'helm-compile-source--default-action t)
(helm-document-attribute 'default-action "default-action plug-in"
  "Default action.")


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

(define-helm-type-attribute 'sexp
    '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
       ("Add s-expression to kill ring" . kill-new))
      (action-transformer helm-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

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


;;; Preconfigured Helm
;;
;;
;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-recentf
                       helm-c-source-buffer-not-found)
                     "*helm mini*"))
;;;###autoload
(defun helm-for-files ()
  "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-prefered-list'."
  (interactive)
  (helm-other-buffer helm-for-files-prefered-list "*helm for files*"))

;;;###autoload
(defun helm-recentf ()
  "Preconfigured `helm' for `recentf'."
  (interactive)
  (helm-other-buffer 'helm-c-source-recentf "*helm recentf*"))

;;;###autoload
(defun helm-info-at-point (arg)
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive "P")
  (let ((helm-c-google-suggest-default-function
         'helm-c-google-suggest-emacs-lisp))
    (helm :sources '(helm-c-source-info-elisp
                     helm-c-source-info-cl
                     helm-c-source-info-pages
                     helm-c-source-google-suggest)
          :input (and arg (thing-at-point 'symbol))
          :buffer "*helm info*")))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-c-source-minibuffer-history
                       "*helm minibuffer-history*")))

;;;###autoload
(defun helm-gentoo ()
  "Preconfigured `helm' for gentoo linux."
  (interactive)
  (helm-other-buffer '(helm-c-source-gentoo
                       helm-c-source-use-flags)
                     "*helm gentoo*"))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (helm :sources 'helm-c-source-imenu
        :buffer "*helm imenu*"))

;;;###autoload
(defun helm-bbdb ()
  "Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/"
  (interactive)
  (helm-other-buffer 'helm-c-source-bbdb "*helm bbdb*"))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG \(C-u\).
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg))

;;;###autoload
(defun helm-w3m-bookmarks ()
  "Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/"
  (interactive)
  (helm-other-buffer 'helm-c-source-w3m-bookmarks
                     "*helm w3m bookmarks*"))

;;;###autoload
(defun helm-firefox-bookmarks ()
  "Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value \
to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.
"
  (interactive)
  (helm-other-buffer 'helm-c-source-firefox-bookmarks
                     "*Helm Firefox*"))

;;;###autoload
(defun helm-colors ()
  "Preconfigured `helm' for color."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-colors helm-c-source-customize-face)
   "*helm colors*"))

;;;###autoload
(defun helm-bookmarks ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm-other-buffer 'helm-c-source-bookmarks "*helm bookmarks*"))

;;;###autoload
(defun helm-c-pp-bookmarks ()
  "Preconfigured `helm' for bookmarks (pretty-printed)."
  (interactive)
  (helm-other-buffer '(helm-c-source-bookmarks-local
                       helm-c-source-bookmarks-su
                       helm-c-source-bookmarks-ssh)
                     "*helm pp bookmarks*"))

;;;###autoload
(defun helm-c-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-c-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-man-woman ()
  "Preconfigured `helm' for Man and Woman pages."
  (interactive)
  (helm-other-buffer 'helm-c-source-man-pages "*Helm man woman*"))

;;;###autoload
(defun helm-org-keywords ()
  "Preconfigured `helm' for org keywords."
  (interactive)
  (helm-other-buffer 'helm-c-source-org-keywords "*org keywords*"))

;;;###autoload
(defun helm-emms ()
  "Preconfigured `helm' for emms sources."
  (interactive)
  (helm :sources '(helm-c-source-emms-streams
                   helm-c-source-emms-files
                   helm-c-source-emms-dired)
        :buffer "*Helm Emms*"))

;;;###autoload
(defun helm-eev-anchors ()
  "Preconfigured `helm' for eev anchors."
  (interactive)
  (helm-other-buffer 'helm-c-source-eev-anchor "*Helm eev anchors*"))

;;;###autoload
(defun helm-bm-list ()
  "Preconfigured `helm' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el"
  (interactive)
  (let ((helm-outline-using t))
    (helm-other-buffer 'helm-c-source-bm "*helm bm list*")))

;;;###autoload
(defun helm-list-emacs-process ()
  "Preconfigured `helm' for emacs process."
  (interactive)
  (helm-other-buffer 'helm-c-source-emacs-process "*helm process*"))

;;;###autoload
(defun helm-browse-code ()
  "Preconfigured helm to browse code."
  (interactive)
  (helm :sources 'helm-c-source-browse-code
        :buffer "*helm browse code*"
        :default (thing-at-point 'symbol)))

;;;###autoload
(defun helm-org-headlines ()
  "Preconfigured helm to show org headlines."
  (interactive)
  (helm-other-buffer 'helm-c-source-org-headline "*org headlines*"))

;;;###autoload
(defun helm-c-etags-select (arg)
  "Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache."
  (interactive "P")
  (let ((tag  (helm-c-etags-get-tag-file))
        (init (and (equal arg '(4)) (thing-at-point 'symbol)))
        (helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (helm-compile-source-functions
         (if helm-c-etags-use-regexp-search
             ;; rule out helm-match-plugin because the input is one regexp.
             (delq 'helm-compile-source--match-plugin
                   (copy-sequence helm-compile-source-functions))
             helm-compile-source-functions)))
    (when (or (equal arg '(16))
              (and helm-c-etags-mtime-alist
                   (helm-c-etags-file-modified-p tag)))
      (remhash tag helm-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-c-source-etags-select
              :keymap helm-c-etags-map
              :input init
              :buffer "*helm etags*")
        (message "Error: No tag file found, please create one with etags shell command."))))

;;;###autoload
(defun helm-bookmark-ext ()
  "Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-c-source-google-suggest'."
  (interactive)
  (helm
   :sources
   '(helm-c-source-bookmark-files&dirs
     helm-c-source-bookmark-w3m
     helm-c-source-google-suggest
     helm-c-source-bmkext-addressbook
     helm-c-source-bookmark-gnus
     helm-c-source-bookmark-info
     helm-c-source-bookmark-man
     helm-c-source-bookmark-images
     helm-c-source-bookmark-su-files&dirs
     helm-c-source-bookmark-ssh-files&dirs)
   :prompt "SearchBookmark: "
   :buffer "*helm bmkext*"))

;;;###autoload
(defun helm-simple-call-tree ()
  "Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el"
  (interactive)
  (helm-other-buffer
   '(helm-c-source-simple-call-tree-functions-callers
     helm-c-source-simple-call-tree-callers-functions)
   "*helm simple-call-tree*"))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-edit-or-view ()
  "Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-c-source-yaoddmuse-emacswiki-edit-or-view))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-post-library ()
  "Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-c-source-yaoddmuse-emacswiki-post-library))

;;;###autoload
(defun helm-call-source ()
  "Preconfigured `helm' to call helm source."
  (interactive)
  (helm :sources 'helm-c-source-call-source
        :buffer helm-source-select-buffer))

;;;###autoload
(defun helm-execute-helm-command ()
  "Preconfigured `helm' to execute preconfigured `helm'."
  (interactive)
  (helm-other-buffer 'helm-c-source-helm-commands
                     "*helm commands*"))

;;;###autoload
(defun helm-create (&optional string initial-input)
  "Preconfigured `helm' to do many create actions from STRING.
See also `helm-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Helm: " initial-input)))
  (helm :sources '(((name . "Helm Create")
                    (header-name . (lambda (_) (format "Action for \"%s\"" string)))
                    (candidates . helm-create--actions)
                    (candidate-number-limit)
                    (action . (lambda (func) (funcall func string)))))))

;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (let ((helm-samewindow t)
        (helm-enable-shortcuts)
        (helm-display-function 'helm-default-display-buffer)
        (helm-candidate-number-limit 9999))
    (save-window-excursion
      (delete-other-windows)
      (helm-other-buffer 'helm-c-source-top "*helm top*"))))

;;;###autoload
(defun helm-select-xfont ()
  "Preconfigured `helm' to select Xfont."
  (interactive)
  (helm-other-buffer 'helm-c-source-xfonts "*helm select* xfont"))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time."
  (interactive)
  (helm-other-buffer 'helm-c-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-apt (arg)
  "Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache."
  (interactive "P")
  (let ((query (read-string "Search Package: " nil 'helm-c-apt-input-history)))
    (when arg (helm-c-apt-refresh))
    (helm :sources 'helm-c-source-apt
          :prompt "Search Package: "
          :input query
          :history 'helm-c-apt-input-history)))

;;;###autoload
(defun helm-esh-pcomplete ()
  "Preconfigured helm to provide helm completion in eshell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (target (thing-at-point 'symbol))
         (end (point))
         (beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point)))))
    (setq helm-ec-target (or target " "))
    (with-helm-show-completion beg end
      (helm :sources 'helm-c-source-esh
            :buffer "*helm pcomplete*"
            :input (helm-ff-set-pattern ; Handle tramp filenames.
                    (car (last (ignore-errors ; Needed in lisp symbols completion.
                                 (pcomplete-parse-arguments)))))))))

;;;###autoload
(defun helm-eshell-history ()
  "Preconfigured helm for eshell history."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
         (with-helm-show-completion beg end
           (helm :sources 'helm-c-source-eshell-history
                 :buffer "*Eshell history*"
                 :input input))
      (when (and flag-empty
                 (looking-back " "))
        (delete-char -1)))))

;;;###autoload
(defun helm-c-run-external-command (program)
  "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-c-external-commands-list'."
  (interactive (list
                (helm-comp-read
                 "RunProgram: "
                 (helm-c-external-commands-list-1 'sort)
                 :must-match t
                 :name "External Commands"
                 :history helm-external-command-history)))
  (helm-run-or-raise program)
  (setq helm-external-command-history
        (cons program (delete program
                              (loop for i in helm-external-command-history
                                    when (executable-find i) collect i)))))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-c-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-ucs ()
  "Preconfigured helm for `ucs-names' math symbols."
  (interactive)
  (helm :sources 'helm-c-source-ucs
        :keymap  helm-c-ucs-map))

;;;###autoload
(defun helm-xrandr-set ()
  (interactive)
  (helm :sources 'helm-c-source-xrandr-change-resolution
        :buffer "*helm xrandr*"))


;;; Unit tests are now in ../developer-tools/unit-test-helm-config.el.

(provide 'helm-config)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-config.el ends here
