;;; helm-help.el --- Help messages for Helm.

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
(require 'helm)

;;; Embeded documentation.
;;
;;
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

Single source is executed by \\[helm-call-source].")))

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
  '("Buffer(s)" "\
\\<helm-c-buffer-map>\
\\[helm-c-buffer-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string "\
\\<helm-find-files-map>\
\\[helm-ff-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string "\
\\<helm-c-read-file-map>\
\\[helm-read-file-name-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string "\
\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string"\
\\<helm-c-grep-map>\
\\[helm-grep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string "\
\\<helm-c-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string "\
\\<helm-c-etags-map>\
\\[helm-etags-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-etags-select'.")

(defvar helm-c-ucs-mode-line-string "\
\\<helm-c-ucs-map>\
\\[helm-c-ucs-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)" "\
\\<helm-c-bookmark-map>\
\\[helm-c-bookmark-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-occur-mode-line "\
\\<helm-map>\
\\[helm-help]:Help \
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct")

(provide 'helm-help)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; generated-autoload-file: "helm-config.el"
;; End:

;;; helm-help.el ends here
