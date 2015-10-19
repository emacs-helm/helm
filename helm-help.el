;;; helm-help.el --- Help messages for Helm. -*- lexical-binding: t -*-

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
(require 'helm)

(defvar helm-org-headings--nofilename)
(declare-function helm-source-org-headings-for-files "helm-org.el")


(defgroup helm-help nil
  "Embedded help for `helm'."
  :group 'helm)

(defface helm-helper
    '((t :inherit helm-header))
  "Face for helm help string in minibuffer."
  :group 'helm-help)

(defcustom helm-documentation-file "~/.emacs.d/helm-doc.org"
  "The file where you want to save helm documentation."
  :group 'helm-help
  :type 'string)

(defvar helm-help--string-list '(helm-help-message
                                 helm-buffer-help-message
                                 helm-ff-help-message
                                 helm-read-file-name-help-message
                                 helm-generic-file-help-message
                                 helm-grep-help-message
                                 helm-pdfgrep-help-message
                                 helm-etags-help-message
                                 helm-ucs-help-message
                                 helm-bookmark-help-message
                                 helm-esh-help-message
                                 helm-buffers-ido-virtual-help-message
                                 helm-moccur-help-message
                                 helm-top-help-message
                                 helm-apt-help-message
                                 helm-el-package-help-message
                                 helm-M-x-help-message
                                 helm-imenu-help-message
                                 helm-colors-help-message
                                 helm-semantic-help-message
                                 helm-kmacro-help-message))


;;;###autoload
(defun helm-documentation (arg)
  "Preconfigured helm for helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all sources actually documented."
  (interactive "P")
  (require 'helm-org)
  (when arg (delete-file helm-documentation-file)
        (helm-aif (get-file-buffer helm-documentation-file)
            (kill-buffer it)))
  (unless (file-exists-p helm-documentation-file)
    (with-temp-file helm-documentation-file
      (erase-buffer)
      (cl-loop for elm in helm-help--string-list
               for str = (symbol-value elm)
               do (insert (substitute-command-keys
                           (if (functionp str) (funcall str) str))
                          "\n\n"))))
  (let ((helm-org-headings--nofilename t))
    (helm :sources (helm-source-org-headings-for-files
                    (list helm-documentation-file))
          :candidate-number-limit 99999
          :buffer "*helm documentation*")))

;;; Local help messages.

;;; `helm-buffer-list' help
;;
;;
(defvar helm-buffer-help-message
  "* Helm Buffer

** Tips

*** Completion

**** Major-mode

You can enter a partial name of major-mode (e.g. lisp, sh) to narrow down buffers.
To specify the major-mode, prefix it with \"*\" e.g. \"*lisp\".
If you want to match all buffers but the ones with a specific major-mode (negation),
prefix the major-mode with \"!\" e.g. \"*!lisp\".
If you want to specify more than one major-mode, separate them with \",\",
e.g. \"*!lisp,!sh,!fun\" will list all buffers but the ones in lisp-mode, sh-mode and
fundamental-mode.

Enter then a space and a pattern to narrow down to buffers matching this pattern.

**** Search inside buffers

If you enter a space and a pattern prefixed by \"@\" helm will search for text matching
this pattern INSIDE the buffer (i.e not in the name of buffer).
NOTE that if you enter your pattern prefixed with \"@\" but escaped, helm will search a buffer
matching \"@pattern\" but will not search inside.

**** Search by directory name

If you prefix the beginning of pattern with \"/\" the match will occur on directory name
of buffer, it is interesting to narrow down to one directory for example, subsequent string
entered after a space will match on buffer-name only.
Note that negation is not supported for matching on buffer-file-name.
You can specify more than one directory starting from helm v1.6.8
 
**** Fuzzy matching

Note that if `helm-buffers-fuzzy-matching' is non--nil you will have
fuzzy matching on buffer names (not on directory name matching and major-mode though).
A pattern starting with \"^\" will disable fuzzy matching and will match by exact regexp.

**** Examples

if I enter in pattern prompt:

    \"*lisp ^helm @moc\"

helm will narrow down the list by selecting only buffers that are in lisp mode, start by helm
and match \"moc\" in their contents.

if I enter in pattern prompt:

    \"*lisp ^helm moc\"

Notice there is no \"@\" this time
helm will look for lisp mode buffers starting by \"helm\" and have \"moc\" in their name.

if I enter in pattern prompt:

    \"*!lisp !helm\"

helm will narrow down to buffers that are not in \"lisp\" mode and that do not match \"helm\"

if I enter in pattern prompt:

    /helm/ w3

helm will narrow down to buffers that are in any \"helm\" subdirectory and matching w3.

*** Creating buffers

When creating a new buffer use \\[universal-argument] to choose a mode for your buffer in a list.
This list is customizable, see `helm-buffers-favorite-modes'.

*** Killing buffers

You have a command to kill buffer(s) and quit emacs and a command to kill buffers one by one
\(no marked\) without quitting helm.

You can run this persistent kill buffer command either with the regular
`helm-execute-persistent-action' called with a prefix arg (C-u C-j) or with its specific command
`helm-buffer-run-kill-persistent' see binding below.

*** Meaning of colors and prefixes for buffers

Remote buffers are prefixed with '@'.
Red        => Buffer have its file modified on disk by an external process.
Indianred2 => Buffer exists but its file have been deleted.
Orange     => Buffer is modified and its file not saved to disk.
Italic     => A non--file buffer.

** Commands
\\<helm-buffer-map>
\\[helm-buffer-run-zgrep]\t\tGrep Buffer(s) works as zgrep too (C-u grep all buffers but non--file buffers).
\\[helm-buffers-run-multi-occur]\t\tMulti Occur buffer or marked buffers. (C-u toggle force searching current-buffer).
\\[helm-buffer-switch-other-window]\t\tSwitch other window.
\\[helm-buffer-switch-other-frame]\t\tSwitch other frame.
\\[helm-buffer-run-query-replace-regexp]\t\tQuery replace regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\tQuery replace in marked buffers.
\\[helm-buffer-run-ediff]\t\tEdiff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\tEdiff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[helm-buffer-diff-persistent]\t\tToggle Diff buffer with saved file without quitting.
\\[helm-buffer-revert-persistent]\t\tRevert buffer without quitting.
\\[helm-buffer-save-persistent]\t\tSave buffer without quitting.
\\[helm-buffer-run-kill-buffers]\t\tDelete marked buffers and quit.
\\[helm-buffer-run-kill-persistent]\t\tDelete buffer without quitting helm.
\\[helm-toggle-all-marks]\t\tToggle all marks.
\\[helm-mark-all]\t\tMark all.
\\[helm-toggle-buffers-details]\t\tToggle details.
\\[helm-buffers-toggle-show-hidden-buffers]\t\tShow hidden buffers.
\\[helm-buffers-mark-similar-buffers]\t\tMark all buffers with same type (color) than current.")

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "* Helm Find Files

** Tips

*** Navigation summary

For a better experience you can enable auto completion by setting
`helm-ff-auto-update-initial-value' to non-nil in your init file.
It is not enabled by default to not confuse new users.

**** Use `C-j' (persistent action) on a directory to go down one level

On a symlinked directory a prefix arg will allow expanding to its true name.

**** Use `C-l' on a directory to go up one level

**** Use `C-r' to walk back the resulting tree of all the `C-l' you did

Note: The tree is reinitialized each time you enter a new tree with `C-j'
or by entering some pattern in prompt.

*** Find file at point

Helm is using `ffap' partially or completely to find file at point
depending on value of `helm-ff-guess-ffap-filenames'.
You can use full `ffap' by setting this to non-nil (annoying).
Default value is nil which make `ffap' working partially.

**** Find file at number line

With something like this at point:

    ~/elisp/helm/helm.el:1234

Helm will find this file at line number 1234.

**** Find url at point

When an url is found at point, helm expand to that url only.
Pressing RET jump to that url using `browse-url-browser-function'.

**** Find mail at point

When a mail address is found at point helm expand to this email address
prefixed by \"mailto:\". Pressing RET open a message buffer with this mail
address.

*** Quick pattern expansion

**** Enter `~/' at end of pattern to quickly reach home directory

**** Enter `/' at end of pattern to quickly reach root of your file system

**** Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session)

If you are already in `default-directory' this will move cursor on top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor on top

NOTE: This is different to using `C-l' in that `C-l' don't move cursor on top but stay on previous
subdir name.

**** Enter any environment var (e.g. `$HOME') at end of pattern, it will be expanded

**** You can yank any valid filename after pattern, it will be expanded

*** Helm find files is fuzzy matching (start on third char entered)

e.g. \"fob\" or \"fbr\" will complete \"foobar\"
but \"fb\" will wait for a third char for completing.

*** Use `C-u C-j' to watch an image or `C-<down>'

*** `C-j' on a filename will expand in helm-buffer to this filename

Second hit on `C-j' will display buffer filename.
Third hit on `C-j' will kill buffer filename.
NOTE: `C-u C-j' will display buffer directly.

*** To browse images directories turn on `helm-follow-mode' and navigate with arrow keys

You can also use `helm-follow-action-forward' and `helm-follow-action-backward'
\(`C-<down' and `C-<left>').

*** You can turn off/on (toggle) autoupdate completion at any moment with `C-DEL'

It is useful when auto completion is enabled and when trying to create a new file
or directory you want to prevent helm trying to complete what you are writing.
NOTE: On a terminal C-<backspace> may not work, use in this case C-c <backspace>.

*** You can create a new directory and a new file at the same time

Just write the path in prompt and press `<RET>'.
e.g. You can create \"~/new/newnew/newnewnew/my_newfile.txt\".

*** To create a new directory, add a \"/\" at end of new name and press <RET>

*** To create a new file just write the filename not ending with \"/\"

*** Recursive search from helm find files

**** You can use helm browse project (see binding below)

- With no prefix arg
  If your current directory is under version control
  with one of git or hg and you have installed helm-ls-git and/or helm-ls-hg
  https://github.com/emacs-helm/helm-ls-git.git
  https://github.com/emacs-helm/helm-ls-hg
  you will see all your files under version control, otherwise
  you will be back to helm-find-files.
- With one prefix arg
  You will see all the files under this directory
  and other subdirectories (recursion) and this list of files will be cached.
- With two prefix args
  same but the cache will be refreshed.

**** You can start a recursive search with Locate of Find (See commands below)

With Locate you can use a local db with a prefix arg. If the localdb doesn't already
exists, you will be prompted for its creation, if it exists and you want to refresh it,
give two prefix args.

*** Insert filename at point or complete filename at point

On insertion (no completion, i.e nothing at point):

- `C-c i'         => insert absolute file name.
- `C-u C-c i'     => insert abbreviate file name.
- `C-u C-u C-c i' => insert relative file name.

On completion:

- target starts by ~/           => insert abbreviate file name.
- target starts by / or [a-z]:/ => insert full path.
- otherwise                     => insert relative file name.

*** Using wildcard to select multiple files

Use of wilcard is supported to give a set of files to an action:

e.g. You can copy all the files with \".el\" extension by using \"*.el\"
and then run your copy action.

You can do the same but with \"**.el\" (note the two stars),
this will select recursively all \".el\" files under current directory.

NOTE: When using an action that involve an external backend (e.g. grep), using \"**\"
is not advised (even if it works fine) because it will be slower to select all your files,
you have better time letting the backend doing it, it will be faster.
However, if you know you have not many files it is reasonable to use this,
also using not recursive wilcard (e.g. \"*.el\") is perfectly fine for this.

This feature (\"**\") is activated by default with the option `helm-file-globstar'.
The directory selection with \"**foo/\" like bash shopt globstar option is not supported yet.

*** Copying renaming asynchronously

If you use async library (if you have installed helm from MELPA you do) you can enable
async for copying/renaming etc... your files by enabling `dired-async-mode'.

Note that even when async is enabled, running a copy/rename action with a prefix arg
will execute action synchronously, it will follow also the first file of the marked files
in its destination directory.

*** Bookmark your `helm-find-files' session

You can bookmark your `helm-find-files' session with `C-x r m'.
You can retrieve later these bookmarks easily by using M-x helm-filtered-bookmarks
or from the current `helm-find-files' session just hitting `C-x r b'.

*** Run Gid from `helm-find-files'

You can navigate to a project containing an ID file created with the `mkid'
command from id-utils, and run the `gid' command which will use the symbol at point
in `helm-current-buffer' as default.

** Commands
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\tRun Locate (C-u to specify locate db, M-n insert basename of candidate)
\\[helm-ff-run-browse-project]\t\tBrowse project (`C-u' recurse, `C-u C-u' recurse and refresh db)
\\[helm-ff-run-find-sh-command]\t\tRun Find shell command from this directory.
\\[helm-ff-run-grep]\t\tRun Grep (C-u Recursive).
\\[helm-ff-run-pdfgrep]\t\tRun Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\tRun zgrep (C-u Recursive).
\\[helm-ff-run-grep-ag]\t\tRun AG grep on current directory.
\\[helm-ff-run-git-grep]\t\tRun git-grep on current directory.
\\[helm-ff-run-gid]\t\tRun gid (id-utils).
\\[helm-ff-run-etags]\t\tRun Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[helm-ff-run-rename-file]\t\tRename File (C-u Follow).
\\[helm-ff-run-query-replace-on-marked]\t\tQuery replace on marked files.
\\[helm-ff-run-copy-file]\t\tCopy File (C-u Follow).
\\[helm-ff-run-byte-compile-file]\t\tByte Compile File (C-u Load).
\\[helm-ff-run-load-file]\t\tLoad File.
\\[helm-ff-run-symlink-file]\t\tSymlink File.
\\[helm-ff-run-hardlink-file]\t\tHardlink file.
\\[helm-ff-run-delete-file]\t\tDelete File.
\\[helm-ff-run-kill-buffer-persistent]\t\tKill buffer candidate without quitting.
\\[helm-ff-persistent-delete]\t\tDelete file without quitting.
\\[helm-ff-run-switch-to-eshell]\t\tSwitch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\tEshell command on file (C-u Apply on marked files, otherwise treat them sequentially).
\\[helm-ff-run-ediff-file]\t\tEdiff file.
\\[helm-ff-run-ediff-merge-file]\t\tEdiff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\tComplete file name at point.
\\[helm-ff-run-switch-other-window]\t\tSwitch other window.
\\[helm-ff-run-switch-other-frame]\t\tSwitch other frame.
\\[helm-ff-run-open-file-externally]\t\tOpen file with external program (C-u to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\tOpen file externally with default tool.
\\[helm-ff-rotate-left-persistent]\t\tRotate Image Left.
\\[helm-ff-rotate-right-persistent]\t\tRotate Image Right.
\\[helm-find-files-up-one-level]\t\tGo down precedent directory.
\\[helm-ff-run-switch-to-history]\t\tSwitch to last visited directories history.
\\[helm-ff-file-name-history]\t\tSwitch to file name history.
\\[helm-ff-properties-persistent]\t\tShow file properties in a tooltip.
\\[helm-mark-all]\t\tMark all visibles candidates.
\\[helm-ff-run-toggle-auto-update]\t\tToggle auto expansion of directories.
\\[helm-unmark-all]\t\tUnmark all candidates, visibles and invisibles.
\\[helm-ff-run-gnus-attach-files]\t\tGnus attach files to message buffer.
\\[helm-ff-run-print-file]\t\tPrint file, (C-u to refresh printers list).
\\[helm-enlarge-window]\t\tEnlarge helm window.
\\[helm-narrow-window]\t\tNarrow helm window.
\\[helm-ff-run-toggle-basename]\t\tToggle basename/fullpath.
\\[helm-ff-run-find-file-as-root]\t\tFind file as root.
\\[helm-ff-run-find-alternate-file]\t\tFind alternate file.
\\[helm-ff-run-insert-org-link]\t\tInsert org link.")

;;; Help for `helm-read-file-name'
;;
;;
(defvar helm-read-file-name-help-message
  "* Helm read file name

** Tips

If you are here, you are probably using a vanilla command like `find-file'
helmized by `helm-mode', this is cool, but it is even better for your file
navigation to use `helm-find-files' which is fully featured.

*** Navigation

**** Enter `~/' at end of pattern to quickly reach home directory

**** Enter `/' at end of pattern to quickly reach root of your file system

**** Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session)

If you are in `default-directory' move cursor on top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor on top

NOTE: This different to using `C-l' in that `C-l' don't move cursor on top but stay on previous
subdir name.

**** You can complete with partial basename (start on third char entered)

E.g. \"fob\" or \"fbr\" will complete \"foobar\"
but \"fb\" will wait for a third char for completing.

*** Persistent actions

By default `helm-read-file-name' use the persistent actions of `helm-find-files'

**** Use `C-u C-j' to watch an image

**** `C-j' on a filename will expand in helm-buffer to this filename

Second hit on `C-j' will display buffer filename.
Third hit on `C-j' will kill buffer filename.
NOTE: `C-u C-j' will display buffer directly.

**** To browse images directories turn on `helm-follow-mode' and navigate with arrow keys

*** Delete characters backward

When you want to delete backward characters, e.g. to create a new file or directory,
autoupdate may keep updating to an existent directory preventing you from doing so.
In this case, type C-<backspace> and then <backspace>.
This should not be needed when copying/renaming files because autoupdate is disabled
by default in that case.
NOTE: On a terminal C-<backspace> may not work, use in this case C-c <backspace>.

*** Create new directory and files

**** Create a new directory and a new file at the same time

You can create a new directory and a new file at the same time, 
just write the path in prompt and press <RET>.
E.g. You can create \"~/new/newnew/newnewnew/my_newfile.txt\".

**** To create a new directory, add a \"/\" at end of new name and press <RET>

**** To create a new file just write the filename not ending with \"/\"

_NOTE_: File and directory creation work only in some commands (e.g `find-file')
and will not work in other commands where it is not intended to return a file or a directory
\(e.g `list-directory').

** Commands
\\<helm-read-file-map>
\\[helm-find-files-up-one-level]\t\tGo down precedent directory.
\\[helm-ff-run-toggle-auto-update]\t\tToggle auto expansion of directories.
\\[helm-ff-run-toggle-basename]\t\tToggle basename.
\\[helm-ff-file-name-history]\t\tFile name history.
C/\\[helm-cr-empty-string]\t\tMaybe return empty string (unless `must-match').
\\[helm-next-source]\t\tGoto next source.
\\[helm-previous-source]\t\tGoto previous source.")

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "* Helm Generic files

** Tips

*** Locate

You can add after writing search pattern any of the locate command line options.
e.g. -b, -e, -n <number>...etc.
See Man locate for more infos.

Some other sources (at the moment recentf and file in current directory sources)
support the -b flag for compatibility with locate when they are used with it.

*** Browse project

When your directory is not under version control,
don't forget to refresh your cache when files have been added/removed in your directory.

*** Find command

Recursively search files using \"find\" shell command.

Candidates are all filenames that match all given globbing patterns.
This respects the options `helm-case-fold-search' and
`helm-findutils-search-full-path'.

You can pass arbitrary options directly to find after a \"*\" separator.
For example, this would find all files matching \"book\" that are larger
than 1 megabyte:

    book * -size +1M

** Commands
\\<helm-generic-files-map>
\\[helm-ff-run-toggle-basename]\t\tToggle basename.
\\[helm-ff-run-grep]\t\tRun grep (C-u recurse).
\\[helm-ff-run-zgrep]\t\tRun zgrep.
\\[helm-ff-run-gid]\t\tRun gid (id-utils).
\\[helm-ff-run-pdfgrep]\t\tRun Pdfgrep on marked files.
\\[helm-ff-run-copy-file]\t\tCopy file(s)
\\[helm-ff-run-rename-file]\t\tRename file(s).
\\[helm-ff-run-symlink-file]\t\tSymlink file(s).
\\[helm-ff-run-hardlink-file]\t\tHardlink file(s).
\\[helm-ff-run-delete-file]\t\tDelete file(s).
\\[helm-ff-run-byte-compile-file]\t\tByte compile file(s) (C-u load) (elisp).
\\[helm-ff-run-load-file]\t\tLoad file(s) (elisp).
\\[helm-ff-run-ediff-file]\t\tEdiff file.
\\[helm-ff-run-ediff-merge-file]\t\tEdiff merge file.
\\[helm-ff-run-switch-other-window]\t\tSwitch other window.
\\[helm-ff-properties-persistent]\t\tShow file properties.
\\[helm-ff-run-etags]\t\tRun etags (C-u use tap, C-u C-u reload DB).
\\[helm-yank-text-at-point]\t\tYank text at point.
\\[helm-ff-run-open-file-externally]\t\tOpen file with external program (C-u to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\tOpen file externally with default tool.
\\[helm-ff-run-insert-org-link]\t\tInsert org link.")

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "* Helm Grep

** Tips

*** You can start grep with a prefix arg to recurse in subdirectories

*** You can use wild card when selecting files (e.g. *.el)

*** You can grep in many differents directories by marking files or wild cards

*** You can save your results in a `helm-grep-mode' buffer, see commands below

Once in this buffer you can use emacs-wgrep to edit your changes.

*** Important

Grepping on remote file will work only with grep, not ack-grep, but it is
anyway bad supported as tramp doesn't support multiple process running in a
short delay (less than 5s actually) among other things,
so I strongly advice hitting `C-!' (i.e suspend process)
before entering anything in pattern, and hit again `C-!' when
your regexp is ready to send to remote process, even if helm is handling
this by delaying each process at 5s. 
Or even better don't use tramp at all and mount your remote file system on SSHFS.

* Helm Gid

** Tips

Helm gid read the database created with the `mkid' command from id-utils.
The name of the database file can be customized with `helm-gid-db-file-name', it
is usually \"ID\".
Helm Gid use the symbol at point as default-input.
You have access to this command also from `helm-find-files' which allow you to
navigate to another directory to consult its database.

* Helm AG

** Tips

Helm AG is different from grep or ack-grep in that it works on a directory and not
a list of files.
You can ignore files and directories by using a \".agignore\" file, local to directory
or global when placed in home directory (See AG man page for more infos).
This file supports same entries as what you will find in `helm-grep-ignored-files' and
`helm-grep-ignored-directories'.
As always you can access helm AG from `helm-find-files'.

** Commands
\\<helm-grep-map>
\\[helm-goto-next-file]\t\tNext File.
\\[helm-goto-precedent-file]\t\tPrecedent File.
\\[helm-yank-text-at-point]\t\tYank Text at point in minibuffer.
\\[helm-grep-run-other-window-action]\t\tJump other window.
\\[helm-grep-run-other-frame-action]\t\tJump other frame.
\\[helm-grep-run-default-action]\t\tRun default action (Same as RET).
\\[helm-grep-run-save-buffer]\t\tSave to a `helm-grep-mode' enabled buffer.")

;;; Pdf grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "* Helm PdfGrep Map

** Commands
\\<helm-pdfgrep-map>
\\[helm-goto-next-file]\t\tNext File.
\\[helm-goto-precedent-file]\t\tPrecedent File.
\\[helm-yank-text-at-point]\t\tYank Text at point in minibuffer.")

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "* Helm Etags Map

** Commands
\\<helm-etags-map>
\\[helm-goto-next-file]\t\tNext File.
\\[helm-goto-precedent-file]\t\tPrecedent File.
\\[helm-yank-text-at-point]\t\tYank Text at point in minibuffer.")

;;; Ucs help
;;
;;
(defvar helm-ucs-help-message
  "* Helm Ucs

** Tips

Use commands below to insert unicode characters
in current-buffer without quitting helm.

** Commands
\\<helm-ucs-map>
\\[helm-ucs-persistent-insert]\t\tInsert char.
\\[helm-ucs-persistent-forward]\t\tForward char.
\\[helm-ucs-persistent-backward]\t\tBackward char.
\\[helm-ucs-persistent-delete]\t\tDelete char backward.")

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "* Helm bookmark name

** Commands
\\<helm-bookmark-map>
\\[helm-bookmark-run-jump-other-window]\t\tJump other window.
\\[helm-bookmark-run-delete]\t\tDelete bookmark.
\\[helm-bookmark-run-edit]\t\tEdit bookmark.
\\[helm-bookmark-toggle-filename]\t\tToggle bookmark location visibility.")

;;; Eshell command on file help
;;
;;
(defvar helm-esh-help-message
  "* Helm eshell on file

** Tips

*** Passing extra args after filename

Normally your command or alias will be called with file as argument. E.g.,

    <command> 'candidate_file'

But you can also pass an argument or more after 'candidate_file' like this:

    <command> %s [extra_args]

'candidate_file' will be added at '%s' and your command will look at this:

    <command> 'candidate_file' [extra_args]

*** Specify many files as args (marked files)

E.g. <command> file1 file2 ...

Call `helm-find-files-eshell-command-on-file' with one prefix-arg
Otherwise you can pass one prefix-arg from the command selection buffer.
NOTE: This is not working on remote files.

With two prefix-arg before starting or from the command selection buffer
the output is printed to your `current-buffer'.

Note that with no prefix-arg or a prefix-arg value of '(16) (C-u C-u)
the command is called once for each file like this:

    <command> file1 <command> file2 etc...

** Commands
\\<helm-esh-on-file-map>")

;;; Ido virtual buffer help
;;
;;
(defvar helm-buffers-ido-virtual-help-message
  "* Helm ido virtual buffers

** Commands
\\<helm-buffers-ido-virtual-map>
\\[helm-ff-run-switch-other-window]\t\tSwitch other window.
\\[helm-ff-run-switch-other-frame]\t\tSwitch other frame.
\\[helm-ff-run-grep]\t\tGrep file.
\\[helm-ff-run-zgrep]\t\tZgrep file.
\\[helm-ff-run-delete-file]\t\tDelete file.
\\[helm-ff-run-open-file-externally]\t\tOpen file externally.")

;;; Moccur help
;;
;;
(defvar helm-moccur-help-message
  "* Helm Moccur

** Tips

*** Matching

Multiple regexp matching is allowed, just enter a space to separate your regexps.

Matching empty lines is supported with the regexp \"^$\", you will get the results
with only the buffer-name and the line number, you can of course save and edit these
results (i.e add text to the empty line) .

*** Automatically matching symbol at point

You can match automatically the symbol at point, but keeping
the minibuffer empty ready to write into.
This is disabled by default, to enable this you have to add `helm-source-occur'
and `helm-source-moccur' to `helm-sources-using-default-as-input'.

*** Jump to the corresponding line in the searched buffer

You can do this with `C-j' (persistent-action), to do it repetitively
you can use `C-<up>' and `C-<down>' or enable `helm-follow-mode' with `C-c C-f'.

*** Saving results

Same as with helm-grep, you can save the results with `C-x C-s'.
Of course if you don't save your results, you can get back your session
with `helm-resume'.

*** Refreshing the resumed session.

When the buffer(s) where you ran helm-(m)occur have been modified, you will be
warned of this with the buffer flashing to red, you can refresh the buffer by running
`C-c C-u'.
This can be done automatically by customizing `helm-moccur-auto-update-on-resume'.

*** Refreshing a saved buffer

Type `g' to update your buffer.

*** Edit a saved buffer

First, install wgrep https://github.com/mhayashi1120/Emacs-wgrep
and then:

1) C-c C-p to edit the buffer(s).
2) C-x C-s to save your changes.

Tip: Use the excellent iedit https://github.com/tsdh/iedit
to modify occurences in your buffer.

** Commands
\\<helm-moccur-map>
\\[helm-goto-next-file]\t\tNext Buffer.
\\[helm-goto-precedent-file]\t\tPrecedent Buffer.
\\[helm-yank-text-at-point]\t\tYank Text at point in minibuffer.
\\[helm-moccur-run-goto-line-ow]\t\tGoto line in other window.
\\[helm-moccur-run-goto-line-of]\t\tGoto line in new frame.")

;;; Helm Top
;;
;;
(defvar helm-top-help-message
  "* Helm Top

** Tips

** Commands
\\<helm-top-map>
\\[helm-top-run-sort-by-com]\t\tSort by commands.
\\[helm-top-run-sort-by-cpu]\t\tSort by cpu usage.
\\[helm-top-run-sort-by-user]\t\tSort alphabetically by user.
\\[helm-top-run-sort-by-mem]\t\tSort by memory.")

;;; Helm Apt
;;
;;
(defvar helm-apt-help-message
  "* Helm Apt

** Tips

** Commands
\\<helm-apt-map>
\\[helm-apt-show-all]\t\tShow all packages.
\\[helm-apt-show-only-installed]\t\tShow installed packages only.
\\[helm-apt-show-only-not-installed]\t\tShow not installed packages only.
\\[helm-apt-show-only-deinstalled]\t\tShow deinstalled (not purged yet) packages only.>")

;;; Helm elisp package
;;
;;
(defvar helm-el-package-help-message
  "* Helm elisp package

** Tips

*** Compile all your packages asynchronously

When using async (if you have installed from MELPA you do), only helm, helm-core,
and magit are compiled asynchronously, if you want all your packages compiled async,
add to your init file:
    
     (setq async-bytecomp-allowed-packages '(all))
    
*** Upgrade elisp packages

On initial start (when emacs is fetching packages on remote), if helm find
package to upgrade it will start in the upgradables packages view showing the packages
availables to upgrade.
On further starts, you will have to refresh the list with `C-c C-u', if helm find upgrades
you will have a message telling you some packages are available for upgrade, you can switch to
upgrade view (see below) to see what packages are available for upgrade or just hit `C-c U'.
to upgrade all.

To see upgradables packages hit <M-U>.

Then you can install all upgradables packages with the upgrade all action (`C-c C-u'),
or upgrade only the specific packages by marking them (the new ones) and running
the upgrade action (visible only when there is upgradables packages).
Of course you can upgrade a single package by just running the upgrade action
without marking it (`C-c u' or RET) .

\*WARNING* You are strongly advised to RESTART emacs after UPGRADING packages.

*** Meaning of flags prefixing packages (Emacs-25)

- The flag \"S\" that prefix package names mean that this package is one of `package-selected-packages'.
This feature is only available with emacs-25.

- The flag \"U\" that prefix package names mean that this package is no more needed.
This feature is only available with emacs-25.

** Commands
\\<helm-el-package-map>
\\[helm-el-package-show-all]\t\tShow all packages.
\\[helm-el-package-show-installed]\t\tShow installed packages only.
\\[helm-el-package-show-uninstalled]\t\tShow not installed packages only.
\\[helm-el-package-show-upgrade]\t\tShow upgradable packages only.
\\[helm-el-run-package-install]\t\tInstall package(s).
\\[helm-el-run-package-reinstall]\t\tReinstall package(s).
\\[helm-el-run-package-uninstall]\t\tUninstall package(s).
\\[helm-el-run-package-upgrade]\t\tUpgrade package(s).
\\[helm-el-run-package-upgrade-all]\t\tUpgrade all packages upgradables.
\\[helm-el-run-visit-homepage]\t\tVisit package homepage.")

;;; Helm M-x
;;
;;
(defvar helm-M-x-help-message
  "* Helm M-x

** Tips

*** You can get help on any command with persistent action (C-j)

*** Prefix Args

All the prefix args passed BEFORE running `helm-M-x' are ignored,
you should get an error message if you do so.
When you want to pass prefix args, pass them AFTER starting `helm-M-x',
you will see a prefix arg counter appearing in mode-line notifying you
the number of prefix args entered.")

;;; helm-imenu
;;
;;
(defvar helm-imenu-help-message
  "* Helm imenu

** Tips

** Commands
\\<helm-imenu-map>
\\[helm-imenu-next-section]\t\tGo to next section.
\\[helm-imenu-previous-section]\t\tGo to previous section.")

;;; helm-colors
;;
;;
(defvar helm-colors-help-message
  "* Helm colors

** Commands
\\<helm-color-map>
\\[helm-color-run-insert-name]\t\tInsert the entry'name.
\\[helm-color-run-kill-name]\t\tKill the entry's name.
\\[helm-color-run-insert-rgb]\t\tInsert entry in RGB format.
\\[helm-color-run-kill-rgb]\t\tKill entry in RGB format.")

;;; helm semantic
;;
;;
(defvar helm-semantic-help-message
  "* Helm semantic

** Tips

** Commands
\\<helm-semantic-map>")

;;; helm kmacro
;;
;;
(defvar helm-kmacro-help-message
  "* Helm kmacro

** Tips

- Start recording some keys with `f3'
- Record new kmacro with `f4'
- Start `helm-execute-kmacro' to list all your macros.

Use persistent action to run your kmacro as many time as needed,
you can change of kmacro with `helm-next-line' `helm-previous-line'.

NOTE: You can't record keys running helm commands except `helm-M-x' unless
you don't choose from there a command using helm completion.

** Commands
\\<helm-kmacro-map>")


;;; Mode line strings
;;
;;
;;;###autoload
(defvar helm-comp-read-mode-line "\
\\<helm-comp-read-map>\
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-read-file-name-mode-line-string "\
\\<helm-read-file-map>\
\\[helm-help]:Help \
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in `helm-source-find-files'.")

;;;###autoload
(defvar helm-top-mode-line "\
\\<helm-top-map>\
\\[helm-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")


;;; Attribute Documentation
;;
;;
;;;###autoload
(defun helm-describe-helm-attribute (helm-attribute)
  "Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol."
  (interactive (list (intern
                      (completing-read
                       "Describe helm attribute: "
                       (mapcar 'symbol-name helm-attributes)
                       nil t))))
  (with-output-to-temp-buffer "*Help*"
    (princ (get helm-attribute 'helm-attrdoc))))

(helm-document-attribute 'name "mandatory"
  "  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.")

(helm-document-attribute 'header-name "optional"
  "  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name.")

(helm-document-attribute 'candidates "mandatory if candidates-in-buffer attribute is not provided"
  "  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Helm buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Helm will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `helm-pattern'.)
  You should use instead `candidates-process' attribute for
  async processes, a warning will popup when using async process
  in a `candidates' attribute.

  Note that currently results from asynchronous sources appear
  last in the helm buffer regardless of their position in
  `helm-sources'.")

(helm-document-attribute 'candidates-process
    "Same as `candidates' attributes but for process function."
  "  You should use this attribute when using a function involving
  an async process instead of `candidates'.")

(helm-document-attribute 'action "mandatory if type attribute is not provided"
  "  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.")

(helm-document-attribute 'coerce "optional"
  "  It's a function called with one argument: the selected
  candidate.

  This function is intended for type convertion. In normal case,
  the selected candidate (string) is passed to action
  function. If coerce function is specified, it is called just
  before action function.

  Example: converting string to symbol
    (coerce . intern)")

(helm-document-attribute 'type "optional if action attribute is provided"
  "  Indicates the type of the items the source returns.

  Merge attributes not specified in the source itself from
  `helm-type-attributes'.

  This attribute is implemented by plug-in.")

(helm-document-attribute 'init "optional"
  "  Function called with no parameters when helm is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  helm does its job in the minibuffer and in the
  `helm-buffer' and the current directory can be different
  there.")

(helm-document-attribute 'match "optional"
  "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `helm-pattern').

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.")

(helm-document-attribute 'candidate-transformer "optional"
  "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

(helm-document-attribute 'filtered-candidate-transformer "optional"
  "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be dislpayed due to the limit
  imposed by `helm-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.")

(helm-document-attribute 'action-transformer "optional"
  "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

(helm-document-attribute 'pattern-transformer "optional"
  "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `helm-pattern'.
  Functions should return transformed `helm-pattern'.

  It is useful to change interpretation of `helm-pattern'.")

(helm-document-attribute 'delayed "optional"
  "  Candidates from the source are shown only if the user stops
  typing and is idle for `helm-idle-delay' seconds.
  If a value is given to delayed attr, this value is used instead only
  if it is > to `helm-idle-delay'.")

(helm-document-attribute 'volatile "optional"
  "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Helm
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

(helm-document-attribute 'requires-pattern "optional"
  "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

(helm-document-attribute 'persistent-action "optional"
  "  Can be a either a Function called with one parameter (the
  selected candidate) or a cons cell where first element is this
  same function and second element a symbol (e.g. never-split)
  that inform `helm-execute-persistent-action'to not split his
  window to execute this persistent action.")

(helm-document-attribute 'candidates-in-buffer "optional"
  "  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `helm-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in.")

(helm-document-attribute 'search "optional"
  "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `re-search-forward'. This attribute is meant to be used with
  (candidates . helm-candidates-in-buffer) or
  (candidates-in-buffer) in short.")

(helm-document-attribute 'get-line "optional"
  "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `buffer-substring-no-properties'.")

(helm-document-attribute 'display-to-real "optional"
  "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.")

(helm-document-attribute 'real-to-display "optional"
  "  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient.

  Note that DISPLAY parts returned from candidates /
  candidate-transformer are IGNORED as the name `display-to-real'
  says.")

(helm-document-attribute 'cleanup "optional"
  "  Function called with no parameters when *helm* buffer is
  closed. It is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

(helm-document-attribute 'candidate-number-limit "optional"
  "  Override `helm-candidate-number-limit' only for this source.")

(helm-document-attribute 'accept-empty "optional"
  "  Pass empty string \"\" to action function.")

(helm-document-attribute 'dummy "optional"
  "  Set `helm-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.")

(helm-document-attribute 'multiline "optional"
  "  Enable to selection multiline candidates.")

(helm-document-attribute 'update "optional"
  (substitute-command-keys
   "  Function called with no parameters at before \"init\" function when \
\\<helm-map>\\[helm-force-update] is pressed."))

(helm-document-attribute 'mode-line "optional"
  "  Source local `helm-mode-line-string' (included in
  `mode-line-format'). It accepts also variable/function name.")

(helm-document-attribute 'header-line "optional"
  "  Source local `header-line-format'.
  It accepts also variable/function name. ")

(helm-document-attribute
    'resume "optional"
  "  Function called with no parameters at end of initialization
  when `helm-resume' is started.
  If this function try to do something against `helm-buffer', \(e.g. updating,
  searching etc...\) probably you should run it in a timer to ensure
  `helm-buffer' is ready.")

(helm-document-attribute 'keymap "optional"
  "  Specific keymap for this source.
  It is useful to have a keymap per source when using more than
  one source.  Otherwise, a keymap can be set per command with
  `helm' argument KEYMAP.  NOTE: when a source have `helm-map' as
  keymap attr, the global value of `helm-map' will override the
  actual local one.")

(helm-document-attribute 'help-message "optional"
  "  Help message for this source.
  If not present, `helm-help-message' value will be used.")

(helm-document-attribute 'match-part "optional"
  "  Allow matching candidate in the line with `candidates-in-buffer'.
  In candidates-in-buffer sources, match is done with
  `re-search-forward' which allow matching only a regexp on the
  `helm-buffer'; when this search is done, match-part allow
  matching only a specific part of the current line e.g. with a
  line like this:

  filename:candidate-containing-the-word-filename

  What you want is to ignore \"filename\" part and match only
  \"candidate-containing-the-word-filename\"

  So give a function matching only the part of candidate after \":\"

  If source contain match-part attribute, match is computed only
  on part of candidate returned by the call of function provided
  by this attribute. The function should have one arg, candidate,
  and return only a specific part of candidate.

  NOTE: This have effect only on sources using
  `candidates-in-buffer'.")

(helm-document-attribute 'match-strict "optional"
  "  When specifying a match function within a source and
  helm-multi-match is enabled, the result of all matching
  functions will be concatened, which in some cases is not what
  is wanted. When using `match-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function. For anonymous
  function don't add the dot, e.g:

  \(match-strict (lambda () (foo))).")

(helm-document-attribute 'nohighlight "optional"
  "  Disable highlight match in this source.")

(helm-document-attribute 'no-matchplugin "optional"
  "  Disable matchplugin for this source.")

(helm-document-attribute 'history "optional"
  "  Allow passing history variable to helm from source.
  It should be a quoted symbol evaluated from source, i.e:
  (history . ,'history-var)")

(helm-document-attribute 'follow "optional"
  "  Enable `helm-follow-mode' for this source only.
  You must give it a value of 1 or -1, though giving a -1 value
  is surely not what you want, e.g: (follow . 1)

  See `helm-follow-mode' for more infos")

(helm-document-attribute 'follow-delay "optional"
  "  `helm-follow-mode' will execute persistent-action after this delay.
  Otherwise value of `helm-follow-input-idle-delay' is used if non--nil,
  If none of these are found fallback to `helm-input-idle-delay'.")

(helm-document-attribute 'allow-dups "optional"
  "  Allow helm collecting duplicates candidates.")

(helm-document-attribute 'filter-one-by-one "optional"
  "  A transformer function that treat candidates one by one.
  It is called with one arg the candidate.
  It is faster than `filtered-candidate-transformer' or `candidates-transformer',
  but should be used only in sources that recompute constantly their candidates,
  e.g. `helm-source-find-files'.
  Filtering happen early and candidates are treated
  one by one instead of re-looping on the whole list.
  If used with `filtered-candidate-transformer' or `candidates-transformer'
  these functions should treat the candidates transformed by the `filter-one-by-one'
  function in consequence.")

(helm-document-attribute 'nomark "optional"
  "  Don't allow marking candidates when this attribute is present.")

(provide 'helm-help)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-help.el ends here
