;;; helm-help.el --- Help messages for Helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
\\[helm-buffers-run-browse-project]\t\tBrowse Project from buffer.
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

**** RET behavior

Behave differently depending of `helm-selection' (current candidate in helm-buffer):

- candidate basename is \".\"   => open it in dired.
- candidate is a directory    => expand it.
- candidate is a file         => open it.
- marked candidates (1+)      => open them with default action.

Note that when copying, renaming etc... from `helm-find-files' you
will have a file completion with `helm-read-file-name' to select the
destination file; To not confuse users of `read-file-name' or
`read-directory-name' RET behave normally, it exit the minibuffer as
soon as you press RET, if you want the same behavior as in
`helm-find-files', bind `helm-ff-RET' to the `helm-read-file-map':

    (define-key helm-read-file-map (kbd \"RET\") 'helm-ff-RET)

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

NOTE: This is different from using `C-l' in that `C-l' doesn't move cursor on top but stays on previous
subdir name.

**** Enter `..name/' at end of pattern start a recursive search of directories matching name under
your current directory, see below the \"Recursive completion on subdirectories\" section for more infos.

**** Enter any environment var (e.g. `$HOME') at end of pattern, it will be expanded

**** You can yank any valid filename after pattern, it will be expanded

**** Special case with url's at point

This have no effect at end of an url, you have first to kill pattern (`C-k')
before entering one of these quick expansions patterns.

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

**** You can start a recursive search with Locate or Find (See commands below)

With Locate you can use a local db with a prefix arg. If the localdb doesn't already
exists, you will be prompted for its creation, if it exists and you want to refresh it,
give two prefix args.

Note that when using locate the helm-buffer is empty until you type something,
but helm use by default the basename of pattern entered in your helm-find-files session,
hitting M-n should just kick in the locate search with this pattern.
If you want to automatically do this add the `helm-source-locate'
to `helm-sources-using-default-as-input'.

**** Recursive completion on subdirectories

Starting from the current directory you are browsing, it is possible
to have completion of all directories under here.
So if you are at \"/home/you/foo/\" and you want to go to \"/home/you/foo/bar/baz/somewhere/else\"
just type \"/home/you/foo/..else\" and hit `C-j' or enter the final \"/\", helm will show you all
possibles directories under \"foo\" matching \"else\".
\(Note that entering two spaces before \"else\" instead of two dots works also).

NOTE: Completion on subdirectories use locate as backend, you can configure
the command with `helm-locate-recursive-dirs-command'.
Because this completion use an index, you may not have all the recent additions
of directories until you update your index (with `updatedb' for locate).

If for some reason you cannot use an index the find command from findutils can be
used for this, it will be slower of course, you will have to pass the basedir as
first argument of find and the subdir as the value for '-(i)regex' or '-(i)name'
with the two format specs that are mandatory in `helm-locate-recursive-dirs-command',
e.g \"find %s -type d -name '*%s*'\" or \"find %s -type d -regex .*%s.*$\".

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

Note that when copying recursively files, you may have files with same name
dispatched in the different subdirectories, so when copying them in the same directory
they would be overwrited. To avoid this helm have a special action called \"backup files\"
that have the same behavior as the command line \"cp --backup=numbered\", it allows you
copying for example many *.jpg files with the same name from different
subdirectories in one directory.
Files with same name are renamed like this: \"foo.txt.~1~\".
NOTE: This command is available only when `dired-async-mode' is used.

NOTE: When using an action that involve an external backend (e.g. grep), using \"**\"
is not advised (even if it works fine) because it will be slower to select all your files,
you have better time letting the backend doing it, it will be faster.
However, if you know you have not many files it is reasonable to use this,
also using not recursive wilcard (e.g. \"*.el\") is perfectly fine for this.

This feature (\"**\") is activated by default with the option `helm-file-globstar'.
It is different than the bash shopt globstar feature in that to list files with a named extension
recursively you just have to specify e.g \"**.el\" whereas in bash you have to specify \"**/*.el\"
which is not convenient as \"**.el\".
The directory selection with \"**/\" like bash shopt globstar option is not supported yet.

*** Query replace regexp on filenames

WARNING: This is designed to work ONLY in current directory, i.e
         your marked files have to be from the same directory.
         So do not mark files in different directories, [[Using wildcard to select multiple files][recursive globbing]] e.g \"**.txt\"
         is not supported as well for same reasons. 

You can rename your marked files by replacing only part of filenames matching
a regexp.

e.g Rename recursively all files with \".JPG\" extension to \".jpg\":
Use the helm-file-globstar feature described in previous section by
entering at end of helm-find-files pattern \"**.JPG\", then hit `M-%`,
at first prompt enter \"JPG\", at second \"jpg\" and hit `RET`.

Shortcut for basename without extension, only extension or all are available:

- Basename without extension => \"%.\"
- Only extension             => \".%\"
- All                        => \"%\"

So in the example above you could do instead:
At first prompt enter \".%\", at second \"jpg\" and hit `RET`.
Note that when using this instead of using \"JPG\" at first prompt, all extensions
will be renamed to \"jpg\" even if the extension of one of the files is e.g \"png\".

If you want to rename a serie of files from number 001 to 00x use \\# inside the replacement
string when you will be prompted for it.

e.g To rename the files \"foo.jpg\" \"bar.jpg\" and \"baz.jpg\"
    to \"foo-001.jpg\" \"foo-002.jpg\" \"foo-003.jpg\"

Use as replace regexp \"%.\" and as replacement string \"foo-\\#\".

When \"%\", \".%\" or \"%\" are used, \"\\@\" can be used as a placeholder which
remember those values.

e.g To rename the files \"foo.jpg\" \"bar.jpg\" and \"baz.jpg\"
    to \"foo-001.jpg\" \"bar-002.jpg\" \"baz-003.jpg\"

Use as replace regexp \"%.\" and as replacement string \"\\@-\\#\".

Modifying the placeholder (\\@) is possible
\(in contrast of renaming the whole placeholder with something else) with two methods:

- By substring, i.e using only the substring of placeholder:
    \\@:<from>:<to>
  e.g \\@:0:2 replaces from beginning to second char of placeholder
  Note that length of placeholder is used for <to> when <to> is not specified
  e.g \\@:2: replaces from second char of placeholder to end

- By search and replace:
    \\@/<regexp>/<replacement>
  e.g \\@/foo/bar replaces \"foo\" in placeholder by \"bar\"
  Incremental replacement is also handled in <replacement>
  e.g \\@/foo/-\\# replaces \"foo\" in placeholder by 001, 002 etc...

In the second prompt (replace regexp with) shortcut for `upcase', `downcase' and `capitalize'
are available, respectively `%u', `%d' and `%c'.

Note also that unlike the [[Serial renaming][serial rename]] actions the renamed files stay in their initial directory
and are not renamed to current directory, IOW use this (\\#) to rename files inside the same directory.

*** Serial renaming

You can use the serial rename actions to rename, copy or symlink marked files to
a specific directory or in the current one with all your files numbered incrementally.

- Serial rename by renaming
Rename all marked files with incremental number to a specific directory.
- Serial rename by copying
Copy all marked files with incremental number to a specific directory.
-Serial rename by symlinking
Symlink all marked files with incremental number to a specific directory.

*** Edit marked files in a dired buffer

You can open a dired buffer with only marked files with `\\<helm-find-files-map>\\[helm-ff-run-marked-files-in-dired]'
With a prefix arg you can open this same dired buffer in wdired mode for editing files.
Note that wildcards are supported as well, so you can use e.g \"*.txt\" to select all \".txt\" files
in current directory or \"**.txt\" to select all files recursively from current directory
\(See [[Using wildcard to select multiple files]] section above).

*** Copying renaming asynchronously

If you use async library (if you have installed helm from MELPA you do) you can enable
async for copying/renaming etc... your files by enabling `dired-async-mode'.

Note that even when async is enabled, running a copy/rename action with a prefix arg
will execute action synchronously, it will follow also the first file of the marked files
in its destination directory.

When `dired-async-mode' is enabled and additional action named \"Backup files\" will be
available (such command is not available in emacs natively).
See [[Using wildcard to select multiple files]] for details.

*** Bookmark your `helm-find-files' session

You can bookmark your `helm-find-files' session with `C-x r m'.
You can retrieve later these bookmarks easily by using M-x helm-filtered-bookmarks
or from the current `helm-find-files' session just hitting `C-x r b'.

*** Grep files from `helm-find-files'

You can grep individual files from `helm-find-files' by using
\`\\<helm-find-files-map>\\[helm-ff-run-grep]'.  This same command can
grep also recursively files from current directory when called with a
prefix arg, you will be prompted in this case for the file extensions
to use (grep backend) or the types of files to use (ack-grep backend),
see the `helm-grep-default-command' documentation to setup this.
For compressed files or archives, use zgrep with
\`\\<helm-find-files-map>\\[helm-ff-run-zgrep]'.

Otherwise you can use other recursive commands like
\`\\<helm-find-files-map>\\[helm-ff-run-grep-ag]' or `\\<helm-find-files-map>\\[helm-ff-run-git-grep]' that are much more
faster than using `\\<helm-find-files-map>\\[helm-ff-run-grep]' with a
prefix arg.  See `helm-grep-ag-command' and
`helm-grep-git-grep-command' to setup this.

You can also use the gid shell command
\`\\<helm-find-files-map>\\[helm-ff-run-gid]' from id-utils by creating
an ID index file with the `mkid' shell command coming with the
id-utils package.

All these grep commands are using symbol at point as default pattern.
Note that default is a different thing than input (nothing is added to
prompt until you hit `M-n').

**** Grepping on remote files
On remote files grep is not well supported by tramp unless you suspend update before
entering your pattern and reenable it once your pattern is ready.
To toggle suspend update use \\<helm-map>\\[helm-toggle-suspend-update].

*** Setting up aliases in eshell allows you to setup powerful customized commands

Adding eshell aliases to your `eshell-aliases-file' or using the
`alias' command from eshell allows you to create personalized commands
not available in `helm-find-files' actions and use them from `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.
Example:
You want a command to uncompress your \"*.tar.gz\" files from `helm-find-files':

1) Create an alias named untargz (or whatever) in eshell with the
command \"alias untargz tar zxvf $*\"

2) Now from `helm-find-files' select your \"*.tar.gz\" file (you can
mark files if needed) and hit `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

Note:

When using marked files with this, the meaning of prefix arg is quite
subtil: Say you have foo, bar and baz marked, when you run the alias
command `example' on these files with no prefix arg it will loop on
the file list and run sequentially `example' on each file:

    example foo
    example bar
    example baz

However with a prefix arg it will apply `example' on each file:

    example foo bar baz

Of course the alias command should support this.

*** Using Tramp with `helm-find-files' to read remote directories

`helm-find-files' is working fine with tramp with however some limitations.

- By default filenames are not highlighted when working on remote directories,
this is controled by `helm-ff-tramp-not-fancy' variable, if you change this,
expect helm becoming very slow unless your connection is super fast.

- Grepping files is not very well supported when used incrementally,
see above [[Grepping on remote files]].

- Locate is not working on remote directories.

**** Some reminders about Tramp syntax

Not exhaustive, please read Tramp documentation.

- Connect to host 192.168.0.4 as foo user:

    /scp:192.168.0.4@foo:

- Connect to host 192.168.0.4 as foo user with port 2222:

    /scp:192.168.0.4@foo#2222:

- Connect to host 192.168.0.4 as root using multihops syntax:

    /ssh:192.168.0.4@foo|sudo:192.168.0.4:

Note: you can also use `tramp-default-proxies-alist' when connecting often to
some hosts.

Prefer generally scp method unless using multihops (works only with ssh method)
specially when copying large files.

Note also that you have to hit once `C-j' on top of directory at first connection
to complete your pattern in minibuffer.

**** Completing host

As soon as you enter the first \":\" after method e.g =/scp:\= you will
have some completion about previously used hosts or from your =~/.ssh/config\=
file, hitting `C-j' or `right' on a candidate will insert this host in minibuffer
without addind the ending \":\".
As soon the last \":\" is entered Tramp will kick in and you should see the list
of candidates a few seconds later.

When your connection fails, be sure to delete your tramp connection before retrying
with M-x `helm-delete-tramp-connection'.

**** Editing local files as root

Use the sudo method:

    /sudo:host: or just /sudo::

*** Attach files to a mail buffer (message-mode)

If you are in a `message-mode' or `mail-mode' buffer, the action will
appear in action menu, otherwise it is available at any time with
\\<helm-find-files-map>\\[helm-ff-run-mail-attach-files] Here how it
behave:
- If you are in a (mail or message) buffer, files are attached
there.
- If you are not in a mail buffer but one or more mail buffer
exists, you are prompted to add attached file to one of these mail
buffer.
- If you are not in a mail buffer and no mail buffer exists,
a new mail buffer is created with tha attached files in it.

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

When you enable fuzzy matching on locate with
`helm-locate-fuzzy-match', the search will be performed on basename
only for efficiency (so don't add \"-b\" at prompt), as soon as you
separate your patterns with spaces, fuzzy matching will be disabled
and search will be done on the full filename.  Note that in multimatch
fuzzy is completely disabled, which mean that each pattern should be a
compliant regexp matching pattern (i.e \"helm\" will match \"helm\"
but \"hlm\" will NOT match \"helm\").

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
However now that helm support git-grep and AG, you have better time
using one of those for your recursives search.

*** You can use wild card when selecting files (e.g. *.el)

*** You can grep in many differents directories by marking files or wild cards

*** You can save your results in a `helm-grep-mode' buffer, see commands below

Once in this buffer you can use emacs-wgrep (external package not bundled with helm)
to edit your changes.

*** Helm grep is supporting multi matching starting from version 1.9.4.
Just add a space between each pattern like in most helm commands.

*** Important

Grepping works but it is badly supported as tramp doesn't support multiple process running in a
short delay (less than 5s actually) among other things.

Helm is suspending process automatically while you are typing with a special hook, however
you are adviced doing this manually by hitting `C-!' (i.e suspend process)
before entering anything in pattern, and hit again `C-!' when
your regexp is ready to send to remote process, even if helm is handling
this by delaying each process at 5s.

If your regexp is simple enough, you can though try merely to type it directly.

Another solution is to not use tramp at all and mount your remote file system on SSHFS.

* Helm Gid

** Tips

Helm gid read the database created with the `mkid' command from id-utils.
The name of the database file can be customized with `helm-gid-db-file-name', it
is usually \"ID\".
Helm Gid use the symbol at point as default-input.
You have access to this command also from `helm-find-files' which allow you to
navigate to another directory to consult its database.

NOTE: Helm gid support multi matches but only the last pattern entered will be
highlighted due to the lack of ~--color~ support in GID itself.

* Helm AG

** Tips

Helm AG is different from grep or ack-grep in that it works on a directory and not
a list of files.
You can ignore files and directories by using a \".agignore\" file, local to directory
or global when placed in home directory (See AG man page for more infos).
This file supports same entries as what you will find in `helm-grep-ignored-files' and
`helm-grep-ignored-directories'.
As always you can access helm AG from `helm-find-files'.

Starting at version 0.30 AG allow providing one or more TYPE argument on its command line.
Helm provide completion on these TYPES arguments when available with your AG version,
Use a prefix argument when starting helm ag session to get this completion.
NOTE: You can mark several types to match in your ag query, however on the first versions of
AG providing this, only one type was allowed, so in this case the last marked will take effect.

* Helm git-grep

Helm git-grep is searching from current directory
(i.e default-directory or the directory currently browsed by helm-find-files).
If this current directory is a subdirectory of project and you want to match
also upper directories (i.e the whole project) use a prefix arg.

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
\\[helm-el-package-show-built-in]\t\tShow built-in packages only.
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

When you want pass prefix args, you should pass prefix args AFTER starting `helm-M-x',
you will see a prefix arg counter appearing in mode-line notifying you
the number of prefix args entered.

If you pass prefix args before running `helm-M-x', it will be displayed in prompt,
then the first C-u after `helm-M-x' will be used to clear that prefix args.")

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

;;; Kill-ring
;;
;;
(defvar helm-kill-ring-help-message
  "* Helm kill-ring

** Tips

You can bring any candidate on top of kill-ring by using `\\<helm-map>\\[helm-kill-selection-and-quit]'
on it, contrarily to the regular helm command `helm-kill-selection-and-quit'
which is bound in helm to same key, it is here killing the real value
without the need of prefix argument.

The view of truncated candidates can be toggled, see command list below.

You can bind globally `M-y' to `helm-show-kill-ring' and once in the helm kill-ring session
you can navigate for conveniency to next/previous line with `M-y' and `M-u'.
Of course `C-n' and `C-p' are still available.

It is possible to delete unwanted candidates from kill-ring.

You can concatenate marked candidates and yank them in current buffer
creating a new entry in kill-ring.
Note: You can insert marked candidates as well with `\\<helm-map>\\[helm-copy-to-buffer]'
but this will not push a new entry with concatenated candidates in kill-ring.

** Commands
\\<helm-kill-ring-map>
\\[helm-next-line]\t\tNext line.
\\[helm-previous-line]\t\tPrevious line.
\\[helm-kill-ring-delete]\t\tDelete entry.
\\[helm-kill-ring-run-append]\t\tYank concatenated marked candidates.
\\[helm-kill-ring-toggle-truncated]\t\tToggle truncated view of candidate.
\\[helm-kill-ring-kill-selection]\t\tKill real value of selection.")

;;; Org headings
;;
;;
(defvar helm-org-headings-help-message
  "* Helm org headings

** Tips

*** Refiling

The heading to refile will be the one you were at when starting helm
session, and the place to refile this heading will be the selected
candidate (i.e the candidate at point in helm buffer). If you want to
refile another one, move to it in helm buffer, mark it, then move to
the candidate of your choice to refile at this place.
NOTE that of course if you have marked more than one candidate,
all the subsequent candidates will be ignored.

** Commands
\\<helm-org-headings-map>
\\[helm-org-run-open-heading-in-indirect-buffer]\t\tOpen heading in indirect buffer.
\\[helm-org-run-heading-refile]\t\tRefile current heading to selection.
\\[helm-org-run-insert-link-to-heading-at-marker]\t\tInsert link at point to selection."
  )

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


(provide 'helm-help)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-help.el ends here
