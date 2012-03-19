;;; helm-vars.el --- Users variables and faces for helm.

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

;;; Customize
;;
;;
(defgroup helm-config nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defcustom helm-c-adaptive-history-file
  "~/.emacs.d/helm-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'helm-config)

(defcustom helm-c-google-suggest-url
  "http://google.com/complete/search?output=toolbar&q="
  "URL used for looking up Google suggestions."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for Google searching."
  :type 'string
  :group 'helm-config)

(defcustom helm-google-suggest-use-curl-p nil
  "When non--nil use CURL to get info from `helm-c-google-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-c-yahoo-suggest-url
  "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
  "Url used for looking up Yahoo suggestions."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-yahoo-suggest-search-url
  "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
  "Url used for Yahoo searching."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-boring-buffer-regexp
  (rx (or
       (group bos  " ")
       ;; helm-buffers
       "*helm" "*helm-mode"
       ;; echo area
       " *Echo Area" " *Minibuf"))
  "The regexp that match boring buffers.
Buffer candidates matching this regular expression will be
filtered from the list of candidates if the
`helm-c-skip-boring-buffers' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`helm-c-shadow-boring-buffers' is used."
  :type 'string
  :group 'helm-config)
;; (string-match helm-c-boring-buffer-regexp "buf")
;; (string-match helm-c-boring-buffer-regexp " hidden")
;; (string-match helm-c-boring-buffer-regexp " *Minibuf-1*")

(defcustom helm-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and line-start  ".#")
       (and (or ".class" ".la" ".o" "~") eol)))
  "The regexp that match boring files.
File candidates matching this regular expression will be
filtered from the list of candidates if the
`helm-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`helm-c-shadow-boring-files' is used."
  :type 'string
  :group 'helm-config)

(defcustom helm-kill-ring-threshold 10
  "Minimum length to be listed by `helm-c-source-kill-ring'."
  :type 'integer
  :group 'helm-config)

(defcustom helm-c-kill-ring-max-lines-number nil
  "Max number of lines displayed per candidate in kill-ring browser.
If nil or zero, don't truncate candidate, show all."
  :type 'integer
  :group 'helm-config)

(defcustom helm-su-or-sudo "su"
  "What command to use for root access."
  :type 'string
  :group 'helm-config)

(defcustom helm-for-files-prefered-list
  '(helm-c-source-ffap-line
    helm-c-source-ffap-guesser
    helm-c-source-buffers-list
    helm-c-source-recentf
    helm-c-source-bookmarks
    helm-c-source-file-cache
    helm-c-source-files-in-current-dir+
    helm-c-source-locate)
  "Your prefered sources to find files."
  :type 'list
  :group 'helm-config)

(defcustom helm-create--actions-private nil
  "User defined actions for `helm-create' / `helm-c-source-create'.
It is a list of (DISPLAY . FUNCTION) pairs like `action'
attribute of `helm-sources'.

It is prepended to predefined pairs."
  :type 'list
  :group 'helm-config)

(defcustom helm-allow-skipping-current-buffer nil
  "Show current buffer or not in helm buffer"
  :type 'boolean
  :group 'helm-config)

(defcustom helm-c-enable-eval-defun-hack t
  "If non-nil, execute `helm' using the source at point when C-M-x is pressed.
This hack is invoked when pressing C-M-x in the form \
 (defvar helm-c-source-XXX ...) or (setq helm-c-source-XXX ...)."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-tramp-verbose 0
  "Just like `tramp-verbose' but specific to helm.
When set to 0 don't show tramp messages in helm.
If you want to have the default tramp messages set it to 3."
  :type 'integer
  :group 'helm-config)

(defcustom helm-raise-command nil
  "A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\"."
  :type 'string
  :group 'helm-config)

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
    (dolist (map '(minibuffer-local-completion-map
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

(defcustom helm-c-browse-code-regexp-lisp
  "^ *\(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|\
type\\|theme\\|var\\|group\\|custom\\|const\\|method\\|class\\)"
  "Regexp used to parse lisp buffer when browsing code."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-browse-code-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "Regexp used to parse python buffer when browsing code."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-browse-code-regexp-alist
  `((lisp-interaction-mode . ,helm-c-browse-code-regexp-lisp)
    (emacs-lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (python-mode . ,helm-c-browse-code-regexp-python))
  "Alist to store regexps for browsing code corresponding \
to a specific `major-mode'."
  :type 'list
  :group 'helm-config)

(defcustom helm-c-external-programs-associations nil
  "Alist to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
e.g : '\(\(\"jpg\" . \"gqview\"\) (\"pdf\" . \"xpdf\"\)\) "
  :type 'list
  :group 'helm-config)

(defcustom helm-ff-auto-update-initial-value t
  "Auto update when only one candidate directory is matched.
This is the default value when starting `helm-find-files'."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-copy-async-prefered-emacs "emacs"
  "Path to the emacs you want to use for copying async.
Emacs versions < 24 fail to copy directory due to a bug not fixed
in `copy-directory'."
  :group 'helm-config
  :type 'string)

(defcustom helm-ff-lynx-style-map t
  "Use arrow keys to navigate with `helm-find-files'.
You will have to restart Emacs or reeval `helm-find-files-map'
and `helm-c-read-file-map' for this take effect."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-history-max-length 100
  "Number of elements shown in `helm-find-files' history."
  :group 'helm-config
  :type 'integer)

(defcustom helm-ff-smart-completion t
  "Try to complete filenames smarter when non--nil.
See `helm-ff-transform-fname-for-completion' for more info."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-default-kbsize 1024.0
  "Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems."
  :group 'helm-config
  :type 'float)

(defcustom helm-ff-tramp-not-fancy t
  "No colors when listing remote files when set to non--nil.
This make listing much faster, specially on slow machines."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-ff-exif-data-program "exiftran"
  "Program used to extract exif data of an image file."
  :group 'helm-config
  :type 'string)

(defcustom helm-ff-exif-data-program-args "-d"
  "*Arguments used for `helm-ff-exif-data-program'."
  :group 'helm-config
  :type 'string)

(defcustom helm-c-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-pdfgrep-default-read-command "xpdf '%f' %p"
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number"
  :group 'helm-config
  :type  'string)

(defcustom helm-c-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type  'string
  :group 'helm-config)

(defcustom helm-c-etags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'helm-config)

(defcustom helm-c-etags-use-regexp-search nil
  "When non--nil search etags candidates by regexp.
This disable helm-match-plugin when enabled.
When nil search is performed directly on patter and *match-plugin is used
if available.  You can customize `helm-c-etags-search-regexp'."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-etags-search-regexp "^.+: .+ \\<%s"
  "Regexp that match tags in an helm etags buffer.
The format spec is replaced by pattern.
This regexp have no effect when `helm-c-etags-use-regexp-search'
is nil."
  :group 'helm-config
  :type  'regexp)

(defcustom helm-c-eldoc-in-minibuffer-show-fn
  'helm-c-show-info-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display."
  :group 'helm-config
  :type  'symbol)

(defcustom helm-c-turn-on-show-completion t
  "Display candidate in buffer while moving selection when non--nil."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-show-completion-use-special-display t
  "A special display will be used in lisp completion if non--nil.
All functions that are wrapped in macro `with-helm-show-completion'
will be affected."
  :group 'helm-config
  :type  'boolean)

(defcustom helm-c-show-completion-min-window-height 7
  "Minimum completion window height used in show completion.
This is used in macro `with-helm-show-completion'."
  :group 'helm-config
  :type  'integer)

(defcustom helm-lisp-completion-or-indent-delay 0.6
  "After this delay `helm-lisp-completion-counter' is reset to 0.
This allow to indent again without completing lisp symbol after this delay.
Default is 0.6 seconds."
  :group 'helm-config
  :type  'number)

(defcustom helm-c-default-external-file-browser "nautilus"
  "Default external file browser for your system.
Directories will be opened externally with it when
opening file externally in `helm-find-files'.
Set to nil if you do not have external file browser
or do not want to use it.
Windows users should set that to \"explorer.exe\"."
  :group 'helm-config
  :type  'string)

(defcustom helm-c-use-adaptative-sorting nil
  "Wheter to use or not adaptative sorting.
Even if a source use it, it will have no effect when set to nil."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-ff-newfile-prompt-p t
  "Whether Prompt or not when creating new file.
This set `ffap-newfile-prompt'."
  :type  'boolean
  :group 'helm-config)


(defcustom helm-ff-avfs-directory nil
  "The default avfs directory, usually '.avfs'.
When this is set you will be able to expand archive filenames with `C-z'
inside an avfs directory mounted with mountavfs.
See <http://sourceforge.net/projects/avf/>."
  :type  'boolean
  :group 'helm-config)

(defcustom helm-ff-file-compressed-list '("gz" "bz2" "zip" "7z")
  "Minimal list of compressed files extension."
  :type  'list
  :group 'helm-config)

(defcustom helm-locate-db-file-regexp "m?locate\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string
  :group 'helm-config)

(defcustom helm-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and `helm' find such a file in the directory from
where you launch locate, it will use this file and will not prompt you
for a db file.
Note that this happen only when locate is launched with a prefix arg."
  :group 'helm-config
  :type 'string)

(defcustom helm-c-locate-command nil
  "A list of arguments for locate program.
If nil it will be calculated when `helm-locate' startup
with these default values for different systems:

Gnu/linux: \"locate -i -r %s\"
berkeley-unix: \"locate -i %s\"
windows-nt: \"es -i -r %s\"
Others: \"locate %s\"

This string will be passed to format so it should end with `%s'.
The \"-r\" option must be the last option."
  :type 'string
  :group 'helm-config)

(defcustom helm-c-show-info-in-mode-line-delay 12
  "Eldoc will show info in mode-line during this delay if user is idle."
  :type  'integer
  :group 'helm-config)

(defcustom helm-c-copy-files-async-log-file "/tmp/dired.log"
  "The file used to communicate with two emacs when copying files async."
  :type  'string
  :group 'helm-config)

(defcustom helm-ff-printer-list nil
  "A list of available printers on your system.
When non--nil let you choose a printer to print file.
Otherwise when nil the variable `printer-name' will be used.
On Unix based systems (lpstat command needed) you don't need to set this,
`helm-ff-find-printers' will find a list of available printers for you."
  :type 'list
  :group 'helm-config)

(defcustom helm-ff-transformer-show-only-basename nil
  "Show only basename of candidates in `helm-find-files'.
This can be toggled at anytime from `helm-find-files' with \
\\<helm-find-files-map>0\\[helm-ff-run-toggle-basename]."
  :type 'boolean
  :group 'helm-config)

(defcustom helm-ff-quick-delete-dont-prompt-for-deletion nil
  "Don't ask in persistent deletion of files when non--nil."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-ff-signal-error-on-dot-files t
  "Signal error when file is `.' or `..' on file deletion when non--nil.
Default is non--nil.
WARNING: Setting this to nil is unsafe and can cause deletion of a whole tree."
  :group 'helm-config
  :type 'boolean)

(defcustom helm-completing-read-handlers-alist
  '((describe-function . helm-completing-read-symbols)
    (describe-variable . helm-completing-read-symbols)
    (debug-on-entry . helm-completing-read-symbols)
    (find-function . helm-completing-read-symbols)
    (trace-function . helm-completing-read-symbols)
    (trace-function-background . helm-completing-read-symbols)
    (find-tag . helm-completing-read-with-cands-in-buffer)
    (ffap-alternate-file . nil))
  "Alist of handlers to replace `completing-read', `read-file-name' in `helm-mode'.
Each entry is a cons cell like \(emacs_command . completing-read_handler\)
where key and value are symbols.

Each key is an Emacs command that use originaly `completing-read'.

Each value maybe an helm function that take same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use.
This function prefix name must start by \"helm\".

See `helm-completing-read-symbols' for example.

If the value of an entry is nil completion will fall back to
emacs vanilla behavior.
e.g If you want to disable helm completion for `describe-function':
\(describe-function . nil\).

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just 'ido.
e.g ido completion for `find-file':
\(find-file . ido\)
same as
\(find-file . ido-read-file-name\)
Note that you don't need to enable `ido-mode' for this to work."
  :group 'helm-config
  :type '(alist :key-type symbol :value-type symbol))

(defcustom helm-M-x-requires-pattern 2
  "Value of requires-pattern for `helm-M-x'.
Set it to 0 to disable requires-pattern in `helm-M-x'."
  :group 'helm-config
  :type 'boolean)

;;; Build info-index sources with info-index plug-in.
;;
;;
(defun helm-c-build-info-index-command (name doc source buffer)
  "Define an helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-c-source-info-<NAME>' and BUFFER a string buffer name."
  (eval (list 'defun name nil doc
              (list 'interactive)
              (list 'helm
                    :sources source
                    :buffer buffer
                    :candidate-number-limit 1000))))

(defun helm-c-define-info-index-sources (var-value &optional commands)
  "Define helm sources named helm-c-source-info-<NAME>.
Sources are generated for all entries of `helm-c-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `helm-info<NAME>'.
Where NAME is one of `helm-c-default-info-index-list'."
  (loop with symbols = (loop for str in var-value
                             collect
                             (intern (concat "helm-c-source-info-" str)))
        for sym in symbols
        for str in var-value
        do (set sym (list (cons 'name (format "Info index: %s" str))
                          (cons 'info-index str)))
        when commands
        do (let ((com (intern (concat "helm-info-" str))))
             (helm-c-build-info-index-command
              com (format "Predefined helm for %s info." str)
              sym (format "*helm info %s*" str)))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-c-define-info-index-sources value t))

(defcustom helm-c-default-info-index-list
  '("elisp" "cl" "org" "gnus" "tramp" "ratpoison"
    "zsh" "bash" "coreutils" "fileutils"
    "find" "sh-utils" "textutils" "libc"
    "make" "automake" "autoconf" "emacs-lisp-intro"
    "emacs" "elib" "eieio" "gauche-refe" "guile"
    "guile-tut" "goops" "screen" "latex" "gawk"
    "sed" "m4" "wget" "binutils" "as" "bfd" "gprof"
    "ld" "diff" "flex" "grep" "gzip" "libtool"
    "texinfo" "info" "gdb" "stabs" "cvsbook" "cvs"
    "bison" "id-utils" "global")
  "Info Manual entries to use for building helm info index commands."
  :group 'helm-config
  :type  'list
  :set   'helm-info-index-set)

(defcustom helm-c-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'helm-config
  :type  'integer)


;;; Faces
;;
;;
(defface helm-buffer-saved-out
    '((t (:foreground "red")))
  "*Face used for buffer files modified outside of emacs."
  :group 'helm-config)

(defface helm-buffer-not-saved
    '((t (:foreground "Indianred2")))
  "*Face used for buffer files not already saved on disk."
  :group 'helm-config)

(defface helm-ff-prefix
    '((t (:background "yellow" :foreground "black")))
  "*Face used to prefix new file or url paths in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-executable
    '((t (:foreground "green")))
  "*Face used for executable files in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directories in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-symlink
    '((t (:foreground "DarkOrange")))
  "*Face used for symlinks in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-invalid-symlink
    '((t (:foreground "black" :background "red")))
  "*Face used for invalid symlinks in `helm-find-files'."
  :group 'helm-config)

(defface helm-ff-file
    '((t (:foreground "CadetBlue" :underline t)))
  "*Face used for file names in `helm-find-files'."
  :group 'helm-config)

(defface helm-grep-match
    '((t (:inherit match)))
  "Face used to highlight grep matches."
  :group 'helm-config)

(defface helm-grep-file
    '((t (:foreground "BlueViolet" :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-config)

(defface helm-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-config)

(defface helm-grep-running
    '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'helm-config)

(defface helm-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-config)

(defface helm-M-x-key-face '((t (:foreground "orange" :underline t)))
  "*Face used in helm-M-x to show keybinding."
  :group 'helm)

(defface helm-bmkext-info
    '((t (:foreground "green")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm)

(defface helm-bmkext-w3m
    '((t (:foreground "yellow")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm)

(defface helm-bmkext-gnus
    '((t (:foreground "magenta")))
  "*Face used for Gnus bookmarks."
  :group 'helm)

(defface helm-bmkext-man
    '((t (:foreground "Orange4")))
  "*Face used for Woman/man bookmarks."
  :group 'helm)

(defface helm-bmkext-no--file
    '((t (:foreground "grey")))
  "*Face used for non--file bookmarks."
  :group 'helm)

(defface helm-bmkext-file
    '((t (:foreground "Deepskyblue2")))
  "*Face used for non--file bookmarks."
  :group 'helm)

(defface helm-bookmarks-su-face '((t (:foreground "red")))
  "Face for su/sudo bookmarks."
  :group 'helm)

(defface helm-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'helm)

(defface helm-emms-playlist
    '((t (:foreground "Springgreen4" :underline t)))
  "*Face used for tracks in current emms playlist."
  :group 'helm)

(defface helm-apt-installed
    '((t (:foreground "green")))
  "*Face used for apt installed candidates."
  :group 'helm)

(defface helm-apt-deinstalled
    '((t (:foreground "DimGray")))
  "*Face used for apt deinstalled candidates."
  :group 'helm)

(defface helm-gentoo-match-face '((t (:foreground "red")))
  "Face for helm-gentoo installed packages."
  :group 'traverse-faces)

(defface helm-lisp-show-completion
    '((t (:background "DarkSlateGray")))
  "*Face used for showing candidates in `helm-lisp-completion'."
  :group 'helm-config)

(defface helm-lisp-completion-info
    '((t (:foreground "red")))
  "*Face used for showing info in `helm-lisp-completion'."
  :group 'helm-config)

(defface helm-overlay-line-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the helm buffer." :group 'helm)


(provide 'helm-vars)

;;; helm-vars.el ends here
