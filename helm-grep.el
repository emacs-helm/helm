;;; helm-grep.el --- Helm Incremental Grep. -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-regexp)

;;; load wgrep proxy if it's available
(require 'wgrep-helm nil t)

(declare-function helm-buffer-list "helm-buffers")
(declare-function helm-elscreen-find-file "helm-elscreen" (file))
(declare-function View-quit "view")
(declare-function doc-view-goto-page "doc-view" (page))
(declare-function helm-mm-split-pattern "helm-multi-match")
(declare-function helm--ansi-color-apply "helm-lib")
(defvar helm--ansi-color-regexp)


(defgroup helm-grep nil
  "Grep related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-grep-default-command
  "grep --color=always -a -d skip %e -n%cH -e %p %f"
  "Default grep format command for `helm-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options or
     ack-grep --type option.               (Not mandatory)

'%c' format spec is for case-fold-search,
     whether to use the -i option of grep. (Not mandatory)
     When you specify this spec, helm grep will use smartcase
     that is when a upcase character is found in pattern case will
     be respected and no '-i' option will be used, otherwise, when
     no upcase character is found in pattern always use '-i'.
     If you don't want this behavior, don't use this spec and
     specify or not the '-i' option.
     Note that with ack-grep this is not needed, just specify
     the '--smart-case' option.

'%p' format spec is for pattern.           (Mandatory)

'%f' format spec is for filenames.         (Mandatory)

If your grep version doesn't support the --exclude/include args
don't specify the '%e' format spec.

Helm also support ack-grep and git-grep ,
here a default command example for ack-grep:

\(setq helm-grep-default-command \"ack-grep -Hn --smart-case --no-group %e %p %f\"
       helm-grep-default-recurse-command \"ack-grep -H --smart-case --no-group %e %p %f\")

You can ommit the %e spec if you don't want to be prompted for types.

NOTE: Helm for ack-grep support ANSI sequences, so you can remove
the \"--no-color\" option safely (recommended).

Same for grep you can use safely the option \"--color=always\" (default).
You can customize the color of matches using GREP_COLORS env var.
e.g: \(setenv \"GREP_COLORS\" \"ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36\")

To enable ANSI color in git-grep just add \"--color=always\".
To customize the ANSI color in git-grep, GREP_COLORS have no effect,
you will have to setup this in your .gitconfig:

    [color \"grep\"]
        match = black yellow

where \"black\" is the foreground and \"yellow\" the background.
See the git documentation for more infos.

`helm-grep-default-command' and `helm-grep-default-recurse-command'are
independents, so you can enable `helm-grep-default-command' with ack-grep
and `helm-grep-default-recurse-command' with grep if you want to be faster
on recursive grep.

NOTE: Remote grepping is not available with ack-grep,
      and badly supported with grep because tramp handle badly
      repeated remote processes in a short delay (< to 5s)."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-default-recurse-command
  "grep --color=always -a -d recurse %e -n%cH -e %p %f"
  "Default recursive grep format command for `helm-do-grep-1'.
See `helm-grep-default-command' for format specs and infos about ack-grep."
  :group 'helm-grep
  :type  'string)

(defcustom helm-default-zgrep-command
  "zgrep --color=always -a -n%cH -e %p %f"
  "Default command for Zgrep.
See `helm-grep-default-command' for infos on format specs.
Option --color=always is supported and can be used safely
to replace the helm internal match highlighting,
see `helm-grep-default-command' for more infos."
  :group 'helm-grep
  :type  'string)

(defcustom helm-pdfgrep-default-command
  "pdfgrep --color always -niH %s %s"
  "Default command for pdfgrep.
Option \"--color always\" is supported starting helm version 1.7.8,
when used matchs will be highlighted according to GREP_COLORS env var."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'helm-grep
  :type  'boolean)

(defcustom helm-pdfgrep-default-read-command nil
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number.
e.g In Ubuntu you can set it to:

    \"evince --page-label=%p '%f'\"

If set to nil `doc-view-mode' will be used instead of an external command."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-max-length-history 100
  "Max number of elements to save in `helm-grep-history'."
  :group 'helm-grep
  :type 'integer)

(defcustom helm-zgrep-file-extension-regexp
  ".*\\(\\.gz\\|\\.bz\\|\\.xz\\|\\.lzma\\)$"
  "Default file extensions zgrep will search in."
  :group 'helm-grep
  :type 'string)

(defcustom helm-grep-preferred-ext nil
  "This file extension will be preselected for grep."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-save-buffer-name-no-confirm nil
  "when *hgrep* already exists,auto append suffix."
  :group 'helm-grep
  :type 'boolean)

(defcustom helm-grep-ignored-files
  (cons ".#*" (delq nil (mapcar (lambda (s)
                                  (unless (string-match-p "/\\'" s)
                                    (concat "*" s)))
                                completion-ignored-extensions)))
  "List of file names which `helm-grep' shall exclude."
  :group 'helm-grep
  :type '(repeat string))

(defcustom helm-grep-ignored-directories
  helm-walk-ignore-directories
  "List of names of sub-directories which `helm-grep' shall not recurse into."
  :group 'helm-grep
  :type '(repeat string))

(defcustom helm-grep-truncate-lines t
  "When nil the grep line that appears will not be truncated."
  :group 'helm-grep
  :type 'boolean)

(defcustom helm-grep-file-path-style 'basename
  "File path display style when grep results are displayed.
Possible value are:
    basename: displays only the filename, none of the directory path
    absolute: displays absolute path
    relative: displays relative path from root grep directory."
  :group 'helm-grep
  :type '(choice (const :tag "Basename" basename)
                 (const :tag "Absolute" absolute)
                 (const :tag "Relative" relative)))


;;; Faces
;;
;;
(defgroup helm-grep-faces nil
  "Customize the appearance of helm-grep."
  :prefix "helm-"
  :group 'helm-grep
  :group 'helm-faces)

(defface helm-grep-match
  '((((background light)) :foreground "#b00000")
    (((background dark))  :foreground "gold1"))
  "Face used to highlight grep matches."
  :group 'helm-grep-faces)

(defface helm-grep-file
    '((t (:foreground "BlueViolet"
          :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-grep-faces)

(defface helm-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-grep-faces)

(defface helm-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-grep-faces)

(defface helm-grep-cmd-line
    '((t (:inherit diff-added)))
  "Face used to highlight grep command line when no results."
  :group 'helm-grep-faces)


;;; Keymaps
;;
;;
(defvar helm-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-grep-run-other-window-action)
    (define-key map (kbd "C-c C-o")  'helm-grep-run-other-frame-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-grep-run-save-buffer)
    (when helm-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-execute-persistent-action)
      (define-key map (kbd "<left>")  'helm-grep-run-default-action))
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar helm-pdfgrep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    map)
  "Keymap used in pdfgrep.")

(defvar helm-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-grep-mode-jump)
    (define-key map (kbd "C-o")      'helm-grep-mode-jump-other-window)
    (define-key map (kbd "<C-down>") 'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "<C-up>")   'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    (define-key map (kbd "M-n")      'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "M-p")      'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "M-N")      'helm-gm-next-file)
    (define-key map (kbd "M-P")      'helm-gm-precedent-file)
    map))


;;; Internals vars
;;
;;
(defvar helm-rzgrep-cache (make-hash-table :test 'equal))
(defvar helm-grep-default-function 'helm-grep-init)
(defvar helm-zgrep-recurse-flag nil)
(defvar helm-grep-history nil)
(defvar helm-grep-last-targets nil)
(defvar helm-grep-include-files nil)
(defvar helm-grep-in-recurse nil)
(defvar helm-grep-use-zgrep nil)
(defvar helm-grep-default-directory-fn nil
  "A function that should return a directory to expand candidate to.
It is intended to use as a let-bound variable, DON'T set this globaly.")
(defvar helm-pdfgrep-targets nil)
(defvar helm-grep-last-cmd-line nil)
(defvar helm-grep-split-line-regexp "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")


;;; Init
;;
;;
(defun helm-grep-prepare-candidates (candidates in-directory)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (if helm-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
    ;; When candidate is a directory, search in all its files.
    ;; NOTE that `file-expand-wildcards' will return also
    ;; directories, they will be ignored by grep but not
    ;; by ack-grep that will grep all files of this directory
    ;; without recursing in their subdirs though, see that as a one
    ;; level recursion with ack-grep.
    ;; So I leave it as it is, considering it is a feature. [1]
    (cl-loop for i in candidates append
          (cond ((string-match "^git" helm-grep-default-command)
                 (list i))
                ;; Candidate is a directory and we use recursion or ack.
                ((and (file-directory-p i)
                      (or helm-grep-in-recurse
                          ;; ack-grep accept directory [1].
                          (helm-grep-use-ack-p)))
                 (list (expand-file-name i)))
                ;; Grep doesn't support directory only when not in recurse.
                ((file-directory-p i)
                 (file-expand-wildcards
                  (concat (file-name-as-directory (expand-file-name i)) "*") t))
                ;; Candidate is a file or wildcard and we use recursion, use the
                ;; current directory instead of candidate.
                ((and (or (file-exists-p i) (string-match "[*]" i))
                      helm-grep-in-recurse)
                 (list (expand-file-name
                        (directory-file-name ; Needed for windoze.
                         (file-name-directory (directory-file-name i))))))
                ;; Else should be one or more file/directory
                ;; possibly marked.
                ;; When real is a normal filename without wildcard
                ;; file-expand-wildcards returns a list of one file.
                ;; wildcards should have been already handled by
                ;; helm-read-file-name or helm-find-files but do it from
                ;; here too in case we are called from elsewhere.
                (t (file-expand-wildcards i t))) into all-files ; [1]
          finally return
          (let ((files (if (file-remote-p in-directory)
                       ;; Grep don't understand tramp filenames
                       ;; use the local name.
                       (mapcar (lambda (x)
                                   (file-remote-p x 'localname))
                               all-files)
                       all-files)))
            (if (string-match "^git" helm-grep-default-command)
                (mapconcat 'identity files " ")
                (mapconcat 'shell-quote-argument files " "))))))

(defun helm-grep-command (&optional recursive)
  (let* ((com (if recursive
                  helm-grep-default-recurse-command
                  helm-grep-default-command))
         (exe (and com (car (split-string com " ")))))
    (if (and exe (string= exe "git")) "git-grep" exe)))

(cl-defun helm-grep-use-ack-p (&key where)
  (let* ((rec-com (helm-grep-command t))
         (norm-com (helm-grep-command))
         (norm-com-ack-p (string-match "\\`ack" norm-com))
         (rec-com-ack-p (and rec-com (string-match "\\`ack" rec-com))))
    (cl-case where
      (default   (and norm-com norm-com-ack-p))
      (recursive (and rec-com rec-com-ack-p))
      (strict    (and norm-com rec-com rec-com-ack-p norm-com-ack-p))
      (t         (and (not (and norm-com (string= norm-com "git-grep")))
                      (or (and norm-com norm-com-ack-p)
                          (and rec-com rec-com-ack-p)))))))

(defun helm-grep--prepare-cmd-line (only-files &optional include zgrep)
  (let* ((default-directory (or (helm-default-directory)
                                (expand-file-name helm-ff-default-directory)))
         (fnargs            (helm-grep-prepare-candidates
                             only-files default-directory))
         (ignored-files     (unless (helm-grep-use-ack-p)
                              (mapconcat
                               (lambda (x)
                                   (concat "--exclude="
                                           (shell-quote-argument x)))
                               helm-grep-ignored-files " ")))
         (ignored-dirs      (unless (helm-grep-use-ack-p)
                              (mapconcat
                               ;; Need grep version >=2.5.4
                               ;; of Gnuwin32 on windoze.
                               (lambda (x)
                                   (concat "--exclude-dir="
                                           (shell-quote-argument x)))
                               helm-grep-ignored-directories " ")))
         (exclude           (unless (helm-grep-use-ack-p)
                              (if helm-grep-in-recurse
                                  (concat (or include ignored-files)
                                          " " ignored-dirs)
                                ignored-files)))
         (types             (and (helm-grep-use-ack-p)
                                 ;; When %e format spec is not specified
                                 ;; in `helm-grep-default-command'
                                 ;; we need to pass an empty string
                                 ;; to types to avoid error.
                                 (or include "")))
         (smartcase         (if (helm-grep-use-ack-p) ""
                              (unless (let ((case-fold-search nil))
                                        (string-match-p
                                         "[[:upper:]]" helm-pattern)) "i"))))
    (format-spec
     helm-grep-default-command
     (delq nil
           (list (unless zgrep
                   (if types
                       (cons ?e types)
                     (cons ?e exclude)))
                 (cons ?c (or smartcase ""))
                 (cons ?p (shell-quote-argument helm-pattern))
                 (cons ?f fnargs))))))

(defun helm-grep-init (cmd-line)
  "Start an asynchronous grep process with CMD-LINE using ZGREP if non--nil."
  (let* ((default-directory (or helm-ff-default-directory
                                (helm-default-directory)))
         (zgrep (string-match "\\`zgrep" cmd-line))
         ;; Use pipe only with grep, zgrep or git-grep.
         (process-connection-type (and (not zgrep) (helm-grep-use-ack-p)))
         (tramp-verbose helm-tramp-verbose)
         non-essential)
    ;; Start grep process.
    (helm-log "Starting Grep process in directory `%s'" default-directory)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1            ; This function should return the process first.
        (start-file-process-shell-command
         "grep" helm-buffer cmd-line)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
           (let* ((err      (process-exit-status process))
                  (noresult (= err 1)))
             (unless err
               (helm-process-deferred-sentinel-hook
                process event (helm-default-directory)))
             (cond ((and noresult
                         ;; [FIXME] This is a workaround for zgrep
                         ;; that exit with code 1
                         ;; after a certain amount of results.
                         (not (with-helm-buffer helm-grep-use-zgrep)))
                    (with-helm-buffer
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      (propertize helm-grep-last-cmd-line
                                                  'face 'helm-grep-cmd-line)))
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (no results)] "
                                       (if helm-grep-use-zgrep
                                           "Zgrep"
                                         (capitalize
                                          (if helm-grep-in-recurse
                                              (helm-grep-command t)
                                            (helm-grep-command)))))
                                      'face 'helm-grep-finish))))))
                   ((string= event "finished\n")
                    (with-helm-window
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (%s results)] "
                                       (if helm-grep-use-zgrep
                                           "Zgrep"
                                         (capitalize
                                          (if helm-grep-in-recurse
                                              (helm-grep-command t)
                                            (helm-grep-command))))
                                       (helm-get-candidate-number))
                                      'face 'helm-grep-finish))))
                      (force-mode-line-update)))
                   ;; Catch error output in log.
                   (t (helm-log
                       "Error: %s %s"
                       (if helm-grep-use-zgrep "Zgrep" "Grep")
                       (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-grep-collect-candidates ()
  (let ((cmd-line (helm-grep--prepare-cmd-line
                   helm-grep-last-targets
                   helm-grep-include-files
                   helm-grep-use-zgrep)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (funcall helm-grep-default-function cmd-line)))


;;; Actions
;;
;;
(defun helm-grep-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep-1' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (helm-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname        (or (with-current-buffer
                                   (if (eq major-mode 'helm-grep-mode)
                                       (current-buffer)
                                       helm-buffer)
                                 (get-text-property (point-at-bol) 'help-echo))
                               (car split)))
         (tramp-method (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'method))
         (tramp-host   (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (cl-case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (pdf          (if helm-pdfgrep-default-read-command
                        (helm-pdfgrep-action-1 split lineno (car split))
                      (find-file (car split)) (doc-view-goto-page lineno)))
      (t            (find-file fname)))
    (unless (or (eq where 'grep) (eq where 'pdf))
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (eq major-mode 'helm-grep-mode)
                (string= helm-pattern ""))
      (setq helm-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-grep-history)))
      (when (> (length helm-grep-history)
               helm-grep-max-length-history)
        (setq helm-grep-history
              (delete (car (last helm-grep-history))
                      helm-grep-history))))))

(defun helm-grep-persistent-action (candidate)
  "Persistent action for `helm-do-grep-1'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-grep-action candidate nil 'mark)
    (helm-grep-action candidate))
  (helm-highlight-current-line))

(defun helm-grep-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-grep-action candidate 'other-window))

(defun helm-grep-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-grep-action candidate 'other-frame))

(defun helm-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from helm grep."
  (helm-grep-action candidate 'elscreen))

(defun helm-goto-next-or-prec-file (n)
  "Go to next or precedent candidate file in helm grep/etags buffers.
If N is positive go forward otherwise go backward."
  (let* ((allow-mode (or (eq major-mode 'helm-grep-mode)
                         (eq major-mode 'helm-moccur-mode)))
         (sel (if allow-mode
                  (buffer-substring (point-at-bol) (point-at-eol))
                (helm-get-selection nil t)))
         (current-line-list  (helm-grep-split-line sel))
         (current-fname      (nth 0 current-line-list))
         (bob-or-eof         (if (eq n 1) 'eobp 'bobp))
         (mark-maybe (lambda ()
                         (if allow-mode
                             (ignore)
                           (helm-mark-current-line)))))
    (catch 'break
      (while (not (funcall bob-or-eof))
        (forward-line n) ; Go forward or backward depending of n value.
        ;; Exit when current-fname is not matched or in `helm-grep-mode'
        ;; the line is not a grep line i.e 'fname:num:tag'.
        (setq sel (buffer-substring (point-at-bol) (point-at-eol)))
        (unless (or (string= current-fname
                             (car (helm-grep-split-line sel)))
                    (and (eq major-mode 'helm-grep-mode)
                         (not (get-text-property (point-at-bol) 'help-echo))))
          (funcall mark-maybe)
          (throw 'break nil))))
    (cond ((and (> n 0) (eobp))
           (re-search-backward ".")
           (forward-line 0)
           (funcall mark-maybe))
          ((and (< n 0) (bobp))
           (helm-aif (next-single-property-change (point-at-bol) 'help-echo)
               (goto-char it)
             (forward-line 1))
           (funcall mark-maybe)))
    (helm-log-run-hook 'helm-move-selection-after-hook)))

;;;###autoload
(defun helm-goto-precedent-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (helm-goto-next-or-prec-file -1))))
(put 'helm-goto-precedent-file 'helm-only t)

;;;###autoload
(defun helm-goto-next-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (with-helm-window
    (helm-goto-next-or-prec-file 1)))

(defun helm-grep-run-default-action ()
  "Run grep default action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-grep-action)))
(put 'helm-grep-run-default-action 'helm-only t)

(defun helm-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-grep-other-window)))
(put 'helm-grep-run-other-window-action 'helm-only t)

(defun helm-grep-run-other-frame-action ()
  "Run grep goto other frame action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-grep-other-frame)))
(put 'helm-grep-run-other-frame-action 'helm-only t)

(defun helm-grep-run-save-buffer ()
  "Run grep save results action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-grep-save-results)))
(put 'helm-grep-run-save-buffer 'helm-only t)


;;; helm-grep-mode
;;
;;
(defun helm-grep-save-results (candidate)
  (helm-grep-action candidate 'grep))

(defun helm-grep-save-results-1 ()
  "Save helm grep result in a `helm-grep-mode' buffer."
  (let ((buf "*hgrep*")
        new-buf)
    (when (get-buffer buf)
      (if helm-grep-save-buffer-name-no-confirm
          (setq new-buf  (format "*hgrep|%s|-%s" helm-pattern
                                 (format-time-string "%H-%M-%S*")))
          (setq new-buf (helm-read-string "GrepBufferName: " buf))
          (cl-loop for b in (helm-buffer-list)
                   when (and (string= new-buf b)
                             (not (y-or-n-p
                                   (format "Buffer `%s' already exists overwrite? "
                                           new-buf))))
                   do (setq new-buf (helm-read-string "GrepBufferName: " "*hgrep "))))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "-*- mode: helm-grep -*-\n\n"
                (format "Grep Results for `%s':\n\n" helm-pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))))
      (helm-grep-mode) (pop-to-buffer buf))
    (message "Helm Grep Results saved in `%s' buffer" buf)))

(define-derived-mode helm-grep-mode
    special-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}"
    (set (make-local-variable 'helm-grep-last-cmd-line)
         (with-helm-buffer helm-grep-last-cmd-line))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-grep-mode--revert-buffer-function))

(defun helm-grep-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  (goto-char (point-min))
  (re-search-forward "^Grep Results for" nil t)
  (forward-line 0)
  (when (re-search-forward "^$" nil t)
    (forward-line 1))
  (let ((inhibit-read-only t))
    (delete-region (point) (point-max)))
  (message "Reverting buffer...")
  (set-process-sentinel
   (start-file-process-shell-command
    "hgrep"  (generate-new-buffer "*hgrep revert*") helm-grep-last-cmd-line)
   (lambda (process event)
     (when (string= event "finished\n")
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (save-excursion
             (cl-loop for line in (with-current-buffer (process-buffer process)
                                    (prog1 (split-string (buffer-string) "\n")
                                      (kill-buffer)))
                      when (string-match helm-grep-split-line-regexp line)
                      do (insert (propertize
                                  (car (helm-grep-filter-one-by-one line))
                                  ;; needed for wgrep.
                                  'helm-realvalue line)
                                 "\n"))))
         (message "Reverting buffer done"))))))

(defun helm-gm-next-file ()
  (interactive)
  (helm-goto-next-or-prec-file 1))

(defun helm-gm-precedent-file ()
  (interactive)
  (helm-goto-next-or-prec-file -1))

(defun helm-grep-mode-jump ()
  (interactive)
  (helm-grep-action
   (buffer-substring (point-at-bol) (point-at-eol))))

(defun helm-grep-mode-jump-other-window-1 (arg)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (progn
          (save-selected-window
            (helm-grep-action candidate 'other-window)
            (recenter))
          (forward-line arg))
      (error nil))))

(defun helm-grep-mode-jump-other-window-forward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 1))

(defun helm-grep-mode-jump-other-window-backward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 -1))

(defun helm-grep-mode-jump-other-window ()
  (interactive)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (helm-grep-action candidate 'other-window)
      (error nil))))


;;; ack-grep types
;;
;;
(defun helm-grep-hack-types ()
  "Return a list of known ack-grep types."
  (with-temp-buffer
    ;; "--help-types" works with both 1.96 and 2.1+, while
    ;; "--help types" works only with 1.96 Issue #422.
    ;; `helm-grep-command' should return the ack executable
    ;; when this function is used in the right context
    ;; i.e After checking is we are using ack-grep with
    ;; `helm-grep-use-ack-p'.
    (call-process (helm-grep-command t) nil t nil "--help-types")
    (goto-char (point-min))
    (cl-loop while (re-search-forward
                    "^ *--\\(\\[no\\]\\)\\([^. ]+\\) *\\(.*\\)" nil t)
             collect (cons (concat (match-string 2)
                                   " [" (match-string 3) "]")
                           (match-string 2))
             collect (cons (concat "no" (match-string 2)
                                   " [" (match-string 3) "]")
                           (concat "no" (match-string 2))))))

(defun helm-grep-ack-types-transformer (candidates _source)
  (cl-loop for i in candidates
        if (stringp i)
        collect (rassoc i helm-grep-ack-types-cache)
        else
        collect i))

(defvar helm-grep-ack-types-cache nil)
(defun helm-grep-read-ack-type ()
  "Select types for the '--type' argument of ack-grep."
  (require 'helm-mode)
  (require 'helm-adaptive)
  (setq helm-grep-ack-types-cache (helm-grep-hack-types))
  (let ((types (helm-comp-read
                "Types: " helm-grep-ack-types-cache
                :name "*Ack-grep types*"
                :marked-candidates t
                :must-match t
                :fc-transformer '(helm-adaptive-sort
                                  helm-grep-ack-types-transformer)
                :buffer " *helm ack-types*")))
    (mapconcat (lambda (type) (concat "--type=" type)) types " ")))


;;; grep extensions
;;
;;
(defun helm-grep-guess-extensions (files)
  "Try to guess file extensions in FILES list when using grep recurse.
These extensions will be added to command line with --include arg of grep."
  (cl-loop with ext-list = (list helm-grep-preferred-ext "*")
        with lst = (if (file-directory-p (car files))
                       (directory-files
                        (car files) nil
                        directory-files-no-dot-files-regexp)
                     files)
        for i in lst
        for ext = (file-name-extension i 'dot)
        for glob = (and ext (not (string= ext ""))
                        (concat "*" ext))
        unless (or (not glob)
                   (and glob-list (member glob glob-list))
                   (and glob-list (member glob ext-list))
                   (and glob-list (member glob helm-grep-ignored-files)))
        collect glob into glob-list
        finally return (delq nil (append ext-list glob-list))))

(defun helm-grep-get-file-extensions (files)
  "Try to return a list of file extensions to pass to '--include' arg of grep."
  (let* ((all-exts (helm-grep-guess-extensions
                    (mapcar 'expand-file-name files)))
         (extensions (helm-comp-read "Search Only in: " all-exts
                                     :marked-candidates t
                                     :fc-transformer 'helm-adaptive-sort
                                     :buffer " *helm grep exts*"
                                     :name "*helm grep extensions*")))
    (when (listp extensions) ; Otherwise it is empty string returned by C-RET.
      ;; If extensions is a list of one string containing spaces,
      ;; assume user entered more than one glob separated by space(s) and
      ;; split this string to pass it later to mapconcat.
      ;; e.g '("*.el *.py")
      (cl-loop for i in extensions
               append (split-string-and-unquote i " ")))))


;;; Set up source
;;
;;
(defvar helm-source-grep nil)
(defun helm-do-grep-1 (targets &optional recurse zgrep exts default-input input)
  "Launch grep on a list of TARGETS files.
When RECURSE is given use -r option of grep and prompt user
to set the --include args of grep.
You can give more than one arg separated by space at prompt.
e.g *.el *.py *.tex.
From lisp use the EXTS argument as a list of extensions as above.
If you are using ack-grep, you will be prompted for --type
instead and EXTS will be ignored.
If prompt is empty `helm-grep-ignored-files' are added to --exclude.
Argument DEFAULT-INPUT is use as `default' arg of `helm' and INPUT
is used as `input' arg of `helm', See `helm' docstring.
ZGREP when non--nil use zgrep instead, without prompting for a choice
in recurse, and ignoring EXTS, search being made on
`helm-zgrep-file-extension-regexp'."
  (when (and (helm-grep-use-ack-p)
             helm-ff-default-directory
             (file-remote-p helm-ff-default-directory))
    (error "Error: Remote operation not supported with ack-grep."))
  (let* (non-essential
         (exts (and recurse
                    ;; [FIXME] I could handle this from helm-walk-directory.
                    (not zgrep) ; zgrep doesn't handle -r opt.
                    (not (helm-grep-use-ack-p :where 'recursive))
                    (or exts (helm-grep-get-file-extensions targets))))
         (include-files (and exts
                             (mapconcat (lambda (x)
                                            (concat "--include="
                                                    (shell-quote-argument x)))
                                        (if (> (length exts) 1)
                                            (remove "*" exts)
                                          exts) " ")))
         (types (and (not include-files)
                     (not zgrep)
                     recurse
                     (helm-grep-use-ack-p :where 'recursive)
                     ;; When %e format spec is not specified
                     ;; ignore types and do not prompt for choice.
                     (string-match "%e" helm-grep-default-command)
                     (helm-grep-read-ack-type)))
         (follow (and helm-follow-mode-persistent
                      (assoc-default 'follow helm-source-grep))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    ;; If `helm-find-files' haven't already started,
    ;; give a default value to `helm-ff-default-directory'.
    (unless helm-ff-default-directory
      (setq helm-ff-default-directory default-directory))
    ;; We need to store these vars locally
    ;; to pass infos later to `helm-resume'.
    (helm-set-local-variable 'helm-zgrep-recurse-flag (and recurse zgrep)
                             'helm-grep-last-targets targets
                             'helm-grep-include-files (or include-files types)
                             'helm-grep-in-recurse recurse
                             'helm-grep-use-zgrep zgrep
                             'helm-grep-default-command
                             (cond (zgrep helm-default-zgrep-command)
                                   (recurse helm-grep-default-recurse-command)
                                   ;; When resuming the local value of
                                   ;; `helm-grep-default-command' is used, only git-grep
                                   ;; should need this.
                                   (t helm-grep-default-command)))
    ;; Setup the source.
    (setq helm-source-grep
          (helm-build-async-source
           (if zgrep "Zgrep" (capitalize (if recurse
                                             (helm-grep-command t)
                                             (helm-grep-command))))
            :candidates-process 'helm-grep-collect-candidates
            :filter-one-by-one 'helm-grep-filter-one-by-one
            :keymap helm-grep-map
            :nohighlight t
            :nomark t
            :candidate-number-limit 9999
            :help-message 'helm-grep-help-message
            :history 'helm-grep-history
            :action (helm-make-actions
                     "Find File" 'helm-grep-action
                     "Find file other frame" 'helm-grep-other-frame
                     (lambda () (and (locate-library "elscreen")
                                     "Find file in Elscreen"))
                     'helm-grep-jump-elscreen
                     "Save results in grep buffer" 'helm-grep-save-results
                     "Find file other window" 'helm-grep-other-window)
            :persistent-action 'helm-grep-persistent-action
            :persistent-help "Jump to line (`C-u' Record in mark ring)"
            :requires-pattern 2
            :follow follow))
    (helm
     :sources 'helm-source-grep
     :buffer (format "*helm %s*" (if zgrep "zgrep" (helm-grep-command recurse)))
     :default default-input
     :input input
     :keymap helm-grep-map
     :history 'helm-grep-history
     :truncate-lines helm-grep-truncate-lines)))



;;; zgrep
;;
;;
(defun helm-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or helm-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir helm-rzgrep-cache)
                               (puthash
                                def-dir
                                (helm-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match helm-zgrep-file-extension-regexp)
                                helm-rzgrep-cache))
                         flist)))
         (helm-do-grep-1 only recursive 'zgrep))
    (setq helm-zgrep-recurse-flag nil)))


;;; transformers
;;
;;
(defun helm-grep-split-line (line)
  "Split a grep output line."
  ;; The output of grep may send a truncated line in this chunk,
  ;; so don't split until grep line is valid, that is
  ;; once the second part of the line comes with next chunk
  ;; send by process.
  (when (string-match helm-grep-split-line-regexp line)
    ;; Don't use split-string because buffer/file name or string
    ;; may contain a ":".
    (cl-loop for n from 1 to 3 collect (match-string n line))))

(defun helm-grep--filter-candidate-1 (candidate &optional dir)
  (let* ((root   (or dir (and helm-grep-default-directory-fn
                              (funcall helm-grep-default-directory-fn))))
         (ansi-p (string-match-p helm--ansi-color-regexp candidate))
         (line   (if ansi-p (helm--ansi-color-apply candidate) candidate))
         (split  (helm-grep-split-line line))
         (fname  (if (and root split)
                     (expand-file-name (car split) root)
                   (car-safe split)))
         (lineno (nth 1 split))
         (str    (nth 2 split))
         (display-fname (cl-ecase helm-grep-file-path-style
                          (basename (and fname (file-name-nondirectory fname)))
                          (absolute fname)
                          (relative (and fname root
                                         (file-relative-name fname root))))))
    (if (and display-fname lineno str)
        (cons (concat (propertize display-fname
                                  'face 'helm-grep-file
                                  'help-echo fname)
                      ":"
                      (propertize lineno 'face 'helm-grep-lineno)
                      ":"
                      (if ansi-p str (helm-grep-highlight-match str)))
              line)
        "")))

(defun helm-grep-filter-one-by-one (candidate)
  "`filter-one-by-one' transformer function for `helm-do-grep-1'."
  (let ((helm-grep-default-directory-fn
         (or helm-grep-default-directory-fn
             (lambda () (or helm-ff-default-directory
                            (helm-default-directory)
                            default-directory)))))
    (if (consp candidate)
        ;; Already computed do nothing (default as input).
        candidate
        (and (stringp candidate)
             (helm-grep--filter-candidate-1 candidate)))))

(defun helm-grep-highlight-match (str &optional multi-match)
  "Highlight in string STR all occurences matching `helm-pattern'."
  (let (beg end)
    (condition-case-unless-debug nil
        (with-temp-buffer
          (insert (propertize str 'read-only nil)) ; Fix (#1176)
          (goto-char (point-min))
          (cl-loop for reg in
                   (if multi-match
                       ;; (m)occur.
                       (cl-loop for r in (helm-mm-split-pattern
                                          helm-pattern)
                                unless (string-match "\\`!" r)
                                collect
                                (helm-aif (and helm-migemo-mode
                                               (assoc r helm-mm--previous-migemo-info))
                                    (cdr it) r))
                       ;; async sources (grep, gid etc...)
                       (list helm-input))
                   do
                   (while (and (re-search-forward reg nil t)
                               (> (- (setq end (match-end 0))
                                     (setq beg (match-beginning 0))) 0))
                     (add-text-properties beg end '(face helm-grep-match)))
                   do (goto-char (point-min))) 
          (buffer-string))
      (error nil))))


;;; Grep from buffer list
;;
;;
(defun helm-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file--buffers or CANDIDATE if it is a file--buffer.
If one of selected buffers is not a file--buffer,
it is ignored and grep will run on all others file--buffers.
If only one candidate is selected and it is not a file--buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring non--file-buffers."
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (helm-ff-default-directory
          (if (and helm-ff-default-directory
                   (file-remote-p helm-ff-default-directory))
              default-directory
            helm-ff-default-directory))
         (cands (if prefarg
                    (buffer-list)
                  (helm-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname and remote buffers are ignored.
         (bufs (cl-loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when (and fname (not (file-remote-p fname)))
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (helm-do-grep-1 bufs nil 'zgrep)
          (helm-do-grep-1 bufs))
      ;; bufs is empty, thats mean we have only CANDIDATE
      ;; and it is not a buffer-filename, fallback to occur.
      (switch-to-buffer candidate)
      (when (get-buffer helm-action-buffer)
        (kill-buffer helm-action-buffer))
      (helm-occur)
      (when (eq helm-exit-status 1)
        (set-window-configuration win-conf)))))

(defun helm-grep-buffers (candidate)
  "Action to grep buffers."
  (helm-grep-buffers-1 candidate))

(defun helm-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (helm-grep-buffers-1 candidate 'zgrep))


;;; Helm interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar helm-pdfgrep-default-function 'helm-pdfgrep-init)
(defun helm-pdfgrep-init (only-files)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((default-directory (or helm-ff-default-directory
                                default-directory))
         (fnargs   (helm-grep-prepare-candidates
                    (if (file-remote-p default-directory)
                        (mapcar (lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                      only-files)
                    default-directory))
         (cmd-line (format helm-pdfgrep-default-command
                           helm-pattern
                           fnargs))
         process-connection-type)
    ;; Start pdf grep process.
    (helm-log "Starting Pdf Grep process in directory `%s'" default-directory)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1
        (start-file-process-shell-command
         "pdfgrep" helm-buffer cmd-line)
      (message nil)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                         (:eval (propertize
                                 (format "[Pdfgrep Process Finish - %s result(s)] "
                                         (max (1- (count-lines
                                                   (point-min) (point-max))) 0))
                                 'face 'helm-grep-finish))))
                 (force-mode-line-update))
             (helm-log "Error: Pdf grep %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-do-pdfgrep-1 (only)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let* (helm-grep-in-recurse) ; recursion is never used in pdfgrep.
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    (setq helm-pdfgrep-targets only)
    (helm
     :sources (helm-build-async-source "PdfGrep"
                :init (lambda ()
                        ;; If `helm-find-files' haven't already started,
                        ;; give a default value to `helm-ff-default-directory'.
                        (setq helm-ff-default-directory (or helm-ff-default-directory
                                                            default-directory)))
                :candidates-process (lambda ()
                                      (funcall helm-pdfgrep-default-function helm-pdfgrep-targets))
                :nohighlight t
                :nomark t
                :filter-one-by-one #'helm-grep-filter-one-by-one
                :candidate-number-limit 9999
                :history 'helm-grep-history
                :keymap helm-pdfgrep-map
                :help-message 'helm-pdfgrep-help-message
                :action #'helm-pdfgrep-action
                :persistent-help "Jump to PDF Page"
                :requires-pattern 2)
     :buffer " *helm pdfgrep*"
     :history 'helm-grep-history)))

(defun helm-pdfgrep-action (candidate)
  (helm-grep-action candidate 'pdf))

(defun helm-pdfgrep-action-1 (_split pageno fname)
  (save-selected-window
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec helm-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))

;;; AG - AT
;;
;;  https://github.com/ggreer/the_silver_searcher
;;  https://github.com/monochromegane/the_platinum_searcher

(defcustom helm-grep-ag-command
  "ag --line-numbers -S --hidden --color --nogroup %s %s"
  "The default command for AG or PT.
Takes two format specs, the first for pattern and the second for directory.

You must use an output format that fit with helm grep, that is:

    \"filename:line-number:string\"

The option \"--nogroup\" allow this.
The option \"--line-numbers\" is also mandatory except with PT (not supported).

You can use safely \"--color\" (default)."
  :group 'helm-grep
  :type 'string)

(defun helm-grep--ag-command ()
  (car (split-string helm-grep-ag-command)))

(defun helm-grep-ag-init (directory)
  (let ((cmd-line (format helm-grep-ag-command
                          (shell-quote-argument helm-pattern)
                          (shell-quote-argument directory))))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (prog1
        (start-process-shell-command
         "ag" helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (_process event)
         (when (string= event "finished\n")
           (with-helm-window
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                     (:eval (propertize
                             (format
                              "[%s process finished - (%s results)] "
                              (upcase (helm-grep--ag-command))
                              (helm-get-candidate-number))
                             'face 'helm-grep-finish))))
             (force-mode-line-update))))))))

(defvar helm-source-grep-ag nil)
(defun helm-grep-ag-1 (directory)
  (setq helm-source-grep-ag
        (helm-build-async-source (upcase (helm-grep--ag-command))
          :header-name (lambda (name)
                         (format "%s [%s]"
                                 name (abbreviate-file-name directory)))
          :candidates-process
          (lambda () (helm-grep-ag-init directory))
          :nohighlight t
          :keymap helm-grep-map
          :help-message 'helm-grep-help-message
          :filter-one-by-one 'helm-grep-filter-one-by-one
          :persistent-action 'helm-grep-persistent-action
          :candidate-number-limit 99999
          :requires-pattern 2
          :nomark t
          :action (helm-make-actions
                   "Find File" 'helm-grep-action
                   "Find file other frame" 'helm-grep-other-frame
                   (lambda () (and (locate-library "elscreen")
                                   "Find file in Elscreen"))
                   'helm-grep-jump-elscreen
                   "Save results in grep buffer" 'helm-grep-save-results
                   "Find file other window" 'helm-grep-other-window)))
  (helm :sources 'helm-source-grep-ag
        :keymap helm-grep-map
        :truncate-lines helm-grep-truncate-lines
        :buffer (format "*helm %s*" (helm-grep--ag-command))))

;;; Git grep
;;
;;
(defcustom helm-grep-git-grep-command
  "git grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f"
  "The git grep default command line.
The option \"--color=always\" can be used safely.
The color of matched items can be customized in your .gitconfig
See `helm-grep-default-command' for more infos.

The \"--exclude-standard\" and \"--no-index\" switches allow
skipping unwanted files specified in ~/.gitignore_global
and searching files not already staged.
You have also to enable this in global \".gitconfig\" with
    \"git config --global core.excludesfile ~/.gitignore_global\"."
  :group 'helm-grep
  :type 'string)

(defun helm-grep-git-1 (directory &optional all)
  (require 'vc)
  (let* ((helm-grep-default-command helm-grep-git-grep-command)
         helm-grep-default-recurse-command
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the help-echo prop.
         (helm-grep-default-directory-fn (lambda ()
                                           (vc-find-root directory ".git")))
         (helm-ff-default-directory (funcall helm-grep-default-directory-fn)))
    (cl-assert helm-ff-default-directory nil "Not inside a Git repository")
    (helm-do-grep-1 (if all '("") `(,(expand-file-name directory))))))


;;;###autoload
(defun helm-do-grep-ag ()
  "Preconfigured helm for grepping with AG in `default-directory'."
  (interactive)
  (require 'helm-files)
  (helm-grep-ag-1 default-directory))

;;;###autoload
(defun helm-grep-do-git-grep (arg)
  "Preconfigured helm for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-git-1 default-directory arg))


(provide 'helm-grep)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-grep.el ends here
