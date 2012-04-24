;;; helm-grep.el --- Helm Incremental Grep.

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
(eval-when-compile (require 'cl))
(require 'helm)
(require 'grep)
(require 'helm-regexp)
(require 'helm-buffers)

(declare-function helm-elscreen-find-file "helm-elscreen" (file))
(declare-function View-quit "view")


(defgroup helm-grep nil
  "Grep related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-c-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'helm-grep
  :type  'boolean)

(defcustom helm-c-pdfgrep-default-read-command "xpdf '%f' %p"
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number"
  :group 'helm-grep
  :type  'string)

(defcustom helm-c-grep-max-length-history 100
  "Max number of elements to save in `helm-c-grep-history'."
  :group 'helm-grep
  :type 'integer)

(defcustom helm-zgrep-file-extension-regexp
  ".*\\(\.gz\\|\.bz\\|\.xz\\|\.lzma\\)$"
  "Default file extensions zgrep will search in."
  :group 'helm-grep
  :type 'string)


;;; Faces
;;
;;
(defface helm-grep-match
    '((t (:inherit match)))
  "Face used to highlight grep matches."
  :group 'helm-grep)

(defface helm-grep-file
    '((t (:foreground "BlueViolet" :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-grep)

(defface helm-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-grep)

(defface helm-grep-running
    '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'helm-grep)

(defface helm-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-grep)


(defvar helm-c-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-c-grep-run-other-window-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-c-grep-run-save-buffer)
    (when helm-c-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-c-grep-run-persistent-action)
      (define-key map (kbd "<left>")  'helm-c-grep-run-default-action))
    (define-key map (kbd "C-c ?")    'helm-grep-help)
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar helm-c-pdfgrep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-c-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-c-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-pdfgrep-help)
    map)
  "Keymap used in pdfgrep.")

(defvar helm-c-grep-default-command
  "grep -d skip %e -niH -e %p %f"
  "Default grep format command for `helm-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options.
'%p' format spec is for pattern.
'%f' format spec is for filenames.")

(defvar helm-c-grep-default-recurse-command
  "grep -d recurse %e -niH -e %p %f"
  "Default recursive grep format command for `helm-do-grep-1'.
See `helm-c-grep-default-command' for format specs.")

(defvar helm-c-default-zgrep-command "zgrep -niH -e %p %f")

(defvar helm-c-rzgrep-cache (make-hash-table :test 'equal))

(defvar helm-c-grep-default-function 'helm-c-grep-init)

(defvar helm-c-grep-debug-command-line nil
  "Turn on helm grep command-line debugging when non--nil.")

(defvar helm-c-zgrep-recurse-flag nil)

(defvar helm-c-grep-history nil)

(defun helm-c-grep-prepare-candidates (candidates)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (if helm-c-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
      (loop for i in candidates append
            (cond ( ;; Candidate is a directory and we use recursion.
                   (and (file-directory-p i)
                        (helm-c-grep-recurse-p))
                   (list (expand-file-name i)))
                  ;; Candidate is a directory, search in all files.
                  ((file-directory-p i)
                   (file-expand-wildcards
                    (concat (file-name-as-directory (expand-file-name i)) "*") t))
                  ;; Candidate is a file or wildcard and we use recursion, use the
                  ;; current directory instead of candidate.
                  ((and (or (file-exists-p i) (string-match "\*" i))
                        (helm-c-grep-recurse-p))
                   (list (expand-file-name
                          (directory-file-name ; Needed for windoze.
                           (file-name-directory (directory-file-name i))))))
                  ;; Candidate use wildcard.
                  ((string-match "^\*" (helm-c-basename i))
                   (file-expand-wildcards i t))
                  ;; Else should be one or more file.
                  (t (list i))) into all-files
            finally return
            (mapconcat 'shell-quote-argument all-files " "))))

(defun helm-c-grep-recurse-p ()
  "Check if `helm-do-grep-1' have switched to recursive."
  (let ((args (replace-regexp-in-string
               "grep" "" helm-c-grep-default-command)))
    (string-match-p "r\\|recurse" args)))

(defun helm-c-grep-init (only-files &optional include zgrep)
  "Start an asynchronous grep process in ONLY-FILES list."
  (let* ((fnargs        (helm-c-grep-prepare-candidates
                         (if (file-remote-p helm-ff-default-directory)
                             (mapcar #'(lambda (x)
                                         (file-remote-p x 'localname))
                                     only-files)
                             only-files)))
         (ignored-files (mapconcat
                         #'(lambda (x)
                             (concat "--exclude=" (shell-quote-argument x)))
                         grep-find-ignored-files " "))
         (ignored-dirs  (mapconcat
                         ;; Need grep version >=2.5.4 of Gnuwin32 on windoze.
                         #'(lambda (x)
                             (concat "--exclude-dir=" (shell-quote-argument x)))
                         grep-find-ignored-directories " "))
         (exclude       (if (helm-c-grep-recurse-p)
                            (concat (or include ignored-files) " " ignored-dirs)
                            ignored-files))
         (cmd-line      (format-spec
                         helm-c-grep-default-command
                         (delq nil
                               (list (unless zgrep (cons ?e exclude))
                                     (cons ?p (shell-quote-argument helm-pattern))
                                     (cons ?f fnargs))))))
    (when helm-c-grep-debug-command-line
      (with-current-buffer (get-buffer-create "*any grep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (when (get-process "grep-process")
                     (propertize "[Grep Process Running] "
                                 'face 'helm-grep-running)))))
    (force-mode-line-update nil)
    (prog1
        (let ((default-directory helm-ff-default-directory))
          (start-file-process-shell-command "grep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "grep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-helm-window
               (helm-update-move-first-line)
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (line-number-mode "%l") " "
                       (:eval (propertize
                               (format "[Grep Process Finished - (%s results)] "
                                       (let ((nlines (1- (count-lines
                                                          (point-min)
                                                          (point-max)))))
                                         (if (> nlines 0) nlines 0)))
                               'face 'helm-grep-finish))))
               (force-mode-line-update nil))))))))

(defun helm-c-grep-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (helm-c-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname    (or (get-text-property (point-at-bol) 'help-echo)
                           (car split)))
         (tramp-method (file-remote-p helm-ff-default-directory 'method))
         (tramp-host   (file-remote-p helm-ff-default-directory 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-c-grep-save-results-1))
      (pdf          (helm-c-pdfgrep-action-1 split lineno (car split)))
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
      (setq helm-c-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-c-grep-history)))
      (when (> (length helm-c-grep-history)
               helm-c-grep-max-length-history)
        (setq helm-c-grep-history
              (delete (car (last helm-c-grep-history))
                      helm-c-grep-history))))))

(defun helm-c-grep-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-c-grep-action candidate 'other-window))

(defun helm-c-grep-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-c-grep-action candidate 'other-frame))

(defun helm-c-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from helm grep."
  (helm-c-grep-action candidate 'elscreen))

(defun helm-c-grep-save-results (_candidate)
  (helm-c-grep-action _candidate 'grep))

(defun helm-c-grep-save-results-1 ()
  "Save helm grep result in a `grep-mode' buffer."
  (let ((buf "*hgrep*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string "GrepBufferName: " buf))
      (loop for b in (helm-c-buffer-list)
            when (and (string= new-buf b)
                      (not (y-or-n-p
                            (format "Buffer `%s' already exists overwrite? "
                                    new-buf))))
            do (setq new-buf (read-string "GrepBufferName: " "*hgrep ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (set (make-local-variable 'buffer-read-only) t)
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

(defvar helm-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-grep-mode-jump)
    (define-key map (kbd "C-o")      'helm-grep-mode-jump-other-window)
    (define-key map (kbd "q")        'helm-grep-mode-quit)
    (define-key map (kbd "<C-down>") 'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "<C-up>")   'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    map))

;;;###autoload
(define-derived-mode helm-grep-mode
    text-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}")

;;;###autoload
(defun helm-gm-next-file ()
  (interactive)
  (helm-c-goto-next-or-prec-file 1))

;;;###autoload
(defun helm-gm-precedent-file ()
  (interactive)
  (helm-c-goto-next-or-prec-file -1))

;;;###autoload
(defun helm-grep-mode-quit ()
  (interactive)
  (view-mode 1) (View-quit))

;;;###autoload
(defun helm-grep-mode-jump ()
  (interactive)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (progn (helm-c-grep-action candidate) (delete-other-windows))
      (error nil))))

(defun helm-grep-mode-jump-other-window-1 (arg)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (progn
          (save-selected-window
            (helm-c-grep-action candidate 'other-window)
            (recenter))
          (forward-line arg))
      (error nil))))

;;;###autoload
(defun helm-grep-mode-jump-other-window-forward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 1))

;;;###autoload
(defun helm-grep-mode-jump-other-window-backward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 -1))

;;;###autoload
(defun helm-grep-mode-jump-other-window ()
  (interactive)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (helm-c-grep-action candidate 'other-window)
      (error nil))))

(defun helm-c-grep-persistent-action (candidate)
  "Persistent action for `helm-do-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-c-grep-action candidate nil 'mark)
      (helm-c-grep-action candidate))
  (helm-match-line-color-current-line))

(defun helm-c-grep-guess-extensions (files)
  "Try to guess file extensions in FILES list when using grep recurse.
These extensions will be added to command line with --include arg of grep."
  (loop
        with glob-list = nil
        with lst = (if (file-directory-p (car files))
                       (directory-files
                        (car files) nil
                        directory-files-no-dot-files-regexp)
                       files)
        for i in lst
        for ext = (file-name-extension i t)
        for glob = (and ext (not (string= ext ""))
                        (concat "*" ext))
        unless (or (not glob)
                   (member glob glob-list)
                   (member glob grep-find-ignored-files))
        collect glob into glob-list
        finally return glob-list))

(defun helm-grep-collect-candidates (targets recurse zgrep include-files)
  (let* ((helm-c-grep-default-command
          (cond (zgrep helm-c-default-zgrep-command)
                (recurse helm-c-grep-default-recurse-command)
                (t helm-c-grep-default-command))))
    (funcall helm-c-grep-default-function targets include-files zgrep)))

;; Internal
(defvar helm-grep-last-targets nil)
(defvar helm-grep-include-files nil)
(defvar helm-grep-in-recurse nil)
(defvar helm-grep-use-zgrep nil)
(defun helm-do-grep-1 (targets &optional recurse zgrep)
  "Launch grep on a list of TARGETS files.
When RECURSE is given use -r option of grep and prompt user
to set the --include args of grep.
You can give more than one arg separated by space.
e.g *.el *.py *.tex.
If it's empty --exclude `grep-find-ignored-files' is used instead."
  ;; When called as action from an other source e.g *-find-files
  ;; we have to kill action buffer.
  (let* ((helm-compile-source-functions
          ;; rule out helm-match-plugin because the input is one regexp.
          (delq 'helm-compile-source--match-plugin
                (copy-sequence helm-compile-source-functions)))
         (exts (helm-c-grep-guess-extensions targets))
         (globs (and (not zgrep) (mapconcat 'identity exts " ")))
         (include-files (and recurse
                             ;; zgrep will search in all files with ext matching
                             ;; `helm-zgrep-file-extension-regexp'
                             (not zgrep)
                             (read-string "OnlyExt(*.[ext]): "
                                          globs))))
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    (when include-files
      (setq include-files
            (and (not (string= include-files ""))
                 (mapconcat #'(lambda (x)
                                (concat "--include=" (shell-quote-argument x)))
                            (split-string include-files) " "))))
    (helm
     :sources
     `(((name . ,(if zgrep "Zgrep" "Grep"))
        (init . (lambda ()
                  ;; `helm-find-files' haven't already started,
                  ;; give a default value to `helm-ff-default-directory'.
                  (setq helm-ff-default-directory (or helm-ff-default-directory
                                                      default-directory))
                  ;; We need this to pass infos to `helm-resume'.
                  (setq helm-grep-last-targets targets)
                  (setq helm-grep-include-files include-files)
                  (setq helm-grep-in-recurse recurse)
                  (setq helm-grep-use-zgrep zgrep)))
        (header-name . (lambda (name)
                         (concat name "(C-c ? Help)")))
        (candidates . (lambda ()
                        (helm-grep-collect-candidates
                         helm-grep-last-targets
                         helm-grep-in-recurse
                         helm-grep-use-zgrep
                         helm-grep-include-files)))
        (filtered-candidate-transformer helm-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (nohighlight)
        (mode-line . helm-grep-mode-line-string)
        (keymap . ,helm-c-grep-map)
        (history . ,'helm-c-grep-history)
        (action . ,(delq
                    nil
                    `(("Find File" . helm-c-grep-action)
                      ("Find file other frame" . helm-c-grep-other-frame)
                      ,(and (locate-library "elscreen")
                            '("Find file in Elscreen"
                              . helm-c-grep-jump-elscreen))
                      ("Save results in grep buffer" . helm-c-grep-save-results)
                      ("Find file other window" . helm-c-grep-other-window))))
        (persistent-action . helm-c-grep-persistent-action)
        (persistent-help . "Jump to line (`C-u' Record in mark ring)")
        (requires-pattern . 3)
        (delayed)))
     :buffer (format "*helm %s*" (if zgrep "zgrep" "grep"))
     :keymap helm-c-grep-map
     :history 'helm-c-grep-history)))

(defun helm-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or helm-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir helm-c-rzgrep-cache)
                               (puthash
                                def-dir
                                (helm-c-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match helm-zgrep-file-extension-regexp)
                                helm-c-rzgrep-cache))
                           flist)))
         (when recursive (setq helm-c-zgrep-recurse-flag t))
         (helm-do-grep-1 only recursive 'zgrep))
    (setq helm-c-zgrep-recurse-flag nil)))

(defun helm-c-grep-split-line (line)
  "Split a grep output line."
  (let (beg fname lineno str)
    ;; Don't print until grep line is valid.
    (when (string-match "\\(.*\\)\\(:[0-9]+:\\)\\(.*\\)" line)
      (with-temp-buffer
        (insert line)
        (goto-char (point-min))
        (setq beg (point))
        (forward-char 2)
        (re-search-forward ":" nil t)
        (setq fname (buffer-substring-no-properties beg (1- (point))))
        (setq beg (point))
        (re-search-forward ":" nil t)
        (setq lineno (buffer-substring-no-properties beg (1- (point))))
        (setq str (buffer-substring-no-properties (point) (point-at-eol))))
      (list fname lineno str))))

(defun helm-c-grep-cand-transformer (candidates sources)
  "Filtered candidate transformer function for `helm-do-grep'."
  (loop for i in candidates
        for split  = (and i (helm-c-grep-split-line i))
        for fname  = (car split)
        for lineno = (nth 1 split)
        for str    = (nth 2 split)
        when (and fname lineno str)
        collect
        (cons (concat (propertize (file-name-nondirectory fname)
                                  'face 'helm-grep-file
                                  'help-echo fname) ":"
                                  (propertize lineno 'face 'helm-grep-lineno) ":"
                                  (helm-c-grep-highlight-match str))
              i)))

(defun helm-c-grep-highlight-match (str)
  "Highlight in string STR all occurences matching `helm-pattern'."
  (condition-case nil
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (and (re-search-forward helm-pattern nil t)
                    (> (- (match-end 0) (match-beginning 0)) 0))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           '(face helm-grep-match)))
        (buffer-string))
    (error nil)))

(defun helm-c-goto-next-or-prec-file (n)
  "Go to next or precedent candidate file in helm grep/etags buffers.
If N is positive go forward otherwise go backward."
  (let* ((current-line-list  (split-string
                              (buffer-substring
                               (point-at-bol)
                               (point-at-eol)) ":"))
         (current-fname      (nth 0 current-line-list))
         (bob-or-eof         (if (eq n 1) 'eobp 'bobp)))
    (flet ((mark-maybe ()
             (if (eq major-mode 'helm-grep-mode)
                 (ignore)
                 (helm-mark-current-line))))
      (catch 'break
        (while (not (funcall bob-or-eof))
          (forward-line n) ; Go forward or backward depending of n value.
          ;; Exit when current-fname is not matched or in `helm-grep-mode'
          ;; the line is not a grep line i.e 'fname:num:tag'.
          (unless (or (search-forward current-fname (point-at-eol) t)
                      (and (eq major-mode 'helm-grep-mode)
                           (not (get-text-property (point-at-bol) 'help-echo))))
            (mark-maybe)
            (throw 'break nil))))
      (cond ((and (> n 0) (eobp))
             (re-search-backward ".")
             (forward-line 0)
             (mark-maybe))
            ((and (< n 0) (bobp))
             (helm-aif (next-single-property-change (point-at-bol) 'help-echo)
                 (goto-char it)
             (forward-line 1))
             (mark-maybe))))))

;;;###autoload
(defun helm-c-goto-precedent-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (with-helm-window
    (helm-c-goto-next-or-prec-file -1)))

;;;###autoload
(defun helm-c-goto-next-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (with-helm-window
    (helm-c-goto-next-or-prec-file 1)))

;;;###autoload
(defun helm-c-grep-run-persistent-action ()
  "Run grep persistent action from `helm-do-grep-1'."
  (interactive)
  (helm-attrset 'jump-persistent 'helm-c-grep-persistent-action)
  (helm-execute-persistent-action 'jump-persistent))

;;;###autoload
(defun helm-c-grep-run-default-action ()
  "Run grep default action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-action))

;;;###autoload
(defun helm-c-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-other-window))

;;;###autoload
(defun helm-c-grep-run-save-buffer ()
  "Run grep save results action from `helm-do-grep-1'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-save-results))


;;; Grep buffers
;;
;;
(defun helm-c-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file--buffers or CANDIDATE if it is a file--buffer.
If one of selected buffers is not a file--buffer,
it is ignored and grep will run on all others file--buffers.
If only one candidate is selected and it is not a file--buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring non--file-buffers."
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (cands (if prefarg
                    (buffer-list)
                    (helm-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname buffers are ignored.
         (bufs (loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when fname
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (helm-do-grep-1 bufs nil 'zgrep)
            (helm-do-grep-1 bufs))
        ;; bufs is empty, thats mean we have only CANDIDATE
        ;; and it is not a buffer-filename, fallback to occur.
        (helm-c-switch-to-buffer candidate)
        (when (get-buffer helm-action-buffer)
          (kill-buffer helm-action-buffer))
        (helm-occur)
        (when (eq helm-exit-status 1)
          (set-window-configuration win-conf)))))

(defun helm-c-grep-buffers (candidate)
  "Action to grep buffers."
  (helm-c-grep-buffers-1 candidate))

(defun helm-c-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (helm-c-grep-buffers-1 candidate 'zgrep))


;;; Helm interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar helm-c-pdfgrep-default-command "pdfgrep --color never -niH %s %s")
(defvar helm-c-pdfgrep-default-function 'helm-c-pdfgrep-init)
(defvar helm-c-pdfgrep-debug-command-line nil)

(defun helm-c-pdfgrep-init (only-files)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((fnargs   (helm-c-grep-prepare-candidates
                    (if (file-remote-p helm-ff-default-directory)
                        (mapcar #'(lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                        only-files)))
         (cmd-line (format helm-c-pdfgrep-default-command
                           helm-pattern
                           fnargs)))
    (when helm-c-pdfgrep-debug-command-line
      (with-current-buffer (get-buffer-create "*any pdfgrep debug*")
        (goto-char (point-max))
        (insert (concat ">>> " cmd-line "\n\n"))))
    (setq mode-line-format
          '(" " mode-line-buffer-identification " "
            (line-number-mode "%l") " "
            (:eval (propertize "(Pdfgrep Process Running) "
                    'face '((:foreground "red"))))))
    (prog1
        (let ((default-directory helm-ff-default-directory))
          (start-file-process-shell-command "pdfgrep-process" nil cmd-line))
      (message nil)
      (set-process-sentinel
       (get-process "pdfgrep-process")
       #'(lambda (process event)
           (when (string= event "finished\n")
             (with-helm-window
               (helm-update-move-first-line))
             (force-mode-line-update nil)))))))

;; Internal
(defvar helm-pdfgrep-targets nil)
(defun helm-do-pdfgrep-1 (only)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let* ((helm-compile-source-functions
          ;; rule out helm-match-plugin because the input is one regexp.
          (delq 'helm-compile-source--match-plugin
                (copy-sequence helm-compile-source-functions))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    (helm
     :sources
     `(((name . "PdfGrep")
        (init . (lambda ()
                  ;; If `helm-find-files' haven't already started,
                  ;; give a default value to `helm-ff-default-directory'.
                  (setq helm-ff-default-directory (or helm-ff-default-directory
                                                      default-directory))
                  (setq helm-pdfgrep-targets only)))
        (candidates
         . (lambda ()
             (funcall helm-c-pdfgrep-default-function helm-pdfgrep-targets)))
        (filtered-candidate-transformer helm-c-grep-cand-transformer)
        (candidate-number-limit . 9999)
        (nohighlight)
        (history . ,'helm-c-grep-history)
        (keymap . ,helm-c-pdfgrep-map)
        (mode-line . helm-pdfgrep-mode-line-string)
        (action . helm-c-pdfgrep-action)
        (persistent-help . "Jump to PDF Page")
        (requires-pattern . 3)
        (delayed)))
     :buffer "*helm pdfgrep*"
     :history 'helm-c-grep-history)))

(defun helm-c-pdfgrep-action (candidate)
  (helm-c-grep-action candidate 'pdf))

(defun helm-c-pdfgrep-action-1 (split pageno fname)
  (save-selected-window
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec helm-c-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))

;;;###autoload
(defun helm-do-grep ()
  "Preconfigured helm for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `helm-do-grep-1'."
  (interactive)
  (let ((only    (helm-c-read-file-name
                  "Search in file(s): "
                  :marked-candidates t
                  :preselect (or (dired-get-filename nil t)
                                 (buffer-file-name (current-buffer)))))
        (prefarg (or current-prefix-arg helm-current-prefix-arg)))
    (helm-do-grep-1 only prefarg)))

;;;###autoload
(defun helm-do-zgrep ()
  "Preconfigured helm for zgrep."
  (interactive)
  (let ((prefarg (or current-prefix-arg helm-current-prefix-arg))
        (ls (helm-c-read-file-name
             "Search in file(s): "
             :marked-candidates t
             :preselect (or (dired-get-filename nil t)
                            (buffer-file-name (current-buffer))))))
    (helm-ff-zgrep-1 ls prefarg)))

;;;###autoload
(defun helm-do-pdfgrep ()
  "Preconfigured helm for pdfgrep."
  (interactive)
  (let ((only (helm-c-read-file-name
               "Search in file(s): "
               :marked-candidates t
               :test #'(lambda (file)
                         (or (string= (file-name-extension file) "pdf")
                             (string= (file-name-extension file) "PDF")
                             (file-directory-p file)))
               :preselect (or (dired-get-filename nil t)
                              (buffer-file-name (current-buffer)))))
        (helm-c-grep-default-function 'helm-c-pdfgrep-init))
    (helm-do-pdfgrep-1 only)))


(provide 'helm-grep)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-grep.el ends here
