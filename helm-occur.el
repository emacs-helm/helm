;;; helm-occur.el --- Incremental Occur for Helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2020 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-utils)

(declare-function helm-buffers-get-visible-buffers "helm-buffers")
(declare-function helm-buffer-list "helm-buffers")
(declare-function helm-grep-split-line "helm-grep")
(declare-function helm-grep-highlight-match "helm-grep")
(declare-function helm-comp-read "helm-mode")

;;; Internals
;;
(defvar helm-source-occur nil)
(defvar helm-source-moccur nil
  "This is just a flag to add to `helm-sources-using-default-as-input'.
Don't set it to any value, it will have no effect.")
(defvar helm-occur--buffer-list nil)
(defvar helm-occur--buffer-tick nil)
(defvar helm-occur-history nil)
(defvar helm-occur--search-buffer-regexp "\\`\\([0-9]*\\)\\s-\\{1\\}\\(.*\\)\\'"
  "The regexp matching candidates in helm-occur candidate buffer.")
(defvar helm-occur-mode--last-pattern nil)


(defvar helm-occur-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")    'helm-occur-run-goto-line-ow)
    (define-key map (kbd "C-c C-o")  'helm-occur-run-goto-line-of)
    (define-key map (kbd "C-x C-s")  'helm-occur-run-save-buffer)
    map)
  "Keymap used in occur source.")

(defgroup helm-occur nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-occur-actions
  '(("Go to Line" . helm-occur-goto-line)
    ("Goto line other window (C-u vertically)" . helm-occur-goto-line-ow)
    ("Goto line new frame" . helm-occur-goto-line-of)
    ("Save buffer" . helm-occur-save-results)
    )
  "Actions for helm-occur."
  :group 'helm-occur
  :type '(alist :key-type string :value-type function))

(defcustom helm-occur-use-ioccur-style-keys nil
  "Similar to `helm-grep-use-ioccur-style-keys' but for multi occur.

Note that if you define this variable with `setq' your change will
have no effect, use customize instead."
  :group 'helm-occur
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (progn
               (define-key helm-occur-map (kbd "<right>")  'helm-occur-right)
               (define-key helm-occur-map (kbd "<left>")   'helm-occur-run-default-action))
           (define-key helm-occur-map (kbd "<right>") nil)
           (define-key helm-occur-map (kbd "<left>")  nil))))

(defcustom helm-occur-always-search-in-current nil
  "Helm multi occur always search in current buffer when non--nil."
  :group 'helm-occur
  :type 'boolean)

(defcustom helm-occur-truncate-lines t
  "Truncate lines in occur buffer when non nil."
  :group 'helm-occur
  :type 'boolean)

(defcustom helm-occur-auto-update-on-resume nil
  "Allow auto updating helm-occur buffer when outdated.
noask => Always update without asking
nil   => Don't update but signal buffer needs update
never => Never update and do not signal buffer needs update
Any other non--nil value update after confirmation."
  :group 'helm-regexp
  :type '(radio :tag "Allow auto updating helm-occur buffer when outdated."
          (const :tag "Always update without asking" noask)
          (const :tag "Never update and do not signal buffer needs update" never)
          (const :tag "Don't update but signal buffer needs update" nil)
          (const :tag "Update after confirmation" t)))

(defcustom helm-occur-candidate-number-limit 99999
  "Value of `helm-candidate-number-limit' for helm-occur."
  :group 'helm-occur
  :type 'integer)

(defcustom helm-occur-buffer-substring-fn-for-modes
  '((mu4e-headers-mode . buffer-substring))
  "Function to use to display buffer contents for major-mode.

Can be one of `buffer-substring' or `buffer-substring-no-properties'.

Note that when using `buffer-substring' initialization will be slower."
  :group 'helm-regexp
  :type '(alist :key-type (symbol :tag "Mode")
                :value-type (radio (const :tag "With text properties" buffer-substring)
                                   (const :tag "Without text properties" buffer-substring-no-properties))))


(defface helm-moccur-buffer
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "DarkTurquoise" :underline t))
  "Face used to highlight occur buffer names."
  :group 'helm-occur)

(defface helm-resume-need-update
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "red"))
  "Face used to flash occur buffer when it needs update."
  :group 'helm-occur)


;;;###autoload
(defun helm-occur ()
    "Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster."
  (interactive)
  (setq helm-source-occur
        (car (helm-occur-build-sources (list (current-buffer)) "Helm occur")))
  (helm-set-local-variable 'helm-occur--buffer-list (list (current-buffer))
                           'helm-occur--buffer-tick
                           (list (buffer-chars-modified-tick (current-buffer))))
  (save-restriction
    (let ((helm-sources-using-default-as-input
           (unless (> (buffer-size) 2000000)
             helm-sources-using-default-as-input))
          def pos)
      (when (use-region-p)
        ;; When user mark defun with `mark-defun' with intention of
        ;; using helm-occur on this region, it is relevant to use the
        ;; thing-at-point located at previous position which have been
        ;; pushed to `mark-ring'.
        (setq def (save-excursion
                    (goto-char (setq pos (car mark-ring)))
                    (helm-aif (thing-at-point 'symbol) (regexp-quote it))))
        (narrow-to-region (region-beginning) (region-end)))
      (unwind-protect
           (helm :sources 'helm-source-occur
                 :buffer "*helm occur*"
                 :history 'helm-occur-history
                 :default (or def (helm-aif (thing-at-point 'symbol)
                                      (regexp-quote it)))
                 :preselect (and (memq 'helm-source-occur
                                       helm-sources-using-default-as-input)
                                 (format "^%d:" (line-number-at-pos
                                                 (or pos (point)))))
                 :truncate-lines helm-occur-truncate-lines)
        (deactivate-mark t)))))

;;;###autoload
(defun helm-occur-visible-buffers ()
  "Run helm-occur on all visible buffers in frame."
  (interactive)
  (require 'helm-buffers)
  (if (or (one-window-p) (region-active-p))
      (call-interactively #'helm-occur)
    (let ((buffers (helm-buffers-get-visible-buffers)))
      (helm-multi-occur-1 (mapcar 'get-buffer buffers)))))

(defun helm-occur-transformer (candidates source)
  "Return CANDIDATES prefixed with line number."
  (cl-loop with buf = (helm-get-attr 'buffer-name source)
           for c in candidates collect
           (when (string-match helm-occur--search-buffer-regexp c)
             (let ((linum (match-string 1 c))
                   (disp (match-string 2 c)))
               (cons (format "%s:%s"
                             (propertize
                              linum 'face 'helm-grep-lineno
                              'help-echo (buffer-file-name
                                          (get-buffer buf)))
                             disp)
                     (string-to-number linum))))))

(defclass helm-moccur-class (helm-source-in-buffer)
  ((buffer-name :initarg :buffer-name
                :initform nil)
   (moccur-buffers :initarg :moccur-buffers
                   :initform nil)))

(defun helm-occur-build-sources (buffers &optional source-name)
  "Build sources for `helm-occur' for each buffer in BUFFERS list."
  (cl-loop for buf in buffers
           collect
           (helm-make-source (or source-name
                                 (format "HO [%s]"
                                         (buffer-name buf)))
               'helm-moccur-class
             :buffer-name (buffer-name buf)
             :match-part
             (lambda (candidate)
               ;; The regexp should match what is in candidate buffer,
               ;; not what is displayed in helm-buffer e.g. "12 foo"
               ;; and not "12:foo".
               (when (string-match helm-occur--search-buffer-regexp
                                   candidate)
                 (match-string 2 candidate)))
             :search (lambda (pattern)
                       (when (string-match "\\`\\^\\([^ ]*\\)" pattern)
                         (setq pattern (concat "^[0-9]* \\{1\\}" (match-string 1 pattern))))
                       (condition-case _err
                           (re-search-forward pattern nil t)
                         (invalid-regexp nil)))
             :init `(lambda ()
                      (with-current-buffer ,buf
                        (let* ((bsfn (or (cdr (assq
                                               major-mode
                                               helm-occur-buffer-substring-fn-for-modes))
                                         #'buffer-substring-no-properties))
                               (contents (funcall bsfn (point-min) (point-max))))
                          (helm-set-attr 'get-line bsfn)
                          (with-current-buffer (helm-candidate-buffer 'global)
                            (insert contents)
                            (goto-char (point-min))
                            (let ((linum 1))
                              (insert (format "%s " linum))
                              (while (re-search-forward "\n" nil t)
                                (cl-incf linum)
                                (insert (format "%s " linum))))))))
             :filtered-candidate-transformer 'helm-occur-transformer
             :help-message 'helm-moccur-help-message
             :nomark t
             :migemo t
             ;; Needed for resume.
             :history 'helm-occur-history
             :candidate-number-limit helm-occur-candidate-number-limit
             :action 'helm-occur-actions
             :requires-pattern 2
             :follow 1
             :group 'helm-occur
             :keymap helm-occur-map
             :resume 'helm-occur-resume-fn
             :moccur-buffers buffers)))

(defun helm-multi-occur-1 (buffers &optional input)
  "Run `helm-occur' on a list of buffers.
Each buffer's result is displayed in a separated source."
  (let* ((curbuf (current-buffer))
         (bufs (if helm-occur-always-search-in-current
                   (cons curbuf (remove curbuf buffers))
                 buffers))
         (helm-sources-using-default-as-input
           (unless (cl-loop with total_size = 0
                            for b in bufs
                            do (setq total_size (buffer-size b))
                            finally return (> total_size 2000000))
             helm-sources-using-default-as-input))
         (sources (helm-occur-build-sources bufs))
         (helm-maybe-use-default-as-input
          (not (null (memq 'helm-source-moccur
                           helm-sources-using-default-as-input)))))
    (helm-set-local-variable 'helm-occur--buffer-list bufs
                             'helm-occur--buffer-tick
                             (cl-loop for b in bufs collect
                                      (buffer-chars-modified-tick
                                       (get-buffer b))))
    (helm :sources sources
          :buffer "*helm moccur*"
          :history 'helm-occur-history
          :default (helm-aif (thing-at-point 'symbol) (regexp-quote it))
          :input input
          :truncate-lines helm-occur-truncate-lines)))

;;; Actions
;;
(cl-defun helm-occur-action (lineno
                                  &optional (method (quote buffer)))
  "Jump to line number LINENO with METHOD.
METHOD can be one of buffer, buffer-other-window, buffer-other-frame."
  (require 'helm-grep)
  (let ((buf (if (eq major-mode 'helm-occur-mode)
                 (get-text-property (point) 'buffer-name)
               (helm-get-attr 'buffer-name)))
        (split-pat (helm-mm-split-pattern helm-input)))
    (cl-case method
      (buffer              (switch-to-buffer buf))
      (buffer-other-window (helm-window-show-buffers (list buf) t))
      (buffer-other-frame  (switch-to-buffer-other-frame buf)))
    (with-current-buffer buf
      (helm-goto-line lineno)
      ;; Move point to the nearest matching regexp from bol.
      (cl-loop for reg in split-pat
               when (save-excursion
                      (condition-case _err
                          (if helm-migemo-mode
                              (helm-mm-migemo-forward reg (point-at-eol) t)
                            (re-search-forward reg (point-at-eol) t))
                        (invalid-regexp nil)))
               collect (match-beginning 0) into pos-ls
               finally (when pos-ls (goto-char (apply #'min pos-ls)))))))

(defun helm-occur-goto-line (candidate)
  "From multi occur, switch to buffer and CANDIDATE line."
  (helm-occur-action
   candidate 'buffer))

(defun helm-occur-goto-line-ow (candidate)
  "Go to CANDIDATE line in other window.
Same as `helm-occur-goto-line' but go in other window."
  (helm-occur-action
   candidate 'buffer-other-window))

(defun helm-occur-goto-line-of (candidate)
  "Go to CANDIDATE line in new frame.
Same as `helm-occur-goto-line' but go in new frame."
  (helm-occur-action
   candidate 'buffer-other-frame))

(defun helm-occur-run-goto-line-ow ()
  "Run goto line other window action from `helm-occur'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-occur-goto-line-ow)))
(put 'helm-occur-run-goto-line-ow 'helm-only t)

(defun helm-occur-run-goto-line-of ()
  "Run goto line new frame action from `helm-occur'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-occur-goto-line-of)))
(put 'helm-occur-run-goto-line-of 'helm-only t)

(defun helm-occur-run-default-action ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-occur-goto-line)))
(put 'helm-occur-run-default-action 'helm-only t)

(defun helm-occur-run-save-buffer ()
  "Run moccur save results action from `helm-moccur'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-occur-save-results)))
(put 'helm-moccur-run-save-buffer 'helm-only t)

(defun helm-occur-right ()
  "`helm-occur' action for right arrow.
This is used when `helm-occur-use-ioccur-style-keys' is enabled.
If follow is enabled (default) go to next source, otherwise execute
persistent action."
  (interactive)
  (if (helm-aand (helm-get-attr 'follow) (> it 0))
      (helm-next-source)
    (helm-execute-persistent-action)))
(put 'helm-occur-right 'helm-only t)


;;; helm-occur-mode
;;
;;
(defvar helm-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-occur-mode-goto-line)
    (define-key map (kbd "C-o")      'helm-occur-mode-goto-line-ow)
    (define-key map (kbd "<C-down>") 'helm-occur-mode-goto-line-ow-forward)
    (define-key map (kbd "<C-up>")   'helm-occur-mode-goto-line-ow-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    (define-key map (kbd "M-n")      'helm-occur-mode-goto-line-ow-forward)
    (define-key map (kbd "M-p")      'helm-occur-mode-goto-line-ow-backward)
    (define-key map (kbd "M-N")      'helm-gm-next-file)
    (define-key map (kbd "M-P")      'helm-gm-precedent-file)
    (define-key map (kbd "C-c b")    'helm-occur-mode-resume-session)
    map))

(defun helm-occur-mode-goto-line ()
  (interactive)
  (helm-aif (get-text-property (point) 'helm-realvalue)
    (progn (helm-occur-goto-line it) (helm-match-line-cleanup-pulse))))

(defun helm-occur-mode-goto-line-ow ()
  (interactive)
  (helm-aif (get-text-property (point) 'helm-realvalue)
    (progn (helm-occur-goto-line-ow it) (helm-match-line-cleanup-pulse))))

(defun helm-occur-mode-goto-line-ow-forward-1 (arg)
  (condition-case nil
      (progn
        (when (or (eq last-command 'helm-occur-mode-goto-line-ow-forward)
                  (eq last-command 'helm-occur-mode-goto-line-ow-backward))
          (forward-line arg))
        (save-selected-window
          (helm-occur-mode-goto-line-ow)
          (recenter)))
    (error nil)))

(defun helm-occur-mode-goto-line-ow-forward (arg)
  (interactive "p")
  (helm-occur-mode-goto-line-ow-forward-1 arg))

(defun helm-occur-mode-goto-line-ow-backward (arg)
  (interactive "p")
  (helm-occur-mode-goto-line-ow-forward-1 (- arg)))

(defun helm-occur-save-results (_candidate)
  "Save helm moccur results in a `helm-moccur-mode' buffer."
  (let ((buf "*hmoccur*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (helm-read-string "OccurBufferName: " buf))
      (cl-loop for b in (helm-buffer-list)
               when (and (string= new-buf b)
                         (not (y-or-n-p
                               (format "Buffer `%s' already exists overwrite? "
                                       new-buf))))
               do (setq new-buf (helm-read-string
                                 "OccurBufferName: " "*hmoccur ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (kill-all-local-variables)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap))
            buf-name)
        (erase-buffer)
        (insert "-*- mode: helm-occur -*-\n\n"
                (format "Occur Results for `%s':\n\n" helm-input))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min))
                    (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (save-excursion
          (forward-line -2)
          (while (not (eobp))
            (if (helm-pos-header-line-p)
                (let ((beg (point-at-bol))
                      (end (point-at-eol)))
                  (set-text-properties beg (1+ end) nil)
                  (delete-region (1- beg) end))
              (helm-aif (setq buf-name (assoc-default
                                        'buffer-name
                                        (get-text-property (point) 'helm-cur-source)))
                  (progn
                    (insert (propertize (concat it ":")
                                        'face 'helm-moccur-buffer
                                        'helm-realvalue (get-text-property (point) 'helm-realvalue)))
                    (add-text-properties
                     (point-at-bol) (point-at-eol)
                     `(buffer-name ,buf-name))
                    (add-text-properties
                     (point-at-bol) (point-at-eol)
                     `(keymap ,map
                              help-echo ,(concat
                                          (buffer-file-name
                                           (get-buffer buf-name))
                                          "\nmouse-1: set point\nmouse-2: jump to selection")
                              mouse-face highlight
                              invisible nil))
                    (define-key map [mouse-1] 'mouse-set-point)
                    (define-key map [mouse-2] 'helm-occur-mode-mouse-goto-line)
                    (define-key map [mouse-3] 'ignore))))
            (forward-line 1))))
      (buffer-enable-undo)
      (helm-occur-mode))
    (pop-to-buffer buf)
    (message "Helm occur Results saved in `%s' buffer" buf)))

(defun helm-occur-mode-mouse-goto-line (event)
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (pos    (posn-point (event-end event))))
    (with-selected-window window
      (when (eq major-mode 'helm-occur-mode)
        (goto-char pos)
        (helm-occur-mode-goto-line)))))
(put 'helm-moccur-mode-mouse-goto-line 'helm-only t)

(defun helm-occur-mode-resume-session ()
  (interactive)
  (cl-assert (eq major-mode 'helm-occur-mode) nil "Helm command called in wrong context")
  (helm-multi-occur-1 helm-occur--buffer-list helm-occur-mode--last-pattern))

(defun helm-occur-buffer-substring-with-linums ()
  "Return current-buffer contents as a string with all lines
numbered.  The property 'buffer-name is added to the whole string."
  (let ((bufstr (buffer-substring-no-properties (point-min) (point-max)))
        (bufname (buffer-name)))
    (with-temp-buffer
      (save-excursion
        (insert bufstr))
      (let ((linum 1))
        (insert (format "%s " linum))
        (while (re-search-forward "\n" nil t)
          (cl-incf linum)
          (insert (format "%s " linum)))
        (add-text-properties (point-min) (point-max) `(buffer-name ,bufname)))
      (buffer-string))))

(defun helm-occur-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  "The `revert-buffer-function' for `helm-occur-mode'."
  (goto-char (point-min))
  (let (pattern)
    (when (re-search-forward "^Occur Results for `\\(.*\\)'" nil t)
      (setq pattern (match-string 1))
      (forward-line 0)
      (when (re-search-forward "^$" nil t)
        (forward-line 1))
      (let ((inhibit-read-only t)
            (buffer (current-buffer))
            (buflst helm-occur--buffer-list))
        (delete-region (point) (point-max))
        (message "Reverting buffer...")
        (save-excursion
          (with-temp-buffer
            (insert
             "\n"
             (cl-loop for buf in buflst
                      for bufstr = (or (and (buffer-live-p (get-buffer buf))
                                            (with-current-buffer buf
                                              (helm-occur-buffer-substring-with-linums)))
                                       "")
                      concat bufstr)
             "\n")
            (goto-char (point-min))
            (cl-loop with linum
                     with mpart
                     ;; Bind helm-pattern used by `helm-grep-split-line'.
                     with helm-pattern = pattern
                     while (helm-mm-search pattern) ; point is at eol.
                     ;; Calculate line number (linum) and extract real
                     ;; part of line (mpart).
                     do (when (save-excursion
                                ;; `helm-mm-search' puts point at eol.
                                (forward-line 0)
                                (re-search-forward "^\\([0-9]*\\)\\s-\\{1\\}\\(.*\\)$"
                                                   (point-at-eol) t))
                          (setq linum (string-to-number (match-string 1))
                                mpart (match-string 2)))
                     ;; Match part after line number.
                     when (and mpart (string-match pattern mpart))
                     for line = (format "%s:%d:%s"
                                        (get-text-property (point) 'buffer-name)
                                        linum
                                        mpart)
                     when line
                     do (with-current-buffer buffer
                          (insert
                           (propertize
                            (car (helm-occur-filter-one-by-one line))
                            'helm-realvalue linum)
                           "\n"))))
          (when (fboundp 'wgrep-cleanup-overlays)
            (wgrep-cleanup-overlays (point-min) (point-max)))
          (message "Reverting buffer done"))))))

(defun helm-occur-filter-one-by-one (candidate)
  "`filter-one-by-one' function for `helm-source-moccur'."
  (require 'helm-grep)
  (let* ((split  (helm-grep-split-line candidate))
         (buf    (car split))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (cons (concat (propertize
                   buf
                   'face 'helm-moccur-buffer
                   'help-echo (buffer-file-name
                               (get-buffer buf))
                   'buffer-name buf)
                  ":"
                  (propertize lineno 'face 'helm-grep-lineno)
                  ":"
                  (helm-grep-highlight-match str))
          candidate)))

(define-derived-mode helm-occur-mode
    special-mode "helm-moccur"
    "Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-occur-mode-map}"
    (set (make-local-variable 'helm-occur--buffer-list)
         (with-helm-buffer helm-occur--buffer-list))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-occur-mode--revert-buffer-function)
    (set (make-local-variable 'helm-occur-mode--last-pattern)
         helm-input))
(put 'helm-moccur-mode 'helm-only t)


;;; Resume
;;
(defun helm-occur-resume-fn ()
  (with-helm-buffer
    (let (new-tick-ls buffer-is-modified)
      (set (make-local-variable 'helm-occur--buffer-list)
           (cl-loop for b in helm-occur--buffer-list
                    when (buffer-live-p (get-buffer b))
                    collect b))
      (setq buffer-is-modified (/= (length helm-occur--buffer-list)
                                   (length (helm-get-attr 'moccur-buffers))))
      (helm-set-attr 'moccur-buffers helm-occur--buffer-list)
      (setq new-tick-ls (cl-loop for b in helm-occur--buffer-list
                                 collect (buffer-chars-modified-tick
                                          (get-buffer b))))
      (when buffer-is-modified
        (setq helm-occur--buffer-tick new-tick-ls))
      (cl-assert (> (length helm-occur--buffer-list) 0) nil
                 "helm-resume error: helm-(m)occur buffer list is empty")
      (unless (eq helm-occur-auto-update-on-resume 'never)
        (when (or buffer-is-modified
                  (cl-loop for b in helm-occur--buffer-list
                           for new-tick = (buffer-chars-modified-tick
                                           (get-buffer b))
                           for tick in helm-occur--buffer-tick
                           thereis (/= tick new-tick)))
          (helm-aif helm-occur-auto-update-on-resume
              (when (or (eq it 'noask)
                        (y-or-n-p "Helm (m)occur Buffer outdated, update? "))
                (run-with-idle-timer
                 0.1 nil (lambda ()
                           (with-helm-buffer
                             (helm-force-update)
                             (message "Helm (m)occur Buffer have been udated")
                             (sit-for 1) (message nil))))
                (unless buffer-is-modified (setq helm-occur--buffer-tick
                                                 new-tick-ls)))
            (run-with-idle-timer
             0.1 nil
             (lambda ()
               (with-helm-buffer
                 (let ((ov (make-overlay (save-excursion
                                           (goto-char (point-min))
                                           (forward-line 1)
                                           (point))
                                         (point-max))))
                   (overlay-put ov 'face 'helm-resume-need-update)
                   (sit-for 0.3) (delete-overlay ov)
                   (message "[Helm occur Buffer outdated (C-c C-u to update)]")))))
            (unless buffer-is-modified
              (with-helm-after-update-hook
                (setq helm-occur--buffer-tick new-tick-ls)
                (message "Helm (m)occur Buffer have been udated")))))))))

;;; Helm occur from isearch
;;
;;;###autoload
(defun helm-occur-from-isearch ()
  "Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string)))
        (bufs (list (current-buffer))))
    (isearch-exit)
    (helm-multi-occur-1 bufs input)))

;;;###autoload
(defun helm-multi-occur-from-isearch ()
  "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'."
  (interactive)
  (let (buf-list
        helm-moccur-always-search-in-current
        (input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (setq buf-list (mapcar 'get-buffer
                           (helm-comp-read "Buffers: "
                                           (helm-buffer-list)
                                           :name "Occur in buffer(s)"
                                           :marked-candidates t)))
    (setq helm-moccur-always-search-in-current
          (if (or current-prefix-arg
                  helm-current-prefix-arg)
              (not helm-moccur-always-search-in-current)
            helm-moccur-always-search-in-current))
    (helm-multi-occur-1 buf-list input)))

(provide 'helm-occur)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-occur.el ends here
