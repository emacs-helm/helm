;;; helm-buffers.el --- helm support for buffers.

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

(require 'cl)
(require 'helm)
(require 'helm-utils)
(require 'helm-elscreen)
(require 'helm-grep)
(require 'helm-regexp)

(declare-function ido-make-buffer-list "ido" (default))

(defgroup helm-buffers nil
  "Buffers related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-c-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf")
  "The regexp list that match boring buffers.
Buffer candidates matching these regular expression will be
filtered from the list of candidates if the
`helm-c-skip-boring-buffers' candidate transformer is used."
  :type  '(repeat (choice regexp))
  :group 'helm-buffers)

(defcustom helm-buffers-favorite-modes '(lisp-interaction-mode
                                         emacs-lisp-mode
                                         text-mode
                                         org-mode)
  "List of preferred mode to open new buffers with."
  :type '(repeat (choice function))
  :group 'helm-buffers)

(defcustom helm-buffer-max-length 20
  "Max length of buffer names before truncate."
  :group 'helm-buffers
  :type  'integer)

;;; Faces
;;
;;
(defface helm-buffer-saved-out
    '((t (:foreground "red" :background "black")))
  "Face used for buffer files modified outside of emacs."
  :group 'helm-buffers)

(defface helm-buffer-not-saved
    '((t (:foreground "Indianred2")))
  "Face used for buffer files not already saved on disk."
  :group 'helm-buffers)

(defface helm-buffer-size
  '((((background dark)) :foreground "RosyBrown")
    (((background light)) :foreground "SlateGray"))
  "Face used for buffer size."
  :group 'helm-buffers)


;;; Buffers keymap
;;
(defvar helm-c-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")     'helm-c-buffer-help)
    ;; No need to have separate command for grep and zgrep
    ;; as we don't use recursivity for buffers.
    ;; So use zgrep for both as it is capable to handle non--compressed files.
    (define-key map (kbd "M-g s")     'helm-buffer-run-zgrep)
    (define-key map (kbd "C-s")       'helm-buffers-run-multi-occur)
    (define-key map (kbd "C-c o")     'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   'helm-buffer-switch-other-frame)
    (define-key map (kbd "C-c =")     'helm-buffer-run-ediff)
    (define-key map (kbd "M-=")       'helm-buffer-run-ediff-merge)
    (define-key map (kbd "C-=")       'helm-buffer-diff-persistent)
    (define-key map (kbd "M-U")       'helm-buffer-revert-persistent)
    (define-key map (kbd "M-D")       'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-x C-s")   'helm-buffer-save-persistent)
    (define-key map (kbd "C-M-%")     'helm-buffer-run-query-replace-regexp)
    (define-key map (kbd "M-%")       'helm-buffer-run-query-replace)
    (define-key map (kbd "M-m")       'helm-toggle-all-marks)
    (define-key map (kbd "M-a")       'helm-mark-all)
    (when (locate-library "elscreen")
      (define-key map (kbd "<C-tab>") 'helm-buffer-switch-to-elscreen))
    (delq nil map))
  "Keymap for buffer sources in helm.")

(defvar helm-buffers-ido-virtual-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")   'helm-buffers-ido-virtual-help)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-ff-run-switch-other-frame)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    map))

(defvar helm-buffers-list-cache nil)
(defvar helm-c-source-buffers-list
  `((name . "Buffers")
    (init . (lambda ()
              ;; Issue #51 Create the list before `helm-buffer' creation.
              (setq helm-buffers-list-cache (helm-c-buffer-list))))
    (candidates . helm-buffers-list-cache)
    (type . buffer)
    (match helm-c-buffer-match-major-mode)
    (persistent-action . helm-c-buffers-list-persistent-action)
    (keymap . ,helm-c-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvar helm-c-source-buffer-not-found
  `((name . "Create buffer")
    (dummy)
    (filtered-candidate-transformer (lambda (cands source)
                                      (list helm-pattern)))
    (keymap . ,helm-map)
    (action . (lambda (candidate)
                (let ((mjm (and helm-current-prefix-arg
                                (intern (helm-comp-read
                                         "Major-mode: "
                                         helm-buffers-favorite-modes))))
                      buffer)
                  (setq buffer (get-buffer-create candidate)) 
                  (if mjm
                      (with-current-buffer buffer (funcall mjm))
                      (set-buffer-major-mode buffer))
                  (helm-c-switch-to-buffer buffer))))))

(defvar helm-c-source-ido-virtual-buffers
  `((name . "Ido virtual buffers")
    (candidates . (lambda ()
                    (let (ido-temp-list
                          ido-ignored-list
                          (ido-process-ignore-lists t))
                    (when ido-use-virtual-buffers
                      (ido-add-virtual-buffers-to-list)
                      ido-virtual-buffers))))
    (keymap . ,helm-buffers-ido-virtual-map)
    (mode-line . helm-buffers-ido-virtual-mode-line-string)
    (action . (("Find file" . helm-find-many-files)
               ("Find file other window" . find-file-other-window)
               ("Find file other frame" . find-file-other-frame)
               ("Find file as root" . helm-find-file-as-root)
               ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
               ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
               ("View file" . view-file)
               ("Delete file(s)" . helm-delete-marked-files)
               ("Open file externally (C-u to choose)"
                . helm-c-open-file-externally)))))

(defun helm-c-buffer-list ()
  "Return the current list of buffers.
Currently visible buffers are put at the end of the list.
See `ido-make-buffer-list' for more infos."
  (require 'ido)
  (let ((ido-process-ignore-lists t)
        ido-ignored-list
        ido-use-virtual-buffers)
    (ido-make-buffer-list nil)))

(defun helm-buffer-size (buffer)
  "Return size of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (helm-file-human-size
       (- (position-bytes (point-max))
          (position-bytes (point-min)))))))

(defun helm-c-highlight-buffers (buffers sources)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers)."
  (loop ;; length of last buffer size string.
        ;; Start at ten, such a length should never be reach.
        ;; e.g 9999K, so the max should be 5 + a space = 6.
        with old-len-size = 10
        for i in buffers
        for buf = (get-buffer i)
        for size = (propertize (helm-buffer-size buf)
                               'face 'helm-buffer-size)
        for len-size = (length size)
        for str-before-size = (helm-aif (and (> old-len-size len-size)
                                             (- old-len-size len-size))
                                  (make-string it ? ) "")
        do (setq old-len-size (+ len-size (length str-before-size)))
        for truncbuf = (if (> (length i) helm-buffer-max-length)
                           (concat (substring i 0 helm-buffer-max-length)
                                   "...")
                           (concat i (make-string
                                      (- (+ helm-buffer-max-length 3)
                                         (length i)) ? )))
        for bfname = (buffer-file-name buf)
        for mode = (with-current-buffer i (symbol-name major-mode))
        collect
        (cond (;; A dired buffer.
               (rassoc buf dired-buffers)
               (cons (concat (propertize
                              truncbuf 'face 'helm-ff-directory
                              'help-echo (car (rassoc buf dired-buffers)))
                             " " str-before-size size "  " mode)
                     i))
              ;; A buffer file modified somewhere outside of emacs.
              ((and bfname (not (file-remote-p bfname))
                    (file-exists-p bfname)
                    (not (verify-visited-file-modtime buf)))
               (cons (concat (propertize truncbuf 'face 'helm-buffer-saved-out
                                         'help-echo bfname)
                             " " str-before-size size "  " mode)
                     i))
              ;; A new buffer file not already saved on disk.
              ((and bfname (not (file-remote-p bfname))
                    (not (verify-visited-file-modtime buf)))
               (cons (concat (propertize truncbuf 'face 'helm-buffer-not-saved
                                         'help-echo bfname)
                             " " str-before-size size "  " mode)
                     i))
              ;; A Remote buffer file modified and not saved on disk.
              ((and bfname (file-remote-p bfname) (buffer-modified-p buf))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'helm-ff-prefix))))
                 (cons (concat prefix (propertize truncbuf 'face 'helm-ff-symlink
                                                  'help-echo bfname)
                               " " str-before-size size "  " mode)
                       i)))
              ;; A buffer file modified and not saved on disk.
              ((and bfname (buffer-modified-p buf))
               (cons (concat (propertize truncbuf 'face 'helm-ff-symlink
                                         'help-echo bfname)
                             " " str-before-size size "  " mode)
                     i))
              ;; A remote buffer file not modified and saved on disk.
              ((and bfname (file-remote-p bfname))
               (let ((prefix (propertize
                              " " 'display
                              (propertize "@ " 'face 'helm-ff-prefix))))
                 (cons (concat prefix (propertize truncbuf 'face 'font-lock-type-face
                                                  'help-echo bfname)
                               " " str-before-size size "  " mode)
                       i)))
              ;; A buffer file not modified and saved on disk.
              (bfname
               (cons (concat (propertize truncbuf 'face 'font-lock-type-face
                                         'help-echo bfname)
                             " " str-before-size size "  " mode)
                     i))
              ;; Any non--file buffer.
              (t (cons (concat (propertize truncbuf 'face 'italic
                                           'help-echo i)
                               " " str-before-size size "  " mode) i)))))

(defun helm-c-buffer-match-major-mode (candidate)
  "Match maybe buffer by major-mode.
If you give a major-mode or partial major-mode,
it will list all buffers of this major-mode and/or buffers with name
matching this major-mode.
If you add a space after major-mode and then a space,
it will match all buffers of the major-mode
before space matching pattern after space.
If you give a pattern which doesn't match a major-mode, it will search buffer
with name matching pattern."
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand)))
    (when buf
      (with-current-buffer buf
        (let ((mjm   (symbol-name major-mode))
              (split (split-string helm-pattern)))
          (cond ((string-match "^@" helm-pattern)
                 (or (helm-buffers-match-inside cand split)
                     (string-match helm-pattern cand)))
                ((string-match "\\s-$" helm-pattern)
                 (string-match (car split) mjm))
                ((string-match "\\s-[@]" helm-pattern)
                 (and (string-match (car split) mjm)
                      (helm-buffers-match-inside cand (cdr split))))
                ((string-match "\\s-" helm-pattern)
                 (and (string-match (car split) mjm)
                      (loop for i in (cdr split) always (string-match i cand))))
                (t (or (string-match helm-pattern mjm)
                       (string-match helm-pattern cand)))))))))

(defun helm-buffers-match-inside (candidate lst)
  (loop for i in lst
        always
        (cond ((string-match "\\`[\\]@" i)
               (string-match i candidate))
              ((string-match "\\`@\\(.*\\)" i)
               (save-excursion
                 (let ((str (match-string 1 i)))
                   (goto-char (point-min))
                   (re-search-forward str nil t))))
              (t (string-match i candidate)))))

(defun helm-c-buffer-query-replace-1 (&optional regexp-flag)
  "Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
  (let ((fn     (if regexp-flag 'query-replace-regexp 'query-replace))
        (prompt (if regexp-flag "Query replace regexp" "Query replace"))
        (bufs   (helm-marked-candidates)))
    (loop with replace = (query-replace-read-from prompt regexp-flag)
          with tostring = (unless (consp replace)
                            (query-replace-read-to
                             replace prompt regexp-flag))
          for buf in bufs
          do
          (save-window-excursion
            (helm-c-switch-to-buffer buf)
            (save-excursion
              (let ((case-fold-search t))
                (goto-char (point-min))
                (if (consp replace)
                    (apply fn (list (car replace) (cdr replace)))
                    (apply fn (list replace tostring)))))))))

(defun helm-c-buffer-query-replace-regexp (candidate)
  (helm-c-buffer-query-replace-1 'regexp))

(defun helm-c-buffer-query-replace (candidate)
  (helm-c-buffer-query-replace-1))

(defun helm-buffer-toggle-diff (candidate)
  "Toggle diff buffer CANDIDATE with it's file."
  (helm-aif (get-buffer-window "*Diff*")
      (progn (kill-buffer "*Diff*")
             (set-window-buffer it helm-current-buffer))
    (diff-buffer-with-file (get-buffer candidate))))

;;;###autoload
(defun helm-buffer-diff-persistent ()
  "Toggle diff buffer without quitting helm."
  (interactive)
  (helm-attrset 'diff-action 'helm-buffer-toggle-diff)
  (helm-execute-persistent-action 'diff-action))

(defun helm-buffer-revert-and-update (candidate)
  (let ((marked (helm-marked-candidates)))
    (loop for buf in marked do (helm-revert-buffer buf))
    (helm-force-update candidate)))

;;;###autoload
(defun helm-buffer-revert-persistent ()
  "Revert buffer without quitting helm."
  (interactive)
  (helm-attrset 'revert-action 'helm-buffer-revert-and-update)
  (helm-execute-persistent-action 'revert-action))

(defun helm-buffer-save-and-update (candidate)
  (let ((marked (helm-marked-candidates))
        (enable-recursive-minibuffers t))
    (loop for buf in marked do
          (with-current-buffer (get-buffer buf)
            (save-buffer)))
    (helm-force-update candidate)))

;;;###autoload
(defun helm-buffer-save-persistent ()
  "Save buffer without quitting helm."
  (interactive)
  (helm-attrset 'save-action 'helm-buffer-save-and-update)
  (helm-execute-persistent-action 'save-action))

;;;###autoload
(defun helm-buffer-run-kill-buffers ()
  "Run kill buffer action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-kill-marked-buffers))

;;;###autoload
(defun helm-buffer-run-grep ()
  "Run Grep action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-grep-buffers))

;;;###autoload
(defun helm-buffer-run-zgrep ()
  "Run Grep action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-zgrep-buffers))

;;;###autoload
(defun helm-buffer-run-query-replace-regexp ()
  "Run Query replace regexp action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-buffer-query-replace-regexp))

;;;###autoload
(defun helm-buffer-run-query-replace ()
  "Run Query replace action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-c-buffer-query-replace))

;;;###autoload
(defun helm-buffer-switch-other-window ()
  "Run switch to other window action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'switch-to-buffer-other-window))

;;;###autoload
(defun helm-buffer-switch-other-frame ()
  "Run switch to other frame action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'switch-to-buffer-other-frame))

;;;###autoload
(defun helm-buffer-switch-to-elscreen ()
  "Run switch to elscreen  action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-find-buffer-on-elscreen))

;;;###autoload
(defun helm-buffer-run-ediff ()
  "Run ediff action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-ediff-marked-buffers))

(defun helm-buffer-run-ediff-merge ()
  "Run ediff action from `helm-c-source-buffers-list'."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-ediff-marked-buffers-merge))

(defun helm-c-buffers-persistent-kill (buffer)
  "Persistent action to kill buffer."
  (with-current-buffer (get-buffer buffer)
    (if (and (buffer-modified-p)
             (buffer-file-name (current-buffer)))
        (progn
          (save-buffer)
          (kill-buffer buffer))
        (kill-buffer buffer)))
  (helm-delete-current-selection)
  (when (helm-empty-source-p) (helm-force-update)))

(defun helm-c-buffers-list-persistent-action (candidate)
  (if current-prefix-arg
      (helm-c-buffers-persistent-kill candidate)
      (helm-c-switch-to-buffer candidate)))

(defun helm-ediff-marked-buffers (candidate &optional merge)
  "Ediff 2 marked buffers or CANDIDATE and `helm-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
  (let ((lg-lst (length (helm-marked-candidates)))
        buf1 buf2)
    (case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 helm-current-buffer
             buf2 (first (helm-marked-candidates))))
      (2
       (setq buf1 (first (helm-marked-candidates))
             buf2 (second (helm-marked-candidates))))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
        (ediff-buffers buf1 buf2))))

(defun helm-ediff-marked-buffers-merge (candidate)
  "Ediff merge `helm-current-buffer' with CANDIDATE.
See `helm-ediff-marked-buffers'."
  (helm-ediff-marked-buffers candidate t))

(defun helm-multi-occur-as-action (_candidate)
  "Multi occur action for `helm-c-source-buffers-list'.
Can be used by any source that list buffers."
  (let ((helm-moccur-always-search-in-current
         (or helm-moccur-always-search-in-current
             helm-current-prefix-arg))
        (buffers (helm-marked-candidates))
        (input (loop for i in (split-string helm-pattern " " t)
                     thereis (and (string-match "\\`@\\(.*\\)" i)
                                  (match-string 1 i)))))
    (helm-multi-occur-1 buffers input)))

;;;###autoload
(defun helm-buffers-run-multi-occur ()
  "Run `helm-multi-occur-as-action' by key."
  (interactive)
  (helm-c-quit-and-execute-action 'helm-multi-occur-as-action))

;;; Candidate Transformers
;;
;;
(defun helm-c-skip-boring-buffers (buffers sources)
  (helm-skip-entries buffers helm-c-boring-buffer-regexp-list))

(defun helm-c-shadow-boring-buffers (buffers)
  "Buffers matching `helm-c-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-shadow-entries buffers helm-c-boring-buffer-regexp-list))

(defun helm-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (or (buffer-modified-p)
              (not (verify-visited-file-modtime
                    (get-buffer candidate))))
      (revert-buffer t t))))

(defun helm-revert-marked-buffers (ignore)
  (mapc 'helm-revert-buffer (helm-marked-candidates)))

(defun helm-kill-marked-buffers (ignore)
  (mapc 'kill-buffer (helm-marked-candidates)))

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
       ("Multi occur buffer(s)" . helm-multi-occur-as-action)
       ("Revert buffer(s)" . helm-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . helm-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . helm-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (helm-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (filtered-candidate-transformer helm-c-skip-boring-buffers
                                      helm-c-highlight-buffers))
  "Buffer or buffer name.")

;;;###autoload
(defun helm-buffers-list ()
  "Preconfigured `helm' to list buffers.
It is an enhanced version of `helm-for-buffers'."
  (interactive)
  (helm :sources '(helm-c-source-buffers-list
                   helm-c-source-ido-virtual-buffers
                   helm-c-source-buffer-not-found)
        :buffer "*helm buffers*" :keymap helm-c-buffer-map))

(provide 'helm-buffers)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-buffers.el ends here
