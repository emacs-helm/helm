;;; helm-regexp.el --- In buffer regexp searching and replacement for helm.

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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


(defgroup helm-regexp nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-browse-code-regexp-lisp
  "^ *\(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|\
type\\|theme\\|var\\|group\\|custom\\|const\\|method\\|class\\)"
  "Regexp used to parse lisp buffer when browsing code."
  :type 'string
  :group 'helm-regexp)

(defcustom helm-browse-code-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "Regexp used to parse python buffer when browsing code."
  :type 'string
  :group 'helm-regexp)

(defcustom helm-browse-code-regexp-alist
  `((lisp-interaction-mode . ,helm-browse-code-regexp-lisp)
    (emacs-lisp-mode . ,helm-browse-code-regexp-lisp)
    (lisp-mode . ,helm-browse-code-regexp-lisp)
    (python-mode . ,helm-browse-code-regexp-python))
  "Alist to store regexps for browsing code corresponding \
to a specific `major-mode'."
  :type '(alist :key-type symbol :value-type regexp)
  :group 'helm-regexp)

(defcustom helm-browse-code-fontify t
  "Fontify `current-buffer' when `helm-browse-code' start.
Slow on large buffers."
  :type 'boolean
  :group 'helm-regexp)

(defcustom helm-moccur-always-search-in-current nil
  "Helm multi occur always search in current buffer when non--nil."
  :group 'helm-regexp
  :type 'boolean)

(defcustom helm-m-occur-idle-delay helm-idle-delay
  "Delay before updating display in `helm-source-moccur'.
It is similar to `helm-idle-delay' but local to `helm-source-moccur'.
This setting apply also to `helm-source-occur'."
  :group 'helm-regexp
  :type 'float)

(defcustom helm-m-occur-use-ioccur-style-keys t
  "Similar to `helm-grep-use-ioccur-style-keys' but for multi occur."
  :group 'helm-regexp
  :type 'boolean)


(defface helm-moccur-buffer
    '((t (:foreground "DarkTurquoise" :underline t)))
  "Face used to highlight moccur buffer names."
  :group 'helm-regexp)


(defvar helm-moccur-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-moccur-help)
    (define-key map (kbd "C-c o")    'helm-m-occur-run-goto-line-ow)
    (define-key map (kbd "C-c C-o")  'helm-m-occur-run-goto-line-of)
    (when helm-m-occur-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-m-occur-run-persistent-action)
      (define-key map (kbd "<left>")   'helm-m-occur-run-default-action))
    (delq nil map))
  "Keymap used in Moccur source.")


(defvar helm-build-regexp-history nil)
(defun helm-query-replace-regexp (candidate)
  "Query replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp (funcall (helm-attr 'regexp))))
    (apply 'query-replace-regexp
           (helm-query-replace-args regexp))))

(defun helm-kill-regexp-as-sexp (candidate)
  "Kill regexp in a format usable in lisp code."
  (helm-regexp-kill-new
   (prin1-to-string (funcall (helm-attr 'regexp)))))

(defun helm-kill-regexp (candidate)
  "Kill regexp as it is in `helm-pattern'."
  (helm-regexp-kill-new (funcall (helm-attr 'regexp))))

(defun helm-query-replace-args (regexp)
  "create arguments of `query-replace-regexp' action in `helm-regexp'."
  (let ((region-only (helm-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace %sregexp %s"
                                    (if helm-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     helm-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defvar helm-source-regexp
  '((name . "Regexp Builder")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global (with-temp-buffer
                         (insert-buffer-substring helm-current-buffer)
                         (buffer-string)))))
    (candidates-in-buffer)
    (get-line . helm-regexp-get-line)
    (persistent-action . helm-regexp-persistent-action)
    (persistent-help . "Show this line")
    (multiline)
    (no-matchplugin)
    (delayed)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    (regexp . (lambda () helm-input))
    (action . (("Kill Regexp as sexp" . helm-kill-regexp-as-sexp)
               ("Query Replace Regexp (C-u Not inside word.)"
                . helm-query-replace-regexp)
               ("Kill Regexp" . helm-kill-regexp)))))

(defun helm-regexp-get-line (s e)
  (let ((matches (match-data))
        (line    (buffer-substring s e)))
    (propertize
     (loop with ln = (format "%5d: %s" (line-number-at-pos (1- s)) line)
           for i from 0 to (1- (/ (length matches) 2))
           concat (format "\n         %s'%s'" (format "Group %d: " i)
                          (match-string i)) into ln1
           finally return (concat ln ln1))
     ;; match beginning
     ;; KLUDGE: point of helm-candidate-buffer is +1 than that of helm-current-buffer.
     ;; It is implementation problem of candidates-in-buffer.
     'helm-real-value (1- s))))

(defun helm-regexp-persistent-action (pt)
  (helm-goto-char pt)
  (helm-persistent-highlight-point))

(defun helm-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))


;;; Occur
;;
;;
(defvar helm-source-occur nil)
(defun helm-occur-init-source ()
  (unless helm-source-occur
    (setq helm-source-occur (copy-alist helm-source-moccur))))


;;; Multi occur
;;
;;

;; Internal
(defvar helm-multi-occur-buffer-list nil)

(defun helm-m-occur-init ()
  "Create the initial helm multi occur buffer with BUFFERS list."
  (set (make-local-variable 'helm-multi-occur-buffer-list)
       (if helm-moccur-always-search-in-current
           (cons helm-current-buffer
                 (remove helm-current-buffer helm-multi-occur-buffer-list))
           helm-multi-occur-buffer-list))
  (helm-init-candidates-in-buffer
   'global
   (loop for buf in helm-multi-occur-buffer-list
         for bufstr = (with-current-buffer buf (buffer-string))
         do (add-text-properties
             0 (length bufstr)
             `(buffer-name ,(buffer-name (get-buffer buf)))
             bufstr)
         concat bufstr)))

(defun helm-m-occur-get-line (beg end)
  "Format line for `helm-source-moccur'."
  (format "%s:%d:%s"
          (get-text-property beg 'buffer-name)
          (save-restriction
            (narrow-to-region (previous-single-property-change
                               (point) 'buffer-name)
                              (next-single-property-change
                               (point) 'buffer-name))
            (line-number-at-pos beg))
          (buffer-substring beg end)))

(defun* helm-m-occur-action (candidate
                             &optional (method (quote buffer)) mark)
  "Jump to CANDIDATE with METHOD.
arg METHOD can be one of buffer, buffer-other-window, buffer-other-frame."
  (require 'helm-grep)
  (let* ((split (helm-grep-split-line candidate))
         (buf (car split))
         (lineno (string-to-number (nth 1 split))))
    (case method
      (buffer              (switch-to-buffer buf))
      (buffer-other-window (switch-to-buffer-other-window buf))
      (buffer-other-frame  (switch-to-buffer-other-frame buf)))
    (helm-goto-line lineno)
    (when (re-search-forward helm-pattern (point-at-eol) t)
      (goto-char (match-beginning 0)))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))))

(defun helm-m-occur-persistent-action (candidate)
  (helm-m-occur-goto-line candidate)
  (helm-match-line-color-current-line))

(defun helm-m-occur-run-persistent-action ()
  (interactive)
  (when helm-alive-p
    (helm-execute-persistent-action)))

(defun helm-m-occur-goto-line (candidate)
  "From multi occur, switch to buffer and go to nth 1 CANDIDATE line."
  (helm-m-occur-action
   candidate 'buffer (or current-prefix-arg         ; persistent.
                         helm-current-prefix-arg))) ; exit.
                         
(defun helm-m-occur-goto-line-ow (candidate)
  "Go to CANDIDATE line in other window.
Same as `helm-m-occur-goto-line' but go in other window."
  (helm-m-occur-action
   candidate 'buffer-other-window
   (or current-prefix-arg         ; persistent.
       helm-current-prefix-arg))) ; exit.

(defun helm-m-occur-goto-line-of (candidate)
  "Go to CANDIDATE line in new frame.
Same as `helm-m-occur-goto-line' but go in new frame."
  (helm-m-occur-action
   candidate 'buffer-other-frame
   (or current-prefix-arg         ; persistent.
       helm-current-prefix-arg))) ; exit.

(defun helm-m-occur-run-goto-line-ow ()
  "Run goto line other window action from `helm-source-moccur'."
  (interactive)
  (when helm-alive-p
    (helm-quit-and-execute-action 'helm-m-occur-goto-line-ow)))

(defun helm-m-occur-run-goto-line-of ()
  "Run goto line new frame action from `helm-source-moccur'."
  (interactive)
  (when helm-alive-p
    (helm-quit-and-execute-action 'helm-m-occur-goto-line-of)))

(defun helm-m-occur-run-default-action ()
  (interactive)
  (helm-quit-and-execute-action 'helm-m-occur-goto-line))

(defvar helm-source-moccur
  `((name . "Moccur")
    (init . (lambda ()
              (require 'helm-grep)
              (helm-m-occur-init)
              (helm-attrset 'delayed helm-m-occur-idle-delay)))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-m-occur-transformer)
    (nohighlight)
    (get-line . helm-m-occur-get-line)
    (migemo)
    (action . (("Go to Line" . helm-m-occur-goto-line)
               ("Goto line other window" . helm-m-occur-goto-line-ow)
               ("Goto line new frame" . helm-m-occur-goto-line-of)))
    (persistent-action . helm-m-occur-persistent-action)
    (persistent-help . "Go to line")
    (recenter)
    (no-matchplugin)
    (candidate-number-limit . 9999)
    (mode-line . helm-moccur-mode-line)
    (keymap . ,helm-moccur-map)
    (history . ,'helm-grep-history)
    (requires-pattern . 2)
    (delayed . ,helm-m-occur-idle-delay))
  "Helm source for multi occur.")

(defun helm-m-occur-transformer (candidates source)
  "Transformer function for `helm-source-moccur'."
  (require 'helm-grep)
  (loop for i in candidates
        for split = (helm-grep-split-line i)
        for buf = (car split)
        for lineno = (nth 1 split)
        for str = (nth 2 split)
        collect (cons (concat (propertize
                               buf
                               'face 'helm-moccur-buffer
                               'help-echo (buffer-file-name
                                           (get-buffer buf))
                               'buffer-name buf)
                              ":"
                              (propertize lineno 'face 'helm-grep-lineno)
                              ":"
                              (helm-grep-highlight-match str))
                      i)))

(defun helm-multi-occur-1 (buffers &optional input)
  "Main function to call `helm-source-moccur' with BUFFERS list."
  (setq helm-multi-occur-buffer-list buffers)
  (helm :sources 'helm-source-moccur
        :buffer "*helm multi occur*"
        :history 'helm-grep-history
        :input input))


;;; Helm browse code.
;;
;;
(defun helm-browse-code-get-line (beg end)
  "Select line if it match the regexp corresponding to current `major-mode'.
Line is parsed for BEG position to END position."
  (let ((str-line (buffer-substring beg end))
        (regexp   (with-helm-current-buffer
                    (assoc-default major-mode helm-browse-code-regexp-alist)))
        (num-line (if (string= helm-pattern "") beg (1- beg))))
    (when (and regexp (string-match regexp str-line))
      (format "%4d:%s" (line-number-at-pos num-line) str-line))))

(defvar helm-source-browse-code
  '((name . "Browse code")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global (with-temp-buffer
                         (insert-buffer-substring helm-current-buffer)
                         (buffer-string)))
              (when helm-browse-code-fontify
                (with-helm-current-buffer
                  (jit-lock-fontify-now)))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (get-line . helm-browse-code-get-line)
    (type . line)
    (recenter)))

(defun helm-display-to-real-numbered-line (candidate)
  "This is used to display a line in occur style in helm sources.
e.g \"    12:some_text\".
It is used with type attribute 'line'."
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate))
            (match-string 2 candidate))
      (error "Line number not found")))

;;; Type attributes
;;
;;
(define-helm-type-attribute 'line
    '((display-to-real . helm-display-to-real-numbered-line)
      (action ("Go to Line" . helm-action-line-goto)))
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
    `((filtered-candidate-transformer helm-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . helm-action-file-line-goto)))
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

;;;###autoload
(defun helm-regexp ()
  "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (when (and (helm-region-active-p)
               ;; Don't narrow to region if buffer is already narrowed.
               (not (helm-current-buffer-narrowed-p (current-buffer))))
      (narrow-to-region (region-beginning) (region-end)))
    (helm :sources helm-source-regexp
          :buffer "*helm regexp*"
          :prompt "Regexp: "
          :history 'helm-build-regexp-history)))

;;;###autoload
(defun helm-occur ()
  "Preconfigured helm for Occur."
  (interactive)
  (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
  (helm-occur-init-source)
  (helm-attrset 'name "Occur" helm-source-occur)
  (helm :sources 'helm-source-occur
        :buffer "*helm occur*"
        :history 'helm-grep-history))

;;;###autoload
(defun helm-occur-from-isearch ()
  "Invoke `helm-occur' from isearch."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                   (regexp-quote isearch-string))))
    (isearch-exit)
    (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
    (helm-occur-init-source)
    (helm-attrset 'name "Occur" helm-source-occur)
    (helm :sources 'helm-source-occur
          :buffer "*helm occur*"
          :history 'helm-grep-history
          :input input)))

;;;###autoload
(defun helm-multi-occur (buffers)
  "Preconfigured helm for multi occur.

  BUFFERS is a list of buffers to search through.
With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling `helm-multi-occur'
or during the buffer selection."
  (interactive (list (helm-comp-read
                      "Buffers: " (helm-buffer-list)
                      :marked-candidates t)))
  (let ((helm-moccur-always-search-in-current
         (if (or current-prefix-arg
                 helm-current-prefix-arg)
             (not helm-moccur-always-search-in-current)
             helm-moccur-always-search-in-current)))
    (helm-multi-occur-1 buffers)))

;;;###autoload
(defun helm-multi-occur-from-isearch (&optional arg)
  "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection."
  (interactive "p")
  (let ((helm-moccur-always-search-in-current
         (if (or current-prefix-arg
                 helm-current-prefix-arg)
             (not helm-moccur-always-search-in-current)
             helm-moccur-always-search-in-current))
        (input (if isearch-regexp
                   isearch-string
                   (regexp-quote isearch-string))))
    (isearch-exit)
    (helm-multi-occur-1
     (helm-comp-read "Buffers: " (helm-buffer-list) :marked-candidates t)
     input)))

;;;###autoload
(defun helm-browse-code ()
  "Preconfigured helm to browse code."
  (interactive)
  (helm :sources 'helm-source-browse-code
        :buffer "*helm browse code*"
        :default (thing-at-point 'symbol)))


(provide 'helm-regexp)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-regexp.el ends here
