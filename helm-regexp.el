;;; helm-regexp.el --- In buffer regexp searching and replacement for helm.

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


(defgroup helm-regexp nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-c-browse-code-regexp-lisp
  "^ *\(def\\(un\\|subst\\|macro\\|face\\|alias\\|advice\\|struct\\|\
type\\|theme\\|var\\|group\\|custom\\|const\\|method\\|class\\)"
  "Regexp used to parse lisp buffer when browsing code."
  :type 'string
  :group 'helm-regexp)

(defcustom helm-c-browse-code-regexp-python
  "\\<def\\>\\|\\<class\\>"
  "Regexp used to parse python buffer when browsing code."
  :type 'string
  :group 'helm-regexp)

(defcustom helm-c-browse-code-regexp-alist
  `((lisp-interaction-mode . ,helm-c-browse-code-regexp-lisp)
    (emacs-lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (lisp-mode . ,helm-c-browse-code-regexp-lisp)
    (python-mode . ,helm-c-browse-code-regexp-python))
  "Alist to store regexps for browsing code corresponding \
to a specific `major-mode'."
  :type 'list
  :group 'helm-regexp)


(defvar helm-occur-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-M-%") 'helm-occur-run-query-replace-regexp)
    map)
  "Keymap for `helm-occur'.")

(defvar helm-build-regexp-history nil)
(defun helm-c-query-replace-regexp (candidate)
  "Query replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp (funcall (helm-attr 'regexp))))
    (apply 'query-replace-regexp
           (helm-c-query-replace-args regexp))))

(defun helm-c-kill-regexp-as-sexp (candidate)
  "Kill regexp in a format usable in lisp code."
  (helm-c-regexp-kill-new
   (prin1-to-string (funcall (helm-attr 'regexp)))))

(defun helm-c-kill-regexp (candidate)
  "Kill regexp as it is in `helm-pattern'."
  (helm-c-regexp-kill-new (funcall (helm-attr 'regexp))))

(defun helm-c-query-replace-args (regexp)
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

(defvar helm-c-source-regexp
  '((name . "Regexp Builder")
    (init . (lambda ()
              (helm-candidate-buffer helm-current-buffer)))
    (candidates-in-buffer)
    (get-line . helm-c-regexp-get-line)
    (persistent-action . helm-c-regexp-persistent-action)
    (persistent-help . "Show this line")
    (multiline)
    (delayed)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    (regexp . (lambda () helm-input))
    (action . (("Kill Regexp as sexp" . helm-c-kill-regexp-as-sexp)
               ("Query Replace Regexp (C-u Not inside word.)"
                . helm-c-query-replace-regexp)
               ("Kill Regexp" . helm-c-kill-regexp)))))

(defun helm-c-regexp-get-line (s e)
  (propertize
   (apply 'concat
          ;; Line contents
          (format "%5d: %s" (line-number-at-pos (1- s)) (buffer-substring s e))
          ;; subexps
          (loop for i from 0 to (1- (/ (length (match-data)) 2))
                collect (format "\n         %s'%s'"
                                (if (zerop i) "Group 0: " (format "Group %d: " i))
                                (match-string i))))
   ;; match beginning
   ;; KLUDGE: point of helm-candidate-buffer is +1 than that of helm-current-buffer.
   ;; It is implementation problem of candidates-in-buffer.
   'helm-realvalue
   (1- s)))

(defun helm-c-regexp-persistent-action (pt)
  (helm-goto-char pt)
  (helm-persistent-highlight-point))

(defun helm-c-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))

;;; Occur
;;
;;
(defun helm-c-occur-init ()
  "Create the initial helm occur buffer.
If region is active use region as buffer contents
instead of whole buffer."
  (with-current-buffer (helm-candidate-buffer 'global)
    (erase-buffer)
    (let ((buf-contents
           (with-helm-current-buffer
             (if (helm-region-active-p)
                 (buffer-substring (region-beginning) (region-end))
                 (buffer-substring (point-min) (point-max))))))
      (insert buf-contents))))

(defun helm-c-occur-get-line (s e)
  (format "%7d:%s" (line-number-at-pos (1- s)) (buffer-substring s e)))

(defun helm-c-occur-query-replace-regexp (candidate)
  "Query replace regexp starting from CANDIDATE.
If region is active ignore CANDIDATE and replace only in region.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp helm-input))
    (unless (helm-region-active-p)
      (helm-c-action-line-goto candidate))
    (apply 'query-replace-regexp
           (helm-c-query-replace-args regexp))))

(defun helm-occur-run-query-replace-regexp ()
  "Run `query-replace-regexp' in helm occur from keymap."
  (interactive)
  (helm-c-quit-and-execute-action
   'helm-c-occur-query-replace-regexp))

(defvar helm-c-source-occur
  `((name . "Occur")
    (init . helm-c-occur-init)
    (candidates-in-buffer)
    (migemo)
    (get-line . helm-c-occur-get-line)
    (display-to-real . helm-c-display-to-real-line)
    (action . (("Go to Line" . helm-c-action-line-goto)
               ("Query replace regexp (C-u Not inside word.)"
                . helm-c-occur-query-replace-regexp)))
    (recenter)
    (mode-line . helm-occur-mode-line)
    (keymap . ,helm-occur-map)
    (requires-pattern . 1)
    (delayed)))


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

;;; Type attributes
;;
;;
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

;;;###autoload
(defun helm-regexp ()
  "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (let ((helm-compile-source-functions
           ;; rule out helm-match-plugin because the input is one regexp.
           (delq 'helm-compile-source--match-plugin
                 (copy-sequence helm-compile-source-functions))))
      (when (and (helm-region-active-p)
                 ;; Don't narrow to region if buffer is already narrowed.
                 (not (helm-current-buffer-narrowed-p)))
        (narrow-to-region (region-beginning) (region-end)))
      (helm :sources helm-c-source-regexp
            :buffer "*helm regexp*"
            :prompt "Regexp: "
            :history 'helm-build-regexp-history))))

;;;###autoload
(defun helm-occur ()
  "Preconfigured Helm for Occur source.
If region is active, search only in region,
otherwise search in whole buffer."
  (interactive)
  (let ((helm-compile-source-functions
         ;; rule out helm-match-plugin because the input is one regexp.
         (delq 'helm-compile-source--match-plugin
               (copy-sequence helm-compile-source-functions))))
    (helm :sources 'helm-c-source-occur
          :buffer "*Helm Occur*"
          :history 'helm-c-grep-history)))

;;;###autoload
(defun helm-browse-code ()
  "Preconfigured helm to browse code."
  (interactive)
  (helm :sources 'helm-c-source-browse-code
        :buffer "*helm browse code*"
        :default (thing-at-point 'symbol)))


(provide 'helm-regexp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-regexp.el ends here
