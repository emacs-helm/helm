;;; helm-info.el --- Browse info index with helm

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
(require 'helm-plugin)
(require 'helm-vars)
(require 'helm-net)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))

;;; Info pages
(defvar helm-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-c-info-pages
      helm-c-info-pages
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
            topics)
        (require 'info)
        (with-temp-buffer
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1) topics))
          (kill-buffer))
        (setq helm-c-info-pages topics))))

(defvar helm-c-source-info-pages
  `((name . "Info Pages")
    (init . helm-c-info-pages-init)
    (candidates . helm-c-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))

;;;###autoload
(defun helm-info-at-point (arg)
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive "P")
  (let ((helm-c-google-suggest-default-function
         'helm-c-google-suggest-emacs-lisp))
    (helm :sources '(helm-c-source-info-elisp
                     helm-c-source-info-cl
                     helm-c-source-info-pages
                     helm-c-source-google-suggest)
          :input (and arg (thing-at-point 'symbol))
          :buffer "*helm info*")))

(provide 'helm-info)

;;; helm-info.el ends here
