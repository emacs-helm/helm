;;; helm-eval.el --- eval expressions from helm.

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
(require 'eldoc)


(defgroup helm-eval nil
  "Eval related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-c-eldoc-in-minibuffer-show-fn
  'helm-c-show-info-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display."
  :group 'helm-eval
  :type  'symbol)

(defcustom helm-c-show-info-in-mode-line-delay 12
  "Eldoc will show info in mode-line during this delay if user is idle."
  :type  'integer
  :group 'helm-eval)


(declare-function eldoc-current-symbol "eldoc")
(declare-function eldoc-get-fnsym-args-string "eldoc" (sym &optional index))
(declare-function eldoc-get-var-docstring "eldoc" (sym))
(declare-function eldoc-fnsym-in-current-sexp "eldoc")

;;; Evaluation Result
;;
;;
;; Internal
(defvar helm-eldoc-active-minibuffers-list nil)

(defvar helm-eval-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-eval-new-line-and-indent)
    (define-key map (kbd "<tab>")      'lisp-indent-line)
    (define-key map (kbd "<C-tab>")    'lisp-complete-symbol)
    (define-key map (kbd "C-p")        'previous-line)
    (define-key map (kbd "C-n")        'next-line)
    (define-key map (kbd "<up>")       'previous-line)
    (define-key map (kbd "<down>")     'next-line)
    (define-key map (kbd "<right>")    'forward-char)
    (define-key map (kbd "<left>")     'backward-char)
    map))

(defvar helm-c-source-evaluation-result
  '((name . "Evaluation Result")
    (dummy)
    (multiline)
    (mode-line . "C-RET: nl-and-indent, tab: reindent, C-tab:complete, C-p/n: next/prec-line.")
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (with-helm-current-buffer
                                               (pp-to-string
                                                (eval (read helm-pattern))))
                                           (error "Error")))))
    (action . (("Copy result to kill-ring" . (lambda (candidate)
                                               (with-current-buffer helm-buffer
                                                 (let ((end (save-excursion
                                                              (goto-char (point-max))
                                                              (search-backward "\n")
                                                              (point))))
                                                   (kill-region (point) end)))))
               ("copy sexp to kill-ring" . (lambda (candidate)
                                             (kill-new helm-input)))))))

(defun helm-eval-new-line-and-indent ()
  (interactive)
  (newline) (lisp-indent-line))

(defun helm-eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `helm-eldoc-active-minibuffers-list'."
  (with-selected-window (minibuffer-window)
    (push (buffer-name) helm-eldoc-active-minibuffers-list)))

(defun helm-eldoc-show-in-eval ()
  "Return eldoc in mode-line for current minibuffer input."
  (let ((buf (with-selected-window (minibuffer-window)
               (buffer-name))))
    (when (member buf helm-eldoc-active-minibuffers-list)
      (let* ((str-all (with-current-buffer buf
                        (minibuffer-completion-contents)))
             (sym     (when str-all
                        (with-temp-buffer
                          (insert str-all)
                          (goto-char (point-max))
                          (unless (looking-back ")\\|\"") (forward-char -1))
                          (eldoc-current-symbol))))
             (info-fn (eldoc-fnsym-in-current-sexp))
             (doc     (or (eldoc-get-var-docstring sym)
                          (eldoc-get-fnsym-args-string
                           (car info-fn) (cadr info-fn)))))
        (when doc (funcall helm-c-eldoc-in-minibuffer-show-fn doc))))))

(defun helm-c-show-info-in-mode-line (str)
  "Display string STR in mode-line."
  (save-selected-window
    (with-current-buffer helm-buffer
      (let ((mode-line-format (concat " " str)))
        (force-mode-line-update)
        (sit-for helm-c-show-info-in-mode-line-delay))
      (force-mode-line-update))))

;;; Calculation Result
;;
;;
(defvar helm-c-source-calculation-result
  '((name . "Calculation Result")
    (dummy)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (calc-eval helm-pattern)
                                           (error "error")))))
    (action ("Copy result to kill-ring" . kill-new))))

;;;###autoload
(defun helm-eval-expression (arg)
  "Preconfigured helm for `helm-c-source-evaluation-result'."
  (interactive "P")
  (helm :sources 'helm-c-source-evaluation-result
        :input (when arg (thing-at-point 'sexp))
        :buffer "*helm eval*"
        :history 'read-expression-history
        :keymap helm-eval-expression-map))

;;;###autoload
(defun helm-eval-expression-with-eldoc ()
  "Preconfigured helm for `helm-c-source-evaluation-result' with `eldoc' support. "
  (interactive)
  (declare (special eldoc-idle-delay))
  (let ((timer (run-with-idle-timer eldoc-idle-delay
                                    'repeat 'helm-eldoc-show-in-eval))
        (minibuffer-completing-symbol t) ; Enable lisp completion.
        (completion-cycle-threshold t))  ; Always cycle, (emacs24* only).
    (unwind-protect
         (minibuffer-with-setup-hook
             'helm-eldoc-store-minibuffer
           (call-interactively 'helm-eval-expression))
      (and timer (cancel-timer timer))
      (setq helm-eldoc-active-minibuffers-list
            (cdr helm-eldoc-active-minibuffers-list)))))

;;;###autoload
(defun helm-calcul-expression ()
  "Preconfigured helm for `helm-c-source-calculation-result'."
  (interactive)
  (helm-other-buffer 'helm-c-source-calculation-result "*helm calcul*"))

(provide 'helm-eval)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-eval.el ends here
