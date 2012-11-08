;;; helm-eshell.el --- pcomplete and eshell completion for helm.

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

;; Enable like this in .emacs:
;;
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-elisp)
(require 'helm-regexp)

(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eshell-parse-arguments "esh-arg" (beg end))

(defvar helm-eshell-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") 'helm-next-line)
    map)
  "Keymap for `helm-eshell-history'.")

(defvar helm-c-source-esh
  '((name . "Eshell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the helm one. (Fixed in Emacs24)
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates . helm-esh-get-candidates)
    (filtered-candidate-transformer
     (lambda (candidates _sources)
       (loop for i in candidates collect
             (cons (abbreviate-file-name i) i))))
    (action . helm-ec-insert))
  "Helm source for Eshell completion.")

;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing at point."
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (if (string-match "\\`~/" helm-ec-target)
      (insert (helm-quote-whitespace (abbreviate-file-name candidate)))
      (insert (helm-quote-whitespace candidate))))

(defun helm-esh-get-candidates ()
  "Get candidates for eshell completion using `pcomplete'."
  (catch 'pcompleted
    (with-helm-current-buffer
      (let* ((pcomplete-stub)
             pcomplete-seen pcomplete-norm-func
             pcomplete-args pcomplete-last pcomplete-index
             (pcomplete-autolist pcomplete-autolist)
             (pcomplete-suffix-list pcomplete-suffix-list)
             (table (pcomplete-completions))
             (entry (condition-case nil
                        ;; On Emacs24 `try-completion' return
                        ;; pattern when more than one result.
                        ;; Otherwise Emacs23 return nil, which
                        ;; is wrong, in this case use pattern
                        ;; to behave like Emacs24.
                        (or (try-completion helm-pattern
                                            (pcomplete-entries))
                            helm-pattern)
                      ;; In Emacs23 `pcomplete-entries' may fail
                      ;; with error, so try this instead.
                      (error
                       nil
                       (let ((fc (car (last
                                       (pcomplete-parse-arguments)))))
                         ;; Check if last arg require fname completion.
                         (and (file-name-directory fc) fc))))))
        (loop for i in (all-completions pcomplete-stub table)
              for file-cand = (and entry
                                   (if (file-remote-p i) i
                                       (expand-file-name
                                        i (file-name-directory entry))))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand)))
              collect file-cand into ls
              else collect i into ls
              finally return
              (if (and entry (not (string= entry "")) (file-exists-p entry))
                  (append (list (expand-file-name entry default-directory)) ls)
                  ls))))))

;;; Eshell history.
;;
;;
(defvar helm-c-source-eshell-history
  `((name . "Eshell history")
    (init . (lambda ()
              (let (eshell-hist-ignoredups)
                ;; Write the content's of ring to file.
                (eshell-write-history eshell-history-file-name)
                (with-current-buffer (helm-candidate-buffer 'global)
                  (insert-file-contents eshell-history-file-name)))
              ;; Same comment as in `helm-c-source-esh'
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates-in-buffer)
    (keymap . ,helm-eshell-history-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (eshell-kill-input)
                (insert candidate))))
  "Helm source for Eshell history.")

;;;###autoload
(defun helm-esh-pcomplete ()
  "Preconfigured helm to provide helm completion in eshell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (end (point-marker))
         (beg (save-excursion (eshell-bol) (point)))
         (args (catch 'eshell-incomplete
                 (eshell-parse-arguments beg end)))
         ;; Use thing-at-point instead of last args value
         ;; to exclude possible delimiters e.g "(".
         (target (thing-at-point 'symbol))
         (first (car args)) ; Maybe lisp delimiter "(".
         last) ; Will be the last but parsed by pcomplete.
    (setq helm-ec-target (or target " ")
          end (point)
          ;; Reset beg for `with-helm-show-completion'.
          beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point))))
    (cond ((eq first ?\()
           (helm-lisp-completion-or-file-name-at-point))
          ;; In eshell `pcomplete-parse-arguments' is called
          ;; with `pcomplete-parse-arguments-function'
          ;; locally bound to `eshell-complete-parse-arguments'
          ;; which is calling `lisp-complete-symbol',
          ;; calling it before would popup the
          ;; *completions* buffer.
          (t (setq last (car (last (ignore-errors
                                     (pcomplete-parse-arguments)))))
             (with-helm-show-completion beg end
               (helm :sources 'helm-c-source-esh
                     :buffer "*helm pcomplete*"
                     :resume 'noresume
                     :input (and (stringp last)
                                 (helm-ff-set-pattern last))))))))

;;;###autoload
(defun helm-eshell-history ()
  "Preconfigured helm for eshell history."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
         (with-helm-show-completion beg end
           (helm :sources 'helm-c-source-eshell-history
                 :buffer "*Eshell history*"
                 :resume 'noresume
                 :input input))
      (when (and flag-empty
                 (looking-back " "))
        (delete-char -1)))))

(provide 'helm-eshell)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-eshell ends here
