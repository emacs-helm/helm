(require 'cl)

(defvar helm-source-x-selection
  `((name . "X Selections")
    (candidates . helm-x-selection-candidates)
    (filtered-candidate-transformer helm-x-selection-transformer)
    (action . (("Yank" . helm-x-selection-insert-action)))
    (persistent-action . (lambda (_candidate) (ignore)))
    (persistent-help . "DoNothing")
    (keymap . ,helm-kill-ring-map)
    (multiline)))


(defun helm-x-selection-candidates ()
  ; Drop empty selections
  (remove-if-not 'identity (list (x-get-selection 'PRIMARY 'STRING)
				 (x-get-selection 'SECONDARY 'STRING)
				 (x-get-selection 'CLIPBOARD 'STRING))))

(defun helm-x-selection-transformer (candidates source)
  (mapcar (lambda (elt) (cons (substring-no-properties elt 0) elt))  candidates))


(defun helm-x-selection-insert-action (str)
  (run-with-timer 0.01 nil `(lambda () (insert-for-yank-1 ,str))))


(provide 'helm-x-selection)
