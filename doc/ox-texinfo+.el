(require 'ox-texinfo)

(defun org-texinfo-keyword+ (keyword contents info)
  (let* ((v (org-element-property :value keyword))
         (k (org-element-property :key keyword))
         (c (org-texinfo-keyword keyword contents info)))
    (if c c
      (when (string-match  "\\(?:\\`\\(?1:.+\\)INDEX\\'\\)" k)
        (format "@%sindex %s" (downcase (match-string 1 k)) v)))))

(defun org-texinfo-export-to-texinfo+ ()
  (interactive)
  (let ((outfile (org-export-output-file-name ".texi"))
        (org-export-coding-system org-texinfo-coding-system))
    (org-export-to-file 'my-texinfo outfile)))

(org-export-define-derived-backend 'my-texinfo 'texinfo
  :translate-alist '((keyword . org-texinfo-keyword+)))

(provide 'ox-texinfo+)
