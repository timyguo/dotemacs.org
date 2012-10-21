
;; see org-init.org for documentation
(setq working-dir
      (file-name-directory (or load-file-name (buffer-file-name))))
(let ((org-dir (expand-file-name "src/org-mode/lisp" working-dir)))
  (add-to-list 'load-path org-dir))
(org-babel-load-file (expand-file-name "dotemacs.org" working-dir))
