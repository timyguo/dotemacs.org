
;; see org-init.org for documentation

;; remember this directory
(setq working-dir
      (file-name-directory (or load-file-name (buffer-file-name))))

;; put the org dir on the path if it exists
(let ((org-dir (expand-file-name "src/org-mode/lisp" working-dir)))
    ;; add the src directory to the load path
    (add-to-list 'load-path org-dir))
 
;; load up the org-init file
(org-babel-load-file (expand-file-name "dotemacs.org" working-dir))
