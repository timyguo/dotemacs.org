
(let ((dotemacs-org-file (quote "/Users/tonyday/.emacs.d/dotemacs.org")))
;; see dotemacs.org for documentation
(setq user-emacs-directory
      (file-name-directory (or load-file-name (buffer-file-name))))
(let ((org-dir (expand-file-name "src/org-mode/lisp" user-emacs-directory)))
  (add-to-list 'load-path org-dir))
(org-babel-load-file (expand-file-name dotemacs-org-file user-emacs-directory))
)
