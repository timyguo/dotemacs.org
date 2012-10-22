
;; see org-init.org for documentation
(setq user-emacs-directory
      (file-name-directory (or load-file-name (buffer-file-name))))
<<<<<<< HEAD

(let ((elisp-dir (expand-file-name "src" starter-kit-dir)))
    ;; add the src directory to the load path
    (add-to-list 'load-path elisp-dir)
    ;; load specific files
    (when (file-exists-p elisp-dir)
      (let ((default-directory elisp-dir))
        (normal-top-level-add-subdirs-to-load-path))))
 
;; load up the starter kit
(org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))
=======
(let ((org-dir (expand-file-name "src/org-mode/lisp" user-emacs-directory)))
  (add-to-list 'load-path org-dir))
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
>>>>>>> flatten
