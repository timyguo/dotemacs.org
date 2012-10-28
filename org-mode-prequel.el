
(setq org-mode-user-lisp-path 
      (expand-file-name "src/org-mode/lisp" user-emacs-directory))
(setq org-user-agenda-files (quote ()))
(setq org-mode-user-contrib-lisp-path 
      (expand-file-name "src/org-mode/contrib/lisp" user-emacs-directory))

(add-to-list 'Info-default-directory-list "~/git/org-mode/doc")
