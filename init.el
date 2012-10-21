
(defun debug-message (&optional debug-name debug-value)
        (message (concat 
           "#+begin_debug\n"
           (format "%s\n" buffer-file-name)
           (if debug-name
             (format "%s\n" debug-name))
           (if debug-value
               (format "%s\n" debug-value))
           "#+end_debug\n")))

(debug-message "load-path init.el entry" load-path)
(debug-message "user-emacs-directory init.el entry" user-emacs-directory)

;; see org-init.org for documentation
(setq user-emacs-directory
      (file-name-directory (or load-file-name (buffer-file-name))))
(let ((org-dir (expand-file-name "src/org-mode/lisp" user-emacs-directory)))
  (add-to-list 'load-path org-dir))
(org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
