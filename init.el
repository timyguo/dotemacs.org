
(let ((tangled-in (quote "/Users/tonyday/.emacs.d/dotemacs.org")))
;; see dotemacs.org for documentation
(setq user-emacs-directory
      (file-name-directory (or load-file-name user-emacs-directory "~/.emacs.d/")))
(defvar org-dir
"repos/jwiegley/override/org-mode/lisp"
"location of the directory containing org-mode")
(add-to-list 'load-path (expand-file-name org-dir user-emacs-directory))
(defvar dotemacs-org-file
"dotemacs.org"
"Name of the org file containing the main startup code")
(if (boundp 'tangled-in)
   (setq dotemacs-org-file tangled-in))
(org-babel-load-file (expand-file-name dotemacs-org-file user-emacs-directory))
)
