
;;;_. Initialization

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(require 'cl)
(defun org-table-list-and-toggle (table)
"turns an org-table into a list"                                       
(if (eq (cadr table) 'hline) 
    (setq table (cddr table)))
(loop for el in table
      when (not (equal "" (cadr el))) 
      collect
      (car el)))

  (defun org-table-remove-header (table)
    "turns an org-table into a list"                                       
    (if (eq (cadr table) 'hline) 
        (setq table (cddr table))))

(defun add-to-load-path (path &optional dir)
  "add dir to load path. "  
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) 
load-path)))

(defun add-to-load-path-include-subdirs (path)
  "add level 1 subdirectories of path to te load-path."
  (add-to-load-path path)
  (dolist (entry (nreverse (directory-files-and-attributes
                            (expand-file-name path user-emacs-directory))))
    (if (and (cadr entry)
             (not (equal "." (car entry)))
             (not (equal ".." (car entry)))) 
        (add-to-load-path (car entry)
                          (expand-file-name path user-emacs-directory)))))

(let ((table (quote (("directory" "toggle") hline ("site-lisp" "t") ("repos/jwiegley/site-lisp" "t") ("elpa" "t")))))
(defvar group-load-directories
      (list "lisp" "site-lisp")
      "directories that should have their subdirectories added 
to the load-path. Can be absolute or relative to user-emacs-directory")

(setq group-load-directories (org-table-list-and-toggle table))
(mapc #'add-to-load-path-include-subdirs
          (nreverse group-load-directories))
)

(let ((table (quote (("directory" "toggle") hline ("~/.emacs.d" "t") ("~/.emacs.d/site-lisp/org-mode/contrib/lisp" "t")))))
(defvar path-directories
      (list "~/.emacs.d")
      "Directories that should be added to load-path.")

(setq path-directories (org-table-list-and-toggle table))
    (mapc #'add-to-load-path
          (nreverse path-directories))
    (setq load-path (delete-dups load-path))
(require 'autoloads nil t)
(require 'cus-load nil t)
)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
;;(package-initialize)

(setq custom-theme-load-path (quote (custom-theme-directory t 
      "/Users/tonyday/.emacs.d/elpa/zenburn-theme-1.5")))
(load-theme 'zenburn t)

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(require 'bind-key)    
(defun bind-keys-from-table (data)
  "Takes a list of key bindings and commands (both in string format) and binds them using bind-key
"
  (dolist (row data)
    (eval (car (read-from-string 
                (apply 'format "(bind-key %S '%s)" row))))))

(defvar running-alternate-emacs nil)

(defun set-variables-from-table (data)
  "Takes a list of key bindings and commands (both in string format) and binds them using bind-key
"
  (dolist (row data)
    (eval (car (read-from-string 
                (apply 'format "(setq %s %s)" row))))))

(defun set-functions-from-table (data)
  "Takes a list of key bindings and commands (both in string format) and binds them using bind-key
"
  (dolist (row data)
    (eval (car (read-from-string 
                (apply 'format "(%s %s)" row))))))

(let ((table (quote (("variable" "value") hline ("transient-mark-mode" "t")))))
(set-variables-from-table (org-table-remove-header table))  
)

(let ((table (quote (("function" "args") hline))))
(set-functions-from-table (org-table-remove-header table))
)

;; % dont survive the format process
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . ,(expand-file-name
                                      (concat user-emacs-directory "backups")))))

(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'yaml-mode)
(require 'find-file-in-project)

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

(defun unfill-region (beg end)
    "Unfill the region, joining text paragraphs into a single
  logical line.  This is useful, e.g., for use with
  `visual-line-mode'."
    (interactive "*r")
    (let ((fill-column (point-max)))
      (fill-region beg end)))
  
  ;; Handy key definition
  (define-key global-map "\C-\M-Q" 'unfill-region)

(defun unfill-paragraph ()
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil)))
  ;; Handy key definition
  (define-key global-map "\M-Q" 'unfill-paragraph)

(let ((table (quote (("/Users/tonyday/.emacs.d/info") ("/usr/share/info/")))))
 (setq clean-table
       (delete nil  
               (loop for line in table
                     collect 
                     (delete "" (delete-dups line)))))
(setq Info-default-directory-list (apply #'append clean-table))
(setq Info-directory-list Info-default-directory-list)
)

(let ((table (quote (("key" "binding") hline ("C-+" "text-scale-increase") ("C--" "text-scale-decrease") ("C-c C-v k" "org-table-recalculate-buffer-tables")))))
(bind-keys-from-table (org-table-remove-header table))  
)

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(defun system-idle-time ()
  (with-temp-buffer
    (call-process "ioreg" nil (current-buffer) nil
                  "-c" "IOHIDSystem" "-d" "4" "-S")
    (goto-char (point-min))
    (and (re-search-forward "\"HIDIdleTime\" = \\([0-9]+\\)" nil t)
         (/ (float (string-to-number (match-string 1)))
            1000000000.0))))

(defun quickping (host)
  (= 0 (call-process "/sbin/ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun cleanup-term-log ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\|
$\\|P.+\\\\\n\\)" nil t)
    (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                 'invisible t))
  (set-buffer-modified-p nil))

(add-hook 'find-file-hooks
          (function
           (lambda ()
             (if (string-match "/\\.iTerm/.*\\.log\\'"
                               (buffer-file-name))
                 (cleanup-term-log)))))

;;;_ , Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_. Keybindings

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;;;_ , global-map

;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

;;;_  . M-?

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" 'async-shell-command)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-s n" 'find-name-dired)
(bind-key "M-s o" 'occur)

(bind-key "A-M-w" 'copy-code-as-rtf)

;;;_  . M-C-?

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(defun isearch-backward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-backward))

(bind-key "C-M-r" 'isearch-backward-other-window)

(defun isearch-forward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-forward))

(bind-key "C-M-s" 'isearch-forward-other-window)

;; Some further isearch bindings
(bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
(bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
(bind-key "C-^" 'isearch-edit-string isearch-mode-map)
(bind-key "C-i" 'isearch-complete isearch-mode-map)

;;;_  . A-?

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;;_ , ctl-x-map

;;;_  . C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

;;;_  . C-x C-?

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)


(defun find-alternate-file-with-sudo (filename)
  (interactive
   (list (read-file-name "Find alternate file: " nil
                         nil nil (concat "/sudo::" (buffer-file-name)))))
  (find-alternate-file filename))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;;;_  . C-x M-?

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

;;;_ , mode-specific-map

;;;_  . C-c ?

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
 Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(bind-key "C-c 0"
  (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
  (recursive-edit-preserving-window-config
   (if (one-window-p 'ignore-minibuffer)
       (error "Current window is the only window in its frame")
     (delete-other-windows))))

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

(bind-key "C-c e E" 'elint-current-buffer)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e j" 'emacs-lisp-mode)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e s" 'scratch)
(bind-key "C-c e v" 'edit-variable)

(defun find-which (name)
  (interactive "sCommand name: ")
  (find-file-other-window
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

(bind-key "C-c e w" 'find-which)
(bind-key "C-c e z" 'byte-recompile-directory)

(bind-key "C-c f" 'flush-lines)
(bind-key "C-c g" 'goto-line)

(bind-key "C-c k" 'keep-lines)

(eval-when-compile
  (defvar emacs-min-top)
  (defvar emacs-min-left)
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(unless noninteractive
  (if running-alternate-emacs
      (progn
        (defvar emacs-min-top (if (= 1050 (x-display-pixel-height)) 574 722))
        (defvar emacs-min-left 5)
        (defvar emacs-min-height 25)
        (defvar emacs-min-width 80))

    (defvar emacs-min-top 22)
    (defvar emacs-min-left (- (x-display-pixel-width) 918))
    (defvar emacs-min-height (if (= 1050 (x-display-pixel-height)) 55 64))
    (defvar emacs-min-width 100)))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (when running-alternate-emacs
    (set-background-color "grey85")
    (set-face-background 'fringe "gray80")))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" 'emacs-toggle-size)

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode
                    haskell-mode literate-haskell-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" 'insert-user-timestamp)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(autoload 'auth-source-search "auth-source")

(defun tinify-url (url)
  (interactive "sURL to shorten: ")
  (let* ((api-login "jwiegley")
         (api-key
          (funcall
           (plist-get
            (car (auth-source-search :host "api.j.mp" :user api-login
                                     :type 'netrc :port 80))
            :secret))))
    (flet ((message (&rest ignore)))
      (with-current-buffer
          (let ((query
                 (format "format=txt&longUrl=%s&login=%s&apiKey=%s"
                         (url-hexify-string url) api-login api-key)))
            (url-retrieve-synchronously
             (concat "http://api.j.mp/v3/shorten?" query)))
        (goto-char (point-min))
        (re-search-forward "^$")
        (prog1
            (kill-new (buffer-substring (1+ (point)) (1- (point-max))))
          (kill-buffer (current-buffer)))))))

(bind-key "C-c U" 'tinify-url)
(bind-key "C-c v" 'ffap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_  . C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_  . C-c M-?

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;;_ , ctl-period-map

;;;_  . C-. ?

(bind-key "C-. m" 'kmacro-keymap)

;;;_  . C-. C-i

(bind-key "C-. C-i" 'indent-rigidly)

(bind-key "C-. C-c" 'org-indent-indent-buffer)

;;;_ , help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_  . C-h e ?

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(defun my-describe-symbol  (symbol &optional mode)
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (let (info-buf find-buf desc-buf cust-buf)
    (save-window-excursion
      (ignore-errors
        (info-lookup-symbol symbol mode)
        (setq info-buf (get-buffer "*info*")))
      (let ((sym (intern-soft symbol)))
        (when sym
          (if (functionp sym)
              (progn
                (find-function sym)
                (setq find-buf (current-buffer))
                (describe-function sym)
                (setq desc-buf (get-buffer "*Help*")))
            (find-variable sym)
            (setq find-buf (current-buffer))
            (describe-variable sym)
            (setq desc-buf (get-buffer "*Help*"))
            ;;(customize-variable sym)
            ;;(setq cust-buf (current-buffer))
            ))))

    (delete-other-windows)

    (flet ((switch-in-other-buffer
            (buf)
            (when buf
              (split-window-vertically)
              (switch-to-buffer-other-window buf))))
      (switch-to-buffer find-buf)
      (switch-in-other-buffer desc-buf)
      (switch-in-other-buffer info-buf)
      ;;(switch-in-other-buffer cust-buf)
      (balance-windows))))

(bind-key "C-h e d" 'my-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)

;;;_ , color-moccur

(let ((ad-redefinition-action 'accept))
  (use-package color-moccur
    :commands (isearch-moccur isearch-all)
    :bind ("M-s O" . moccur)
    :init
    (progn
      (bind-key "M-o" 'isearch-moccur isearch-mode-map)
      (bind-key "M-O" 'isearch-moccur-all isearch-mode-map))

    :config
    (use-package moccur-edit)))

;;;_ , ibuffer

(use-package ibuffer
  :defer t
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
    :bind ("C-x C-b" . ibuffer))

;;;_ , diff-mode

(use-package diff-mode
  :commands diff-mode
  :config
  (use-package diff-mode-))

(setq vc-diff-switches '("-b" "-B" "-u"))

;;;_ , ediff

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-period-equals-map)
    (define-prefix-command 'ctl-period-equals-map)
    (bind-key "C-. =" 'ctl-period-equals-map)

    (bind-key "C-. = c" 'compare-windows)) ; not an ediff command, but it fits

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (use-package ediff-keep))

;;;_ , grep

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s f" . find-grep)
         ("M-s g" . grep)
         ("M-s r" . rgrep))
  :init
  (progn
    (defun find-grep-in-project (command-args)
      (interactive
       (let ((default (thing-at-point 'symbol)))
         (list (read-shell-command "Run find (like this): "
                                   (cons (concat "git --no-pager grep -n "
                                                 default)
                                         (+ 24 (length default)))
                                   'grep-find-history))))
      (if command-args
          (let ((null-device nil))      ; see grep
            (grep command-args))))

    (bind-key "M-s p" 'find-grep-in-project))

  :config
  (progn
    (use-package grep-ed)

    (grep-apply-setting 'grep-command "egrep -nH -e ")
    (grep-apply-setting
     'grep-find-command
     '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))))

;;;_ , ielm

(use-package ielm
  :bind ("C-c :" . ielm)
  :config
  (progn
    (defun my-ielm-return ()
      (interactive)
      (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))

    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "<return>" 'my-ielm-return ielm-map)))
              t)))

;;;_ , info

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)

  :config
  (progn
    (defadvice info-setup (after load-info+ activate)
      (use-package info+))

    (defadvice Info-exit (after remove-info-window activate)
      "When info mode is quit, remove the window."
      (if (> (length (window-list)) 1)
          (delete-window)))))

(use-package info-look
  :commadns info-lookup-add-help)

;;;_ , magit

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    (require 'magit-topgit)
    (require 'rebase-mode)

    (defun start-git-monitor ()
      (interactive)
      (start-process "git-monitor" (current-buffer) "~/bin/git-monitor"))

    ;;(add-hook 'magit-status-mode-hook 'start-git-monitor)
    ))

(let ((table (quote (("~/git/scarce") ("~/stuff") ("~/.emacs.d") ("~/git/o-blog") ("~/git/emfx_R")))))
(setq magit-repo-dirs
      (loop for file in table
                collect (car file)))
)

;;;_ , paredit

(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :config
  (progn
    (use-package paredit-ext)

    (bind-key "C-M-l" 'paredit-recentre-on-sexp paredit-mode-map)

    (bind-key ")" 'paredit-close-round-and-newline paredit-mode-map)
    (bind-key "M-)" 'paredit-close-round paredit-mode-map)

    (bind-key "M-k" 'paredit-raise-sexp paredit-mode-map)
    (bind-key "M-h" 'mark-containing-sexp paredit-mode-map)
    (bind-key "M-I" 'paredit-splice-sexp paredit-mode-map)

    (unbind-key "M-r" paredit-mode-map)
    (unbind-key "M-s" paredit-mode-map)

    (bind-key "C-. d" 'paredit-forward-down paredit-mode-map)
    (bind-key "C-. B" 'paredit-splice-sexp-killing-backward paredit-mode-map)
    (bind-key "C-. C" 'paredit-convolute-sexp paredit-mode-map)
    (bind-key "C-. F" 'paredit-splice-sexp-killing-forward paredit-mode-map)
    (bind-key "C-. a" 'paredit-add-to-next-list paredit-mode-map)
    (bind-key "C-. A" 'paredit-add-to-previous-list paredit-mode-map)
    (bind-key "C-. j" 'paredit-join-with-next-list paredit-mode-map)
    (bind-key "C-. J" 'paredit-join-with-previous-list paredit-mode-map)

    (add-hook 'allout-mode-hook
              #'(lambda ()
                  (bind-key "M-k" 'paredit-raise-sexp allout-mode-map)
                  (bind-key "M-h" 'mark-containing-sexp allout-mode-map)))))

;;;_ , recentf

(use-package recentf
  :if (not noninteractive)
  :init
  (progn
    (recentf-mode 1)

    (defun recentf-add-dired-directory ()
      (if (and dired-directory
               (file-directory-p dired-directory)
               (not (string= "/" dired-directory)))
          (let ((last-idx (1- (length dired-directory))))
            (recentf-add-file
             (if (= ?/ (aref dired-directory last-idx))
                 (substring dired-directory 0 last-idx)
               dired-directory)))))

    (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

;;;_ , whitespace

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :init
  (progn
    (hook-into-modes 'whitespace-mode
                     '(prog-mode-hook
                       c-mode-common-hook))

    (defun normalize-file ()
      (interactive)
      (save-excursion
      (goto-char (point-min))
        (whitespace-cleanup)
        (delete-trailing-whitespace)
        (goto-char (point-max))
        (delete-blank-lines)
        (set-buffer-file-coding-system 'unix)
        (goto-char (point-min))
        (while (re-search-forward "\r$" nil t)
          (replace-match ""))
        (set-buffer-file-coding-system 'utf-8)
        (let ((require-final-newline t))
          (save-buffer))))

    (defun maybe-turn-on-whitespace ()
      "Depending on the file, maybe clean up whitespace."
      (let ((file (expand-file-name ".clean"))
            parent-dir)
        (while (and (not (file-exists-p file))
                    (progn
                      (setq parent-dir
                            (file-name-directory
                             (directory-file-name
                              (file-name-directory file))))
                      ;; Give up if we are already at the root dir.
                      (not (string= (file-name-directory file)
                                    parent-dir))))
          ;; Move up to the parent dir and try again.
          (setq file (expand-file-name ".clean" parent-dir)))
        ;; If we found a change log in a parent, use that.
        (when (and (file-exists-p file)
                   (not (file-exists-p ".noclean"))
                   (not (and buffer-file-name
                             (string-match "\\.texi\\'" buffer-file-name))))
          (add-hook 'write-contents-hooks
                    #'(lambda ()
                        (ignore (whitespace-cleanup))) nil t)
          (whitespace-cleanup))))

    (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t))

  :config
  (progn
    (remove-hook 'find-file-hooks 'whitespace-buffer)
    (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

(let ((init-file-loads (quote (("file" "toggle") hline ("org-mode-prequel.org" "t") ("org-mode.org" "t") ("org-mode-mods.org" "t") ("my-old-org.org" "")))))
(defvar other-init-files
      (list ())
      "Files that are loaded in addition to dotemacs.org.")
(setq file-loads (org-table-list-and-toggle init-file-loads))
(dolist (file file-loads)
  (org-babel-load-file (expand-file-name file user-emacs-directory)))
)

(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)

;;;_. Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
