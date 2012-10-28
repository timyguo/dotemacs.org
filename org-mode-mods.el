
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

(let ((table (quote (("key" "desc" "type" "target type" "target path" "target header" "template") hline ("," ",," "entry" "file" "~/stuff/org/refile.org" "" "* %?\\n") ("t" "todo" "" "" "" "" "") ("tu" "urgent todo" "entry" "file" "~/stuff/org/refile.org" "" "* NEXT %? :urgent:\\n:PROPERTIES:\\n:OPEN: %U\\n:END:\\n") ("tl" "linked todo" "entry" "file" "~/stuff/org/refile.org" "" "* TODO %?\\n%a\\n") ("tn" "next todo" "entry" "file" "~/stuff/org/refile.org" "" "* NEXT %?\\n") ("tt" "todo todo" "entry" "file" "~/stuff/org/refile.org" "" "* TODO %?\\n") ("tb" "yank body" "entry" "file" "~/stuff/org/refile.org" "" "* TODO %?\\n%c\\n") ("th" "yank header" "entry" "file" "~/stuff/org/refile.org" "" "* TODO %c\\n%?\\n") ("i" "idea" "" "" "" "" "") ("ii" "idea" "entry" "file+headline" "~/stuff/content/ideas.org" "incoming" "* %? \\n") ("il" "idea link" "entry" "file+headline" "~/stuff/content/ideas.org" "links" "* %a \\n %?") ("iy" "idea yank" "entry" "file+headline" "~/stuff/content/ideas.org" "incoming" "* %?%c \\n") ("z" "bugz" "entry" "file+headline" "~/stuff/org/bugz.org" "incoming" "* TODO %?\\n%a") ("s" "snipz" "" "" "" "" "") ("sr" "snipz request" "entry" "file+headline" "~/stuff/org/snipz.org" "incoming" "* TODO %?\\n%a\\n") ("sn" "snipz note" "entry" "file+headline" "~/stuff/org/snipz.org" "incoming" "* %?\\n%c\\n") ("b" "binding" "table-line" "file+headline" "~/stuff/emacs/bindings.org" "incoming" "%?") ("k" "kill ring" "" "" "" "" "") ("kb" "kill ring body" "entry" "file" "~/stuff/org/refile.org" "" "* %?\\n%c\\n") ("kh" "kill ring head" "entry" "file" "~/stuff/org/refile.org" "" "* %c\\n%?\\n")))))
(defun org-table-remove-header (table)
  "turns an org-table into a list"                                       
  (if (eq (cadr table) 'hline) 
      (setq table (cddr table))))
    
(setq capture-data (org-table-remove-header table))  

(setq org-capture-templates 
      (loop for row in capture-data
            collect
            (eval (quote (car (read-from-string 
                              (replace-regexp-in-string "(  )" ""                                                     (replace-regexp-in-string "\"\"" "" 
                              (replace-regexp-in-string "\\\\n" "n" 
                               (apply 'format "(%S %S %s (%s %S %S) %S)" row))))))))))
)

(let ((table (quote (("stuff" "" "stuff.org") (".emacs.d" "" "README.org") (".emacs.d" "" "dotemacs.org") ("stuff" "org" "refile.org") ("stuff" "org" "bugz.org") ("stuff" "org" "snipz.org") ("stuff" "org" "org.org") ("stuff" "emacs" "emacs.org") ("stuff" "emacs" "bindings.org") ("stuff" "sys" "sys.org") ("stuff" "biz" "scarce.org") ("stuff" "content" "life.org") ("stuff" "content" "writing.org") ("stuff" "dev" "webdev.org") ("stuff" "dev" "sandpit.org") ("stuff" "dev" "jsdev.org") ("stuff" "factor" "factor.org") ("stuff" "content" "ideas.org") ("stuff" "factor" "parity.org") ("stuff" "quant" "rdev.org") ("stuff" "quant" "volatility.org") ("stuff" "quant" "da.org") ("stuff" "quant" "emfx.org") ("stuff" "sys" "git.org") ("git" "scarce" "scarce.org") ("git" "scarce" "mindev.org") ("git" "scarce" "sitedev.org")))))
(setq clean-table
      (delete nil  
              (loop for line in table
                    collect 
                    (delete "" (delete-dups line)))))

(setq org-agenda-files 
      (mapcar
       (lambda (x) 
         (concat "~/"
                 (mapconcat
                  'eval x "/")))
       clean-table))
)

; Tags with fast selection keys
(setq org-tag-alist (quote (("urgent" . ?u)
                            ("bill" . ?b)
                            ("scarce" . ?s)
                            ("crypt" . ?c)
                            ("emacs" . ?e))))

(org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (sh . t)
           (gnuplot . t)
           (org . t)
           (octave . t)
           (js . t)
           (R . t))))

(setq org-link-abbrev-alist
     '(("google"    . "http://www.google.com/search?q=")))

(setq org-default-notes-file "~/stuff/org/refile.org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/stuff/org/refile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(remove-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp)

(setq org-enable-priority-commands nil)

(let ((table (quote (("key" "binding") hline ("C-c l" "org-store-link") ("C-c a" "org-agenda") ("C-c b" "org-iswitchb") ("<f12>" "org-agenda") ("<f5>" "bh/org-todo") ("<S-f5>" "bh/widen") ("<f7>" "bh/set-truncate-lines") ("<f8>" "org-cycle-agenda-files") ("<f9> <f9>" "bh/show-org-agenda") ("<f9> b" "bbdb") ("<f9> c" "calendar") ("<f9> f" "boxquote-insert-file") ("<f9> g" "gnus") ("<f9> h" "bh/hide-other") ("<f9> n" "org-narrow-to-subtree") ("<f9> w" "widen") ("<f9> u" "bh/narrow-up-one-level") ("<f9> I" "bh/punch-in") ("<f9> O" "bh/punch-out") ("<f9> o" "bh/make-org-scratch") ("<f9> r" "boxquote-region") ("<f9> s" "bh/switch-to-scratch") ("<f9> t" "bh/insert-inactive-timestamp") ("<f9> T" "tabify") ("<f9> U" "untabify") ("<f9> v" "visible-mode") ("<f9> SPC" "bh/clock-in-last-task") ("C-<f9>" "previous-buffer") ("M-<f9>" "org-toggle-inline-images") ("C-x n r" "narrow-to-region") ("C-<f10>" "next-buffer") ("<f11>" "org-clock-goto") ("C-<f11>" "org-clock-in") ("C-s-<f12>" "bh/save-then-publish") ("C-M-r" "org-capture") ("C-c r" "org-capture") ("C-M-r" "org-capture") ("C-c r" "org-capture") ("<f9> p" "bh/phone-call") ("C-s-<f12>" "bh/save-then-publish") ("<f5>" "bh/org-todo") ("<S-f5>" "bh/widen") ("<f9> t" "bh/insert-inactive-timestamp") ("M-x" "smex") ("C-x x" "smex") ("M-X" "smex-major-mode-commands") ("<C-f6>" "(lambda () (interactive) (bookmark-set \"SAVED\"))") ("<f6>" "(lambda () (interactive) (bookmark-jump \"SAVED\"))")))))
(bind-keys-from-table (org-table-remove-header table))  
)
