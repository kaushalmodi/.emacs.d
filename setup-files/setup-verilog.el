;; Time-stamp: <2015-03-11 14:13:53 kmodi>

;; Verilog

(use-package verilog-mode
    :load-path "elisp/verilog-mode"
    :mode (("\\.[st]*v[hp]*\\'" . verilog-mode) ;; .v, .sv, .svh, .tv, .vp
           ("\\.psl\\'"         . verilog-mode)
           ("\\.h\\'"           . verilog-mode)
           ("\\.vinc\\'"        . verilog-mode))
    :config
    (progn
      (setq verilog-indent-level             3
            verilog-indent-level-module      3
            verilog-indent-level-declaration 3
            verilog-indent-level-behavioral  3
            verilog-indent-level-directive   3
            verilog-case-indent              2
            verilog-auto-newline             nil
            verilog-auto-indent-on-newline   t
            verilog-tab-always-indent        t
            verilog-minimum-comment-distance 10
            verilog-indent-begin-after-if    t
            verilog-auto-lineup              'all
            verilog-align-ifelse             nil
            ;; verilog-align-ifelse             t
            verilog-auto-endcomments         t
            verilog-tab-to-comment           t
            verilog-date-scientific-format   t)

      (defun my-verilog-insert-date ()
        "Insert a stamp with date and username  "
        (interactive)
        (insert "// ")
        (verilog-insert-date)
        (insert (concat " - " (getenv "USER"))))

      (when (executable-find "ag")
        (require 'ag)
        (require 'projectile)
        (defvar verilog-identifier-regexp "[a-zA-Z][a-zA-Z0-9_]*"
          "Regexp for a valid verilog identifier.")

        (defun verilog-mode-find-parent-module ()
          "Find the places where the current verilog module is instantiated in
the project."
          (interactive)
          (let ((verilog-module-regexp (concat "^\\s-*" ; elisp regexp
                                               "\\(?:module\\)\\s-+" ; shy group
                                               "\\("
                                               verilog-identifier-regexp
                                               "\\)\\b"))
                verilog-module-name
                verilog-module-instance-regexp)
            (save-excursion
              (search-backward-regexp verilog-module-regexp)
              (setq verilog-module-name (match-string 1))
              (setq verilog-module-instance-regexp
                    (concat "^\\s*" ; pcre regex
                            verilog-module-name
                            "\\s+"
                            ;; "(#\\s*\\((\\n|.)*?\\)(\\n|.)*?)*" ; optional hardware parameters
                            "(#\\s*\\((\\n|.)*?\\))*" ; optional hardware parameters
                                        ; '(\n|.)*?' does non-greedy multi-line grep
                            "(\\n|.)*?" ; optional newline/space before instance name
                            "\\K" ; don't highlight anything till this point
                            verilog-identifier-regexp ; instance name
                            "(?=[^a-zA-Z0-9_]*\\()")) ; optional space/newline after instance name
                                        ; and before opening parenthesis `('
                                        ; don't highlight anything in (?=..)
              (ag-regexp verilog-module-instance-regexp
                         (projectile-project-root)))))
        (bind-key "C-^" #'verilog-mode-find-parent-module verilog-mode-map)

        (defun verilog-mode-find-module-instance (&optional arg)
          "Function to return the module and instance name within which
the point is currently.

    C-u COMMAND -> Jump to the top of the instantiation block.
C-u C-u COMMAND -> Jump to the next module instantiation."
          (interactive "P")
          (let ((verilog-instance-regexp
                 (concat "^\\s-*" ; elisp regexp
                         "\\("
                         verilog-identifier-regexp
                         "\\)"
                         "\\s-+"
                         "\\(#\\s-*([[:ascii:][:nonascii:]]*)\\)*" ; optional hardware parameters
                         "\\([^a-zA-Z0-9_\\.,()]*?\\)" ; optional space/newline before instance name
                         "\\(" verilog-identifier-regexp "\\)" ; instance name
                         "\\([^a-zA-Z0-9_]*(\\)" ; optional space/newline after instance name
                                        ; and before opening parenthesis `('
                         ;; "\\([^a-zA-Z0-9_]\\|\\.\\)" ; optional space/newline after `('
                                        ; or name-based port connections: .input_a(input_a),
                         )))
            (cl-case (car arg)
              (4  (search-backward-regexp verilog-instance-regexp nil :noerror))
              (16 (search-forward-regexp  verilog-instance-regexp nil :noerror))
              (t  (save-excursion
                    (if (search-backward-regexp verilog-instance-regexp nil :noerror)
                        (progn
                          ;; (message "---- 1 ---- %s" (match-string 1))
                          ;; (message "---- 2 ---- %s" (match-string 2))
                          ;; (message "---- 3 ---- %s" (match-string 3))
                          ;; (message "---- 4 ---- %s" (match-string 4))
                          ;; (message "---- 5 ---- %s" (match-string 5))
                          (if (string= "module" (match-string 1))
                              (setq-local verilog-mode-module-name nil)
                            (setq-local verilog-mode-module-name (match-string 1)))
                          (setq-local verilog-mode-instance-name (match-string 4))
                          (concat verilog-mode-module-name
                                  "|" verilog-mode-instance-name))
                      nil))))))

        (defun verilog-mode-jump-to-module-at-point ()
          "If the point is somewhere in a module instance, jump to the definition
of that module.

It is required to have `ctags' executable and `projectile' package installed
for this to work."
          (interactive)
          (when (and (executable-find "ctags")
                     (locate-file "TAGS" (list `,(projectile-project-root)))
                     (featurep 'projectile))
            (if (and (verilog-mode-find-module-instance)
                     verilog-mode-module-name)
                (find-tag verilog-mode-module-name)
              (pop-tag-mark))))

        (when (featurep 'key-chord)
          (key-chord-define verilog-mode-map "^^"   (λ (verilog-mode-find-module-instance '(4))))
          (key-chord-define verilog-mode-map "^&"   (λ (verilog-mode-find-module-instance '(16))))
          (key-chord-define verilog-mode-map "\\\\" #'verilog-mode-jump-to-module-at-point)) ; "\\"

        (when (featurep 'which-func)
          (add-to-list 'which-func-modes 'verilog-mode)
          (defun my/verilog-mode-which-func ()
            (setq-local which-func-functions '(verilog-mode-find-module-instance))
            (which-function-mode))
          (add-hook 'verilog-mode-hook #'my/verilog-mode-which-func)))

      (defun my/verilog-mode-customizations()
        ;; Unbind the backtick binding done to `electric-verilog-tick'
        ;; With binding done to electric-verilog-tick, it's not possible to type
        ;; backticks on multiple lines simultaneously in multiple-cursors mode
        (define-key verilog-mode-map "\`"          nil)
        (define-key verilog-mode-map (kbd "C-c d") #'my-verilog-insert-date)
        ;; Replace tabs with spaces when saving files in verilog-mode
        ;; http://www.veripool.org/issues/345-Verilog-mode-can-t-get-untabify-on-save-to-work
        ;; Note that keeping that `nil' in the argument is crucial; otherwise emacs
        ;; with stay stuck with the "Saving file .." message and the file won't be
        ;; saved.
        (add-hook 'local-write-file-hooks
                  (λ (untabify (point-min) (point-max)) nil))

        ;; Source: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
        (font-lock-add-keywords nil
                                '(("\\b\\(FIXME\\|TODO\\|BUG\\)\\b" 1
                                   font-lock-warning-face t)))
        ;; Above solution highlights those keywords anywhere in the buffer (not just
        ;; in comments). To do the highlighting intelligently, install the fic-mode
        ;; package - https://github.com/lewang/fic-mode

        ;; ;; Enable orgstruct mode
        ;; (setq-local orgstruct-heading-prefix-regexp "//; ")
        ;; (turn-on-orgstruct++)
        )
      (add-hook 'verilog-mode-hook #'my/verilog-mode-customizations)

      ;; Tweak the verilog-mode indentation to skip the lines that begin with
      ;; "<optional-white-space>// *" in order to not break any `outline-mode'
      ;; or `outshine' functionality.
      ;; http://emacs.stackexchange.com/a/8033/115
      (defun my/verilog-selective-indent (&rest args)
        "Return t if the current line starts with '// *'.
If the line matches '// *' delete any preceding white space too."
        (interactive)
        (save-excursion
          (beginning-of-line)
          (let ((match (looking-at "^[[:blank:]]*// \\*")))
            (when match
              (delete-horizontal-space))
            match)))
      ;; Advise the indentation behavior of `indent-region' done using `C-M-\'
      (advice-add 'verilog-indent-line-relative :before-until #'my/verilog-selective-indent)
      ;; Advise the indentation done by hitting `TAB'
      (advice-add 'verilog-indent-line          :before-until #'my/verilog-selective-indent)

      ;; Uncomment the lines for which the advice needs to be removed
      ;; (advice-remove 'verilog-indent-line-relative #'my/verilog-selective-indent)
      ;; (advice-remove 'verilog-indent-line          #'my/verilog-selective-indent)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Macros saved as functions
      ;;
      ;; Regex Search Expression - \$display(\(.*?\));\(.*\)
      ;; Replace Expression - `uvm_info("REPLACE_THIS_GENERIC_ID", $sformatf(\1), UVM_MEDIUM) \2
      (fset 'uvm-convert-display-to-uvm_info
            (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 113 92 36 100 105 115 112 108 97 121 40 92 40 46 42 63 92 41 41 59 92 40 46 42 92 41 return 96 117 118 109 95 105 110 102 111 40 34 82 69 80 76 65 67 69 95 84 72 73 83 95 71 69 78 69 82 73 67 95 73 68 34 44 32 36 115 102 111 114 109 97 116 102 40 92 49 41 44 32 85 86 77 95 77 69 68 73 85 77 41 32 92 50 return 33] 0 "%d")) arg)))
      ;;
      ))


(provide 'setup-verilog)
