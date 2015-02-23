;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Verilog

(use-package verilog-mode
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

    (defun my-verilog-mode-customizations()
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
    (add-hook 'verilog-mode-hook #'my-verilog-mode-customizations)

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
