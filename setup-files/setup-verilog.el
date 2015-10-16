;; Time-stamp: <2015-10-15 18:57:32 kmodi>

;; Verilog

;; Contents:
;;
;;  Variables
;;  modi/verilog-find-module-instance
;;  modi/verilog-get-header
;;  modi/verilog-jump-to-header-dwim
;;  modi/verilog-which-func
;;  modi/verilog-update-which-func-format
;;  modi/verilog-jump-to-module-at-point
;;  modi/verilog-find-parent-module
;;  my/verilog-selective-indent
;;  imenu support
;;  hideshow
;;  hydra-verilog-template
;;  my/verilog-mode-customizations

(use-package verilog-mode
  :load-path "elisp/verilog-mode"
  :mode (("\\.[st]*v[hp]*\\'" . verilog-mode) ; .v, .sv, .svh, .tv, .vp
         ("\\.psl\\'"         . verilog-mode)
         ("\\.vinc\\'"        . verilog-mode))
  :config
  (progn

;;; Variables
    (setq verilog-indent-level             3)   ; 3 (default)
    (setq verilog-indent-level-module      3)   ; 3
    (setq verilog-indent-level-declaration 3)   ; 3
    (setq verilog-indent-level-behavioral  3)   ; 3
    (setq verilog-indent-level-directive   3)   ; 1
    (setq verilog-case-indent              2)   ; 2
    (setq verilog-auto-newline             nil) ; t
    (setq verilog-auto-indent-on-newline   t)   ; t
    (setq verilog-tab-always-indent        t)   ; t
    (setq verilog-minimum-comment-distance 10)  ; 10
    (setq verilog-indent-begin-after-if    t)   ; t
    (setq verilog-auto-lineup              nil) ; 'declarations
    (setq verilog-align-ifelse             nil) ; nil
    (setq verilog-auto-endcomments         t)   ; t
    (setq verilog-tab-to-comment           nil) ; nil
    (setq verilog-date-scientific-format   t)   ; t

    (defconst modi/verilog-identifier-re "\\b[a-zA-Z][a-zA-Z0-9:_]*"
      ;; The : is to allow parsing extern methods like class::task
      "Regexp for a valid verilog identifier.")

    (defconst modi/verilog-module-instance-re
      (concat "^\\s-*"
              "\\(?1:" ; force group number to 1
              modi/verilog-identifier-re ; module name
              "\\)"
              "\\(?:\n\\|\\s-\\)+" ; newline/space
              ;; optional hardware parameters followed by optional comments
              ;; followed by optional space/newline before instance name
              "\\(#([^;]+?)\\(\\s-*//.*?\\)*[^;\\./]+?\\)*"
              "\\(?2:" ; force group number to 2
              modi/verilog-identifier-re
              "\\)" ; instance name
              "\\(?:\n\\|\\s-\\)*" ; optional newline/space
              "(" ; opening parenthesis `(' before port list
              )
      "Regexp for a valid verilog module instance declaration.")

    (defconst modi/verilog-header-re
      (concat "^\\s-*"
              "\\([a-z]+\\s-+\\)*" ; virtual, local, protected
              "\\(?1:" "case" ; force group number to 1
              "\\|" "class"
              "\\|" "clocking"
              "\\|" "`define"
              "\\|" "function"
              "\\|" "group"
              "\\|" "interface"
              "\\|" "module"
              "\\|" "program"
              "\\|" "primitive"
              "\\|" "package"
              "\\|" "property"
              "\\|" "sequence"
              "\\|" "specify"
              "\\|" "table"
              "\\|" "task" "\\)"
              "\\s-+"
              "\\([a-z]+\\s-+\\)*" ; void, static, automatic, ..
              "\\(?2:" modi/verilog-identifier-re "\\)" ; block name
                                        ; force group number to 2
              "\\b"
              )
      "Regexp for a valid verilog block header statement.")

    (defvar modi/verilog-keywords-re nil
      "Regexp for reserved verilog keywords which should not be incorrectly
parsed as a module or instance name.")

    (let ((cnt 1)
          ;; `verilog-keywords' list is defined in the `verilog-mode.el'
          (max-cnt (safe-length verilog-keywords)))
      (dolist (keyword verilog-keywords)
        (cond
         ((= cnt 1)       (setq modi/verilog-keywords-re
                                (concat "\\("
                                        "\\b" keyword "\\b")))
         ((= cnt max-cnt) (setq modi/verilog-keywords-re
                                (concat modi/verilog-keywords-re
                                        "\\|"
                                        "\\b" keyword "\\b" "\\)")))
         (t               (setq modi/verilog-keywords-re
                                (concat modi/verilog-keywords-re
                                        "\\|"
                                        "\\b" keyword "\\b"))))
        (setq cnt (1+ cnt))))

;;; modi/verilog-find-module-instance
    (defun modi/verilog-find-module-instance (&optional fwd)
      "Function to return the module and instance name within which
the point is currently.

Using the optional argument FWD does the search in forward direction.

This function updates the local variable `modi/verilog-which-func-xtra'."
      (let (instance-name return-val) ; return-val will be nil by default
        (setq-local modi/verilog-which-func-xtra nil) ; reset
        (save-excursion
          (when (if fwd
                    (re-search-forward modi/verilog-module-instance-re nil :noerror)
                  (re-search-backward modi/verilog-module-instance-re nil :noerror))
            ;; Ensure that text in line or block comments is not incorrectly
            ;; parsed as a module instance
            (when (not (equal (face-at-point) 'font-lock-comment-face))
              ;; (message "---- 1 ---- %s" (match-string 1))
              ;; (message "---- 2 ---- %s" (match-string 2))
              ;; (message "---- 3 ---- %s" (match-string 3))
              (setq-local modi/verilog-which-func-xtra (match-string 1)) ; module name
              (setq instance-name (match-string 2)) ; instance name

              (when (and (stringp modi/verilog-which-func-xtra)
                         (string-match modi/verilog-keywords-re
                                       modi/verilog-which-func-xtra))
                (setq-local modi/verilog-which-func-xtra nil))

              (when (and (stringp instance-name)
                         (string-match modi/verilog-keywords-re
                                       instance-name))
                (setq instance-name nil))

              (when (and modi/verilog-which-func-xtra
                         instance-name)
                (setq return-val instance-name)))))
        (when (featurep 'which-func)
          (modi/verilog-update-which-func-format))
        return-val))

;;; modi/verilog-get-header
    (defun modi/verilog-get-header (&optional fwd)
      "Function to return the block name under which the point is currently
present.

Using the optional argument FWD does the search in forward direction.

Few examples of what is considered as a block: module, class, package, function,
task, `define

This function updates the local variable `modi/verilog-which-func-xtra'."
      (let (block-type block-name return-val) ; return-val will be nil by default
        (setq-local modi/verilog-which-func-xtra nil) ; reset
        (save-excursion
          (when (if fwd
                    (re-search-forward modi/verilog-header-re nil :noerror)
                  (re-search-backward modi/verilog-header-re nil :noerror))
            ;; Ensure that text in line or block comments is not incorrectly
            ;; parsed as a Verilog block header
            (when (not (equal (face-at-point) 'font-lock-comment-face))
              ;; (message "---- 1 ---- %s" (match-string 1))
              ;; (message "---- 2 ---- %s" (match-string 2))
              ;; (message "---- 3 ---- %s" (match-string 3))
              ;; (message "---- 4 ---- %s" (match-string 4))
              (setq block-type (match-string 1))
              (setq block-name (match-string 2))

              (when (and (stringp block-name)
                         (not (string-match modi/verilog-keywords-re
                                            block-name)))
                (setq-local modi/verilog-which-func-xtra
                            (cond
                             ((string= "class"     block-type) "class")
                             ((string= "clocking"  block-type) "clk")
                             ((string= "`define"   block-type) "macro")
                             ((string= "group"     block-type) "group")
                             ((string= "module"    block-type) "mod")
                             ((string= "interface" block-type) "if")
                             ((string= "package"   block-type) "pkg")
                             ((string= "sequence"  block-type) "seq")
                             (t (substring block-type 0 4)))) ; first 4 chars
                (setq return-val block-name)))))
        (when (featurep 'which-func)
          (modi/verilog-update-which-func-format))
        return-val))

;;; modi/verilog-jump-to-header-dwim
    (defun modi/verilog-jump-to-header-dwim (&optional fwd)
      "Jump to a module instantiation header above the current point. If
a module instantiation is not found, jump to a block header if available.

Using the optional argument FWD does the search in forward direction.

Few examples of what is considered as a block: module, class, package, function,
task, `define."
      (interactive "P")
      (if (modi/verilog-find-module-instance fwd)
          (if fwd
              (re-search-forward modi/verilog-module-instance-re nil :noerror)
            (re-search-backward modi/verilog-module-instance-re nil :noerror))
        (if fwd
            (re-search-forward modi/verilog-header-re nil :noerror)
          (re-search-backward modi/verilog-header-re nil :noerror))))

    (defun modi/verilog-jump-to-header-dwim-fwd ()
      "Executes `modi/verilog-jump-to-header' with non-nil prefix argument so
the point jumps to a module instantiation/block header *below* the current
point."
      (interactive)
      (modi/verilog-jump-to-header-dwim :fwd))

    (bind-keys
     :map verilog-mode-map
      ("C-^" . modi/verilog-jump-to-header-dwim)
      ("C-&" . modi/verilog-jump-to-header-dwim-fwd))

    (with-eval-after-load 'which-func
      (add-to-list 'which-func-modes 'verilog-mode)

;;; modi/verilog-which-func
      (defun modi/verilog-which-func ()
        (setq-local which-func-functions '(modi/verilog-find-module-instance
                                           modi/verilog-get-header))
        (which-function-mode))
      (add-hook 'verilog-mode-hook #'modi/verilog-which-func)

;;; modi/verilog-update-which-func-format
      (defun modi/verilog-update-which-func-format ()
        (let ((modi/verilog-which-func-echo-help
               (concat "mouse-1/scroll up: jump to header UP" "\n"
                       "mouse-3/scroll-down: jump to header DOWN")))

          (setq-local which-func-keymap
                      (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line mouse-1] #'modi/verilog-jump-to-header-dwim)
                        (define-key map [mode-line mouse-4] #'modi/verilog-jump-to-header-dwim) ; scroll up
                        (define-key map [mode-line mouse-2] #'modi/verilog-jump-to-header-dwim-fwd)
                        (define-key map [mode-line mouse-5] #'modi/verilog-jump-to-header-dwim-fwd) ; scroll down
                        map))

          (if modi/verilog-which-func-xtra
              (setq-local which-func-format
                          `("["
                            (:propertize which-func-current
                             local-map ,which-func-keymap
                             face (font-lock-keyword-face :weight bold)
                             mouse-face mode-line-highlight
                             help-echo ,modi/verilog-which-func-echo-help)
                            ":"
                            (:propertize modi/verilog-which-func-xtra
                             local-map ,which-func-keymap
                             face font-lock-keyword-face
                             mouse-face mode-line-highlight
                             help-echo ,modi/verilog-which-func-echo-help)
                            "]"))
            (setq-local which-func-format
                        `("["
                          (:propertize which-func-current
                           local-map ,which-func-keymap
                           face font-lock-keyword-face
                           mouse-face mode-line-highlight
                           help-echo ,modi/verilog-which-func-echo-help)
                          "]"))))))

    (with-eval-after-load 'projectile

;;; modi/verilog-jump-to-module-at-point
      (defun modi/verilog-jump-to-module-at-point ()
        "If the point is somewhere in a module instance, jump to the definition
of that module.

It is required to have `ctags' executable and `projectile' package installed
for this to work."
        (interactive)
        (when (and (executable-find "ctags")
                   (locate-file "TAGS" (list `,(projectile-project-root))))
          ;; `modi/verilog-which-func-xtra' contains the module name in
          ;; whose instance declaration the point is currently.
          (if (and (modi/verilog-find-module-instance)
                   modi/verilog-which-func-xtra)
              (find-tag modi/verilog-which-func-xtra)
            (pop-tag-mark))))

      (key-chord-define verilog-mode-map "\\\\" #'modi/verilog-jump-to-module-at-point) ; "\\"

      (with-eval-after-load 'ag

;;; modi/verilog-find-parent-module
        (defun modi/verilog-find-parent-module ()
          "Find the places where the current verilog module is instantiated in
the project."
          (interactive)
          (let ((verilog-module-re (concat "^\\s-*" ; elisp regexp
                                           "\\(?:module\\)\\s-+" ; shy group
                                           "\\("
                                           modi/verilog-identifier-re
                                           "\\)\\b"))
                module-name
                module-instance-re)
            (save-excursion
              (re-search-backward verilog-module-re)
              (setq module-name (match-string 1))
              (setq module-instance-re
                    (concat "^\\s*" ; pcre regex
                            module-name
                            "\\s+"
                            "(#\\s*\\((\\n|.)*?\\))*" ; optional hardware parameters
                                        ; '(\n|.)*?' does non-greedy multi-line grep
                            "(\\n|.)*?" ; optional newline/space before instance name
                            "\\K" ; don't highlight anything till this point
                            modi/verilog-identifier-re ; instance name
                            "(?=[^a-zA-Z0-9_]*\\()")) ; optional space/newline after instance name
                                        ; and before opening parenthesis `('
                                        ; don't highlight anything in (?=..)
              (ag-regexp module-instance-re (projectile-project-root)))))
        (key-chord-define verilog-mode-map "^^" #'modi/verilog-find-parent-module)))

    ;; Unbind the backtick binding done to `electric-verilog-tick'
    ;; With binding done to electric-verilog-tick, it's not possible to type
    ;; backticks on multiple lines simultaneously in multiple-cursors mode
    (define-key verilog-mode-map "\`" nil)
    ;; Bind `verilog-header' to "C-c C-H" instead of to "C-c C-h"
    (define-key verilog-mode-map (kbd "C-c C-h") nil)
    (define-key verilog-mode-map (kbd "C-c C-S-h") #'verilog-header)

;;; my/verilog-selective-indent
    ;; http://emacs.stackexchange.com/a/8033/115
    (defun my/verilog-selective-indent (&rest args)
      "Return t if either of the below is true:
- The current line starts with optional whitespace and then `// *'
- The current line contains `//.'

If the first match above is true, also delete any preceding white space too.

Tweak the verilog-mode indentation to skip the lines that begin with
“<optional-white-space>// *” in order to not break any `outline-mode'
or `outshine' functionality.

The match with `//.' resolves this issue:
  http://www.veripool.org/issues/922-Verilog-mode-Consistent-comment-column
"
      (save-excursion
        (beginning-of-line)
        (let* ((match1 (looking-at "^[[:blank:]]*// \\*")) ; // *
               (match2 (looking-at "^.*//\\.")) ; //.
               (match (or match1 match2)))
          (when match1
            (delete-horizontal-space))
          match)))
    ;; Advise the indentation behavior of `indent-region' done using `C-M-\'
    (advice-add 'verilog-indent-line-relative :before-until #'my/verilog-selective-indent)
    ;; Advise the indentation done by hitting `TAB'
    (advice-add 'verilog-indent-line          :before-until #'my/verilog-selective-indent)

;;; imenu support
    ;; Imenu expression for Verilog mode.  See `imenu-generic-expression'
    (defconst modi/verilog-imenu-generic-expression
      '(("*Modules*"
         "^\\s-*\\(\\(m\\(odule\\|acromodule\\)\\)\\|primitive\\)\\s-+\\(?1:[A-Za-z_][A-Za-z0-9_.:]+\\)" 1)
        ("*Classes*"
         "^\\s-*\\(\\(static\\|local\\|virtual\\|protected\\)\\b\\s-+\\)*class\\s-+\\(?1:[A-Za-z_][A-Za-z0-9_]+\\)" 1)
        ("*Tasks*"
         "^\\s-*\\(\\(static\\|local\\|virtual\\|protected\\)\\b\\s-+\\)*task\\s-+\\(\\(static\\|automatic\\)\\s-+\\)*\\(?1:[A-Za-z_][A-Za-z0-9_:]+\\)" 1)
        ("*Functions*"
         "^\\s-*\\(\\(static\\|local\\|virtual\\|protected\\)\\b\\s-+\\)*function\\s-+\\([a-z_]+\\s-+\\)*\\(?1:[A-Za-z_][A-Za-z0-9_:]+\\)" 1))
      "Generic regular expression to parse Verilog elements for `imenu'.")

;;; hideshow
    (with-eval-after-load 'setup-fold
      (add-to-list 'hs-special-modes-alist
                   `(verilog-mode ,(concat "\\b\\(begin"
                                           "\\|task"
                                           "\\|function"
                                           "\\|class"
                                           "\\|module"
                                           "\\|program"
                                           "\\|interface"
                                           "\\|module"
                                           "\\|case"
                                           "\\|fork\\)\\b")
                                  ,(concat "\\b\\(end"
                                           "\\|endtask"
                                           "\\|endfunction"
                                           "\\|endclass"
                                           "\\|endmodule"
                                           "\\|endprogram"
                                           "\\|endinterface"
                                           "\\|endmodule"
                                           "\\|endcase"
                                           "\\|join\\|join_none\\|join_any\\)\\b")
                                  nil verilog-forward-sexp-function)))

;;; hydra-verilog-template
    (defhydra hydra-verilog-template (:color blue
                                      :hint nil)
      "
_i_nitial        _?_ if             _j_ fork           _A_ssign                _uc_ uvm-component
_b_egin          _:_ else-if        _m_odule           _I_nput                 _uo_ uvm-object
_a_lways         _f_or              _g_enerate         _O_utput
^^               _w_hile            _p_rimitive        _=_ inout
^^               _r_epeat           _s_pecify          _S_tate-machine         _h_eader
^^               _c_ase             _t_ask             _W_ire                  _/_ comment
^^               case_x_            _F_unction         _R_eg
^^               case_z_            ^^                 _D_efine-signal
"
      ("a"   verilog-sk-always)
      ("b"   verilog-sk-begin)
      ("c"   verilog-sk-case)
      ("f"   verilog-sk-for)
      ("g"   verilog-sk-generate)
      ("h"   verilog-sk-header)
      ("i"   verilog-sk-initial)
      ("j"   verilog-sk-fork)
      ("m"   verilog-sk-module)
      ("p"   verilog-sk-primitive)
      ("r"   verilog-sk-repeat)
      ("s"   verilog-sk-specify)
      ("t"   verilog-sk-task)
      ("w"   verilog-sk-while)
      ("x"   verilog-sk-casex)
      ("z"   verilog-sk-casez)
      ("?"   verilog-sk-if)
      (":"   verilog-sk-else-if)
      ("/"   verilog-sk-comment)
      ("A"   verilog-sk-assign)
      ("F"   verilog-sk-function)
      ("I"   verilog-sk-input)
      ("O"   verilog-sk-output)
      ("S"   verilog-sk-state-machine)
      ("="   verilog-sk-inout)
      ("uc"  verilog-sk-uvm-component)
      ("uo"  verilog-sk-uvm-object)
      ("W"   verilog-sk-wire)
      ("R"   verilog-sk-reg)
      ("D"   verilog-sk-define-signal)
      ("q"   nil nil :color blue)
      ("C-g" nil nil :color blue))
    (bind-key "C-c C-t" #'hydra-verilog-template/body verilog-mode-map)

    ;; Uncomment the lines for which the advice needs to be removed
    ;; (advice-remove 'verilog-indent-line-relative #'my/verilog-selective-indent)
    ;; (advice-remove 'verilog-indent-line          #'my/verilog-selective-indent)

    ;; Reset the verilog-mode abbrev table
    (clear-abbrev-table verilog-mode-abbrev-table)

;;; my/verilog-mode-customizations
    (defun my/verilog-mode-customizations ()
      ;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
      (font-lock-add-keywords nil
                              '(("\\b\\(FIXME\\|TODO\\|BUG\\)\\b" 1
                                 font-lock-warning-face t)))
      ;; Above solution highlights those keywords anywhere in the buffer (not
      ;; just in comments). To do the highlighting intelligently, install the
      ;; `fic-mode' package - https://github.com/lewang/fic-mode

      ;; ;; Enable orgstruct mode
      ;; (setq-local orgstruct-heading-prefix-regexp "//; ")
      ;; (turn-on-orgstruct++)

      (setq imenu-generic-expression
            (append imenu-generic-expression
                    modi/verilog-imenu-generic-expression))
      ;; Replace tabs with spaces when saving files in verilog-mode
      (add-hook 'before-save-hook #'modi/untabify-buffer nil :local))
    (add-hook 'verilog-mode-hook #'my/verilog-mode-customizations)

    ;; Force the `imenu-generic-expression' to be `modi/verilog-imenu-generic-expression'
    ;; for `verilog-mode'
    (defun modi/outshine-hook-mod (orig-fun &rest args)
      (apply orig-fun args)
      (when (derived-mode-p 'verilog-mode)
        (setq imenu-generic-expression
              (append '(("*Level 1*"
                         "^// \\* \\(?1:.*$\\)" 1)
                        ("*Level 2*"
                         "^// \\*\\* \\(?1:.*$\\)" 1)
                        ("*Level 3*"
                         "^// \\*\\* \\(?1:.*$\\)" 1))
                      modi/verilog-imenu-generic-expression))))
    (advice-add 'outshine-hook-function :around #'modi/outshine-hook-mod)

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
