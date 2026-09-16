;; -*- lexical-binding: t; -*-

;; Tree-sitter
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html

;; Contents:
;;
;;  Grammars
;;  Enabled tree-sitter modes
;;  SystemVerilog

;; Everything below is skipped when this Emacs was built without tree-sitter
;; (`treesit-available-p' is nil), so the classic modes keep being used.
(defconst modi/treesit-available (and (fboundp 'treesit-available-p)
                                      (treesit-available-p))
  "Non-nil if this Emacs has tree-sitter support compiled in.")

(defconst modi/treesit-mode-remaps '((sh-mode . bash-ts-mode)
                                     (c-mode . c-ts-mode)
                                     (c++-mode . c++-ts-mode)
                                     (c-or-c++-mode . c-or-c++-ts-mode)
                                     (css-mode . css-ts-mode)
                                     (js-json-mode . json-ts-mode)
                                     (mhtml-mode . mhtml-ts-mode)
                                     (python-mode . python-ts-mode)
                                     (conf-toml-mode . toml-ts-mode)
                                     (yaml-mode . yaml-ts-mode))
  "Classic mode to tree-sitter mode pairs to enable.

Not remapped:
- .html files stay in `web-mode' (Hugo templates); `mhtml-ts-mode' only
  takes over buffers that would otherwise use `mhtml-mode'.
- Tcl and csh/tcsh have no tree-sitter mode in Emacs.")

(when modi/treesit-available
  (use-package treesit
    :config
    (progn

;;; Grammars
      ;; Where to fetch the grammar sources from. The compiled libraries go
      ;; to ~/.emacs.d/tree-sitter/ (see `treesit-extra-load-path'). Install
      ;; one manually with `M-x treesit-install-language-grammar'.
      (setq treesit-language-source-alist
            '((bash "https://github.com/tree-sitter/tree-sitter-bash")
              (c "https://github.com/tree-sitter/tree-sitter-c")
              (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
              (css "https://github.com/tree-sitter/tree-sitter-css")
              (html "https://github.com/tree-sitter/tree-sitter-html")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript") ;Needed by `mhtml-ts-mode'
              (json "https://github.com/tree-sitter/tree-sitter-json")
              (python "https://github.com/tree-sitter/tree-sitter-python")
              (systemverilog "https://github.com/gmlarumbe/tree-sitter-systemverilog") ;For `verilog-ts-mode'
              (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
              (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

      ;; Emacs 31+: compile a missing grammar the first time a mode needs
      ;; it. This needs git and a C compiler in PATH.
      (setq treesit-auto-install-grammar 'always)

      ;; Level 4 also fontifies operators, brackets, delimiters, etc.
      (setq treesit-font-lock-level 4)

;;; Enabled tree-sitter modes
      ;; Visiting a file that would open in the classic mode opens it in
      ;; the tree-sitter mode instead.
      (if (boundp 'treesit-enabled-modes)
          ;; Emacs 31+: this option drives `major-mode-remap-alist' through
          ;; its `:set' function, so it must be set with `setopt'.
          (setopt treesit-enabled-modes (mapcar #'cdr modi/treesit-mode-remaps))
        ;; Emacs 29 and 30
        (dolist (remap modi/treesit-mode-remaps)
          (add-to-list 'major-mode-remap-alist remap)))))

;;; SystemVerilog
  ;; https://github.com/gmlarumbe/verilog-ts-mode
  ;; `verilog-ts-mode' derives from `verilog-mode', so everything hooked to
  ;; `verilog-mode-hook' in setup-verilog.el applies to it as well.
  (use-package verilog-ts-mode
    :init
    (progn
      (add-to-list 'major-mode-remap-alist '(verilog-mode . verilog-ts-mode)))
    :config
    (progn
      (setq verilog-ts-indent-level (if (boundp 'modi/verilog-indent-level)
                                        modi/verilog-indent-level
                                      2)))))

;;; Fallbacks without tree-sitter
;; Emacs has no classic YAML mode; `yaml-ts-mode-maybe' falls back to
;; `fundamental-mode' when the grammar cannot be used. (`:ensure' runs even
;; when a use-package `:if' is nil, hence the `unless' wrapper.)
(unless modi/treesit-available
  (use-package yaml-mode
    :ensure t
    :mode "\\.ya?ml\\'"))


(provide 'setup-treesitter)
