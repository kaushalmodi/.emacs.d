;; -*- lexical-binding: t; -*-

;; Tree-sitter
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html

;; Contents:
;;
;;  Grammars
;;  Enabled tree-sitter modes
;;  SystemVerilog

(use-package treesit
  :config
  (progn

;;; Grammars
    ;; Where to fetch the grammar sources from. The compiled libraries go to
    ;; ~/.emacs.d/tree-sitter/ (see `treesit-extra-load-path'). Install one
    ;; manually with `M-x treesit-install-language-grammar'.
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

    ;; Compile a missing grammar the first time a mode needs it. This needs
    ;; git and a C compiler in PATH.
    (setq treesit-auto-install-grammar 'always)

    ;; Level 4 also fontifies operators, brackets, delimiters, etc.
    (setq treesit-font-lock-level 4)

;;; Enabled tree-sitter modes
    ;; Visiting a file that would open in the classic mode opens it in the
    ;; tree-sitter mode instead (via `major-mode-remap-alist'). This option
    ;; has a `:set' function, so it must be set with `setopt'.
    ;;
    ;; Not remapped:
    ;; - .html files stay in `web-mode' (Hugo templates); `mhtml-ts-mode' only
    ;;   takes over buffers that would otherwise use `mhtml-mode'.
    ;; - Tcl and csh/tcsh have no tree-sitter mode in Emacs.
    (setopt treesit-enabled-modes '(bash-ts-mode
                                    c-ts-mode
                                    c++-ts-mode
                                    c-or-c++-ts-mode
                                    css-ts-mode
                                    json-ts-mode
                                    mhtml-ts-mode
                                    python-ts-mode
                                    toml-ts-mode
                                    yaml-ts-mode))))

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
                                    2))))


(provide 'setup-treesitter)
