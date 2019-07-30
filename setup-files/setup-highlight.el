;; Time-stamp: <2020-07-11 15:43:41 kmodi>

;; Highlight stuff

;; Example of using Hi-lock keyword:
;; Hi-Lock: (("policy" (0 'hi-yellow prepend)))
;; Hi-Lock: end

(use-package hi-lock
  :config
  (progn
    ;; Patch the `hi-lock-face-buffer' aka `highlight-regexp' to pick the
    ;; selected region to derive a regexp if a region is active.
    (defun hi-lock-face-buffer (regexp &optional face)
      "Set face of each match of REGEXP to FACE.
Interactively, prompt for REGEXP using `read-regexp', then FACE.
Use the global history list for FACE.

Use Font lock mode, if enabled, to highlight REGEXP.  Otherwise,
use overlays for highlighting.  If overlays are used, the
highlighting will not update as you type."
      (interactive
       (list
        (hi-lock-regexp-okay
         (read-regexp "Regexp to highlight"
                      (if (use-region-p)
                          ;; Use `rx' to generate regexp for selected text.
                          ;; Example: regexp to find "a.b" text would be
                          ;; "a\.b"
                          (let ((str (buffer-substring-no-properties
                                      (region-beginning) (region-end))))
                            (eval `(rx ,str)))
                        'regexp-history-last)))
        (hi-lock-read-face-name)))
      (or (facep face) (setq face 'hi-yellow))
      (unless hi-lock-mode (hi-lock-mode 1))
      (hi-lock-set-pattern regexp face))

    ;; Don't scan the file beyond 1000 characters to look for the Hi-Lock patterns.
    (setq hi-lock-file-patterns-range 1000)

    ;; Don't ask before highlighting any Hi-Lock: pattern found in a file
    ;; Below, (lambda (pattern) t) simply always returns `t' regardless of
    ;; what the `pattern' input is.
    (setq hi-lock-file-patterns-policy (lambda (pattern) t))

    ;; Mark the `hi-lock-file-patterns' variable as safe so that it can be
    ;; set in `.dir-locals.el' files.
    (put 'hi-lock-file-patterns 'safe-local-variable 'identity)

    ;; Automatically cycle through the highlighting faces listed in
    ;; `hi-lock-face-defaults' instead of bothering the user to pick a face
    ;; manually each time.
    (setq hi-lock-auto-select-face t)

    (defun modi/hi-lock-face-symbol-at-point-or-sel ()
      "If a region is selected, highlight each instance of that.
Else highlight each instance of the symbol at point.

Uses the next face from `hi-lock-face-defaults' without prompting,
unless you use a prefix argument. Uses `find-tag-default-as-symbol-regexp' to
retrieve the symbol at point.

This uses Font lock mode if it is enabled; otherwise it uses overlays,
in which case the highlighting will not update as you type."
      (interactive)
      (let* ((regexp (hi-lock-regexp-okay
                      (cond ((use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))
                            (t
                             (find-tag-default-as-symbol-regexp)))))
             (hi-lock-auto-select-face t)
             (face (hi-lock-read-face-name)))
        (or (facep face) (setq face 'hi-yellow))
        (unless hi-lock-mode (hi-lock-mode 1))
        (hi-lock-set-pattern regexp face)))

    ;; Enable `hi-lock-mode' in `text-mode' too
    ;; The hi-lock fontification will not be visible (the `font-lock-keywords'
    ;; variable will not be updated unless `font-lock-fontified' is already `t'.
    ;; This was derived by studying the definition of `hi-lock-font-lock-hook'
    ;; function.
    (defun modi/hi-lock-enable-in-text-mode ()
      (setq-local font-lock-fontified t))
    (add-hook 'text-mode-hook #'modi/hi-lock-enable-in-text-mode)

    (defun modi/unhighlight-all-in-buffer ()
      "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
      (interactive)
      ;; `unhighlight-regexp' is aliased to `hi-lock-unface-buffer'
      (hi-lock-unface-buffer t))
    (bind-key "h U" #'modi/unhighlight-all-in-buffer search-map)

    (global-hi-lock-mode 1)

    ;; Unbind the "C-x w" bindings because "M-s h" bindings provide the same thing.
    (bind-key "C-x w" nil hi-lock-map)

    (bind-keys
     :map modi-mode-map
     ("C-." . modi/hi-lock-face-symbol-at-point-or-sel))))

;; Highlight Global
;; https://github.com/glen-dai/highlight-global
(use-package highlight-global
  :load-path "elisp/highlight-global"
  :commands (highlight-global-hl-frame-toggle
             highlight-global-clear-hl-frame)
  :init
  (progn
    (bind-to-modi-map "h" #'highlight-global-hl-frame-toggle)
    (bind-to-modi-map "H" #'highlight-global-clear-hl-frame)))

;; ;; Volatile Highlights
;; ;; https://github.com/k-talo/volatile-highlights.el
;; (use-package volatile-highlights
;;   :config
;;   (progn
;;     (volatile-highlights-mode 1)))

;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(use-package auto-highlight-symbol
  :bind (:map modi-mode-map
         ("C-*"             . auto-highlight-symbol-mode)
         ("<C-kp-multiply>" . auto-highlight-symbol-mode))
  :config
  (progn
    (setq ahs-default-range 'ahs-range-whole-buffer)

    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<"     . ahs-backward)
     ("M->"     . ahs-forward)
     ("M--"     . ahs-back-to-start)
     ("C-x C-'" . ahs-change-range)
     ("C-x C-a" . ahs-edit-mode))))

;; Highlight line
(use-package hl-line
  :config
  (progn
    ;; Highlight the line only in the active window
    (setq hl-line-sticky-flag nil)

    ;; hl-line+
    ;; http://www.emacswiki.org/emacs/hl-line+.el
    (use-package hl-line+
      :load-path "elisp/manually-synced/hl-line-plus"
      :config
      (progn
        (toggle-hl-line-when-idle 1) ; Highlight line only when idle
        ;; Number of seconds of idle time after when the line should be highlighted
        (setq hl-line-idle-interval 5)
        ;; Number of seconds for `hl-line-flash' to highlight the line
        (setq hl-line-flash-show-period 3)))))


(provide 'setup-highlight)

;; If `hi-lock-file-patterns-policy' is set to `nil' or `'never', you will need
;; to call `M-x hi-lock-find-patterns' or `M-s h f' to highlighted all
;; occurrences of Hi-Lock: patterns specified in the file.

;; The Hi-Lock regexp forms are in the form of font lock keywords. Do
;; `C-h v font-lock-keywords' to learn more.

;; Hi-Lock: (("<REGEXP>" (<SUBEXP-0> '<FACE-0> [<OVERRIDE> [<LAXMATCH>]])
;;                       (<SUBEXP-1> '<FACE-1> [<OVERRIDE> [<LAXMATCH>]])
;;                       .. ))
;; Hi-Lock: end

;; OVERRIDE and LAXMATCH are flags.
;; If OVERRIDE is t, existing fontification can be overwritten.
;;   If `keep', only parts not already fontified are highlighted.
;;   If `prepend', existing fontification is merged with the new, in
;;     which the new fontification takes precedence.
;;   If `append', existing fontification is merged with the new, in
;;     which the existing fontification takes precedence.
;; If LAXMATCH is non-nil, that means don't signal an error if there is
;; no match for SUBEXP in REGEXP.

;; Examples of Hi-Lock patterns:

;; Highlight outshine headers in `shell-script-mode':
;; # Hi-lock: (("\\(^\\s< *\\**\\)\\(\\* *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))

;; Highlight outshine headers in `emacs-lisp-mode':
;; ;; Hi-lock: (("\\(^;\\{3,\\}\\)\\( *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))

;; Highlight outshine headers in `verilog-mode':
;; // Hi-lock: (("\\(^// \\**\\)\\(\\* *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))
;; If you do not want to modify the source files with the Hi-Lock meta data,
;; you can set the `hi-lock-file-patterns' variable using `.dir-locals.el' files
;; as below:
;;   (("PATH/TO/DIR"
;;     . ((verilog-mode . ((hi-lock-file-patterns
;;                          . (("\\(^// \\**\\)\\(\\* *.*\\)"
;;                              (1 'org-hide prepend)
;;                              (2 '(:inherit org-level-1
;;                                   :height 1.3
;;                                   :weight bold
;;                                   :overline t
;;                                   :underline t)
;;                                 prepend))))
;;                         ))))
;;    )
