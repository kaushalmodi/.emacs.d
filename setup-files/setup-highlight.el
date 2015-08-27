;; Time-stamp: <2015-08-27 16:56:46 kmodi>

;; Highlight stuff

;; Example of using Hi-lock keyword:
;; Hi-Lock: (("policy" (0 'hi-yellow prepend)))
;; Hi-Lock: end

(use-package hi-lock
  :config
  (progn
    (>=e "24.4"
        ;; Patch the `hi-lock-face-buffer' aka `highlight-regexp' to pick the
        ;; highlight color automatically
        (defun hi-lock-face-buffer (regexp)
          "Interactively, prompt for REGEXP using `read-regexp'. Uses the
next face from `hi-lock-face-defaults' without prompting.

Use Font lock mode, if enabled, to highlight REGEXP.  Otherwise, use
overlays for highlighting.  If overlays are used, the highlighting
will not update as you type."
          (interactive
           (list
            (hi-lock-regexp-okay
             ;; (read-regexp "Regexp to highlight" 'regexp-history-last))))
             (read-from-minibuffer "Regexp to highlight: " (modi/get-symbol-at-point)))))
          (let* ((hi-lock-auto-select-face t)
                 (face (hi-lock-read-face-name)))
            (or (facep face) (setq face 'hi-yellow))
            (unless hi-lock-mode (hi-lock-mode 1))
            (hi-lock-set-pattern regexp face))))

    ;; Don't scan the file beyond 1000 characters to look for the Hi-Lock
    ;; patterns
    (setq hi-lock-file-patterns-range 1000)

    ;; Don't ask before highlighting any Hi-Lock: pattern found in a file
    ;; Below, (lambda (pattern) t) simply always returns `t' regardless of
    ;; what the `pattern' input is.
    (setq hi-lock-file-patterns-policy (lambda (pattern) t))

    ;; Mark the `hi-lock-file-patterns' variable as safe so that it can be
    ;; set in `.dir-locals.el' files
    (put 'hi-lock-file-patterns 'safe-local-variable 'identity)

    ;; Automatically cycle through the highlighting faces listed in
    ;; `hi-lock-face-defaults' instead of bothering the user to pick a face
    ;; manually each time.
    (setq hi-lock-auto-select-face t)

    ;; Unbind the "C-x w" bindings because "M-s h" bindings provide the same thing
    (define-key hi-lock-map (kbd "C-x w") nil)

    (global-hi-lock-mode 1)

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
                             (buffer-substring-no-properties (region-beginning) (region-end)))
                            (t
                             (find-tag-default-as-symbol-regexp)))))
             (hi-lock-auto-select-face t)
             (face (hi-lock-read-face-name)))
        (or (facep face) (setq face 'hi-yellow))
        (unless hi-lock-mode (hi-lock-mode 1))
        (hi-lock-set-pattern regexp face)))

    (bind-keys
     :map modi-mode-map
      ("C-." . modi/hi-lock-face-symbol-at-point-or-sel))))

;; Highlight Anything
;; https://github.com/boyw165/hl-anything
(use-package hl-anything
  :if (not (bound-and-true-p disable-pkg-hl-anything))
  ;; This package has known to cause issues with the `list-colors-display'
  ;; command. The buffer that opens on calling that command does not show the
  ;; colors. The issue is fixed temporarily by uncommenting the below line
  ;; and restarting emacs - https://github.com/boyw165/hl-anything/issues/14
  ;; Also causes to show this error at startup:
  ;;   org-mode fontification error
  :init
  (progn
    (setq hl-highlight-save-file (locate-user-emacs-file "hl-save")))
  :config
  (progn
    (hl-highlight-mode 1)

    (defun my/hl-anything (local)
      "Highlight the thing at point globally in all buffers.

If LOCAL is non-nil, highlight only in the current buffer."
      (interactive "P")
      (if local
          (hl-highlight-thingatpt-local)
        (hl-highlight-thingatpt-global)))

    (defun my/unhl-anything (local)
      "Un-highlight the thing at point globally in all buffers.

If LOCAL is non-nil, un-highlight only in the current buffer."
      (interactive "P")
      (if local
          (hl-unhighlight-all-local)
        (hl-unhighlight-all-global)))

    (defhydra hydra-hl-anything (:color red
                                 :hint nil)
      "
_h_/_H_ighlight (global/local)           _n_ext hightlight           _s_ave highlights           _t_oggle highlights
_u_/_U_n-highlight (global/local)        _p_revious highlight        _r_estore highlights
"
      ("h" my/hl-anything)
      ("H" (my/hl-anything :local))
      ("u" my/unhl-anything            :color blue)
      ("U" (my/unhl-anything :local)   :color blue)
      ("n" hl-find-next-thing)
      ("p" hl-find-prev-thing)
      ("s" hl-save-highlights          :color blue)
      ("r" hl-restore-highlights       :color blue)
      ("t" hl-global-highlight-on/off)
      ("q" nil "cancel" :color blue))
    (bind-key "C-c h" #'hydra-hl-anything/body modi-mode-map)))

;; Alternative highlighting package
;; https://github.com/glen-dai/highlight-global
(use-package highlight-global
  :load-path "elisp/highlight-global"
  :config
  (progn
    (bind-to-modi-map "h" #'highlight-frame-toggle)
    (bind-to-modi-map "H" #'clear-highlight-frame)))

;; Volatile Highlights
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :config
  (progn
    (volatile-highlights-mode 1)))

;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(use-package auto-highlight-symbol
  :config
  (progn
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (bind-keys
     :map modi-mode-map
      ("C-*"             . auto-highlight-symbol-mode)
      ("<C-kp-multiply>" . auto-highlight-symbol-mode))
    (bind-keys
     :map auto-highlight-symbol-mode-map
      ("M-<"     . ahs-backward)
      ("M->"     . ahs-forward)
      ("M--"     . ahs-back-to-start)
      ("C-x C-'" . ahs-change-range)
      ("C-x C-a" . ahs-edit-mode))))

;; hl-line+
;; http://www.emacswiki.org/emacs/hl-line+.el
(use-package hl-line+
  :config
  (progn
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    ;; Number of seconds of idle time after when the line should be highlighted
    (setq hl-line-idle-interval 5)
    ;; Number of seconds for `hl-line-flash' to highlight the line
    (setq hl-line-flash-show-period 3)))


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
