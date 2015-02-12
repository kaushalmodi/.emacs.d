;; Time-stamp: <2015-02-12 18:02:12 kmodi>

;; Highlight Symbol at point/cursor
;; (require 'highlight-symbol)
;; NOTE: highlight-symbol package is no longer required. The highlight-global
;; package does a very good job!

;; ;; Highlight Global
;; ;; Source: https://github.com/glen-dai/highlight-global
;; ;; Unlike highlight-symbol which does highlighting only in the current buffer,
;; ;; this package highlights all matches accross ALL buffers. Multiple highlights
;; ;; are supported. Different highlights show in different faces.
;; ;; There are 2 ways to select a highlight target:
;; ;; 1. Mark the selection by region (very useful when you want to
;; ;;    highlight a pattern across all symbols),
;; ;; 2. Put cursor on a symbol to pick the symbol to highlight.
;; (req-package highlight-global
;;   :config
;;   (progn
;;     (bind-to-modi-map "h" highlight-frame-toggle)
;;     (bind-to-modi-map "H" clear-highlight-frame)))

;; Highlight Anything
(req-package hl-anything
  :config
  (progn
    (hl-highlight-mode +1)

    (defun my/hl-anything (&optional arg)
      "Wrapper function to call functions to highlight the thing at point either
globally or locally (when called with prefix `C-u')."
      (interactive "p")
      (if (eq arg 4)
          (hl-highlight-thingatpt-local)
        (hl-highlight-thingatpt-global)))

    (defun my/unhl-anything (&optional arg)
      "Wrapper function to call functions to unhighlight all either
globally or locally (when called with prefix `C-u')."
      (interactive "p")
      (if (eq arg 4)
          (hl-unhighlight-all-local)
        (hl-unhighlight-all-global)))

    (defhydra hydra-hl-anything (:color red)
      "hl-anything"
      ("h" my/hl-anything             "hl-global")
      ("H" (my/hl-anything 4)         "hl-local")
      ("u" my/unhl-anything           "unhl-global" :color blue)
      ("U" (my/unhl-anything 4)       "unhl-local" :color blue)
      ("n" hl-find-next-thing         "next")
      ("p" hl-find-prev-thing         "prev")
      ("s" hl-save-highlights         "save" :color blue)
      ("r" hl-restore-highlights      "restore" :color blue)
      ("t" hl-global-highlight-on/off "toggle")
      ("q" nil                        "cancel" :color blue))

    (bind-to-modi-map "h" hydra-hl-anything/body)))

;; Volatile Highlights
;; https://github.com/k-talo/volatile-highlights.el
(req-package volatile-highlights
  :config
  (progn
    (volatile-highlights-mode t)))


;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(req-package auto-highlight-symbol
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-*"             . auto-highlight-symbol-mode)
     ("<C-kp-multiply>" . auto-highlight-symbol-mode))
    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<left>"    . ahs-backward)
     ("M-<right>"   . ahs-forward)
     ("M-S-<left>"  . ahs-backward-definition)
     ("M-S-<right>" . ahs-forward-definition)
     ("M--"         . ahs-back-to-start)
     ("C-x C-'"     . ahs-change-range)
     ("C-x C-a"     . ahs-edit-mode))))

(>=e "24.4"
     ;; Patch the `hi-lock-face-buffer' aka `highlight-regexp' to pick the
     ;; highlight color automatically
;;;###autoload
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


(provide 'setup-highlight)
