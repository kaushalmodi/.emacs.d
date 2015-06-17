;; Time-stamp: <2015-06-17 10:09:46 kmodi>

;; Highlight stuff

(global-hi-lock-mode 1)

;; Highlight Anything
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
    (bind-to-modi-map "h" #'hydra-hl-anything/body)))

;; Alternative highlighting package when `hl-anything' has issues
(when (not (featurep 'hl-anything))
  (use-package highlight-global
    :load-path "elisp/highlight-global"
    :config
    (progn
      (bind-to-modi-map "h" #'highlight-frame-toggle)
      (bind-to-modi-map "H" #'clear-highlight-frame))))

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

;; hl-line+
(use-package hl-line+
  :config
  (progn
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    ;; Number of seconds of idle time after when the line should be highlighted
    (setq hl-line-idle-interval 5)
    ;; Number of seconds for `hl-line-flash' to highlight the line
    (setq hl-line-flash-show-period 3)))


(provide 'setup-highlight)
