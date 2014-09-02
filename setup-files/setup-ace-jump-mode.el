;; Time-stamp: <2014-08-12 18:05:08 kmodi>

;; Ace Jump
;; Source: http://www.emacswiki.org/emacs/AceJump

(req-package ace-jump-mode
  :require key-chord
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ;; Important to use my minor mode map as I want my bindings to override
     ;; bindings in other major modes (esp org-mode)
     ("C-c SPC" . ace-jump-mode))
    ;; Alternate binding for `ace-jump-mode'
    (key-chord-define-global "l;" 'ace-jump-mode)))

(provide 'setup-ace-jump-mode)

;;         `ace-jump-mode-BINDING' -> `ace-jump-word-mode'
;;     C-u `ace-jump-mode-BINDING' -> `ace-jump-char-mode'
;; C-u C-u `ace-jump-mode-BINDING' -> `ace-jump-line-mode'
