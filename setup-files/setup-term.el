;; Time-stamp: <2016-05-19 21:41:18 kmodi>

;; multi-term
;; http://www.emacswiki.org/emacs/multi-term.el
;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/

(use-package multi-term
  :defer t
  :config
  (progn
    ;; term
    (use-package term
      :config
      (progn
        (setq term-prompt-regexp ".*:.*>.*? ")))
    ;; NOTE: After changing the above regexp, call `term-mode' in the term
    ;; buffer for this expression to be effective; because the term buffers
    ;; make a local copy of this var each time a new term buffer is opened or
    ;; `term-mode' is called again.

    ;; Change default multi-term shell to tcsh if available
    (let ((tcsh-bin (executable-find "tcsh")))
      (when tcsh-bin
        (setq multi-term-program tcsh-bin)))

    (setq term-bind-key-alist
          '(("C-c C-c" . term-interrupt-subjob)            ; default
            ("C-c C-e" . term-send-esc)                    ; default
            ("C-c C-j" . term-line-mode)
            ("C-c C-k" . term-char-mode)
            ("C-a"     . term-bol)
            ("C-b"     . term-send-left)
            ("C-f"     . term-send-right)
            ("C-p"     . previous-line)                    ; default
            ("C-n"     . next-line)                        ; default
            ("C-s"     . isearch-forward)                  ; default
            ("C-r"     . isearch-backward)                 ; default
            ("C-m"     . term-send-return)                 ; default
            ("C-y"     . term-paste)                       ; default
            ("M-f"     . term-send-forward-word)           ; default
            ("M-b"     . term-send-backward-word)          ; default
            ("M-o"     . term-send-backspace)              ; default
            ("M-p"     . term-send-up)                     ; default
            ("M-n"     . term-send-down)                   ; default
            ;; ("M-M"     . term-send-forward-kill-word)   ; default
            ("M-d"     . term-send-forward-kill-word)
            ;; ("M-N"     . term-send-backward-kill-word)  ; default
            ("M-DEL"   . term-send-backward-kill-word)
            ("M-r"     . term-send-reverse-search-history) ; default
            ("M-,"     . term-send-raw)                    ; default
            ("M-."     . comint-dynamic-complete)))        ; default

    (setq multi-term-buffer-name "term")))


(provide 'setup-term)
