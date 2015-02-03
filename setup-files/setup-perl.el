;; Time-stamp: <2015-02-03 15:34:31 kmodi>

;; Perl

(req-package cperl-mode
  :require (isend-mode)
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode) ; cperl-mode instead of perl-mode
  :interpreter (("perl"     . cperl-mode)
                ("perl5"    . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
  (progn
    (setq cperl-indent-level 3
          cperl-close-paren-offset -3
          cperl-continued-statement-offset 3
          cperl-indent-parens-as-block t
          cperl-tab-always-indent t)

    (defun my-cperl-mode-customizations()
      (req-package setup-editing)
      (define-key cperl-mode-map "\177"      'delete-backward-char)
      (define-key cperl-mode-map (kbd "C-j") 'pull-up-line))

    ;; Source: http://stackoverflow.com/a/13632665/1219634
    ;; Debugging Perl scripts using `isend-mode'
    ;;
    ;; " For all kinds of interpreted languages, I use =isend-mode=,
    ;;   which allows sending parts of a buffer to a terminal in another
    ;;   buffer. Here is how you would use it (after having installed it)."
    ;;
    ;; - Open an =ansi-term= buffer with a shell process :
    ;;     =M-x ansi-term RET /bin/tcsh=
    ;; - Open the buffer with the code you want to execute, and associate it to
    ;;   the interpreter buffer:
    ;;     =M-x isend-associate RET *ansi-term* RET=
    ;; - Start perl debugger in the =*ansi-term*= buffer:
    ;;     =perl -d -e 42=
    ;; - Hit =C-RET= in the perl buffer to send the current line to the
    ;;   interpreter in the ansi-term buffer. If a region is active, all
    ;;   lines spanning the region will be sent.

    (defun isend--perl (buf-name)
      "Prepend 'x ' to normal perl instructions.
Leave 'print' instructions untouched."
      (with-current-buffer buf-name
        (goto-char (point-min))
        (unless (looking-at "[[:space:]]*print")
          (insert "x ")))
      (insert-buffer-substring buf-name))

    (defun isend-default-perl-setup ()
      (when (eq major-mode 'cperl-mode)
        (set (make-local-variable 'isend-send-line-function) #'isend--perl)))

    (add-hook 'isend-mode-hook #'isend-default-perl-setup)

    (add-hook 'cperl-mode-hook #'my-cperl-mode-customizations)))


(provide 'setup-perl)
