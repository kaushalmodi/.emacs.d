;; Time-stamp: <2015-04-01 09:35:52 kmodi>

;; Perl

(use-package cperl-mode
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode) ; cperl-mode instead of perl-mode
  :interpreter (("perl"     . cperl-mode)
                ("perl5"    . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
  (progn
    (setq cperl-indent-level               3)
    (setq cperl-close-paren-offset         -3)
    (setq cperl-continued-statement-offset 3)
    (setq cperl-indent-parens-as-block     t)
    (setq cperl-tab-always-indent          t)

    (with-eval-after-load 'setup-editing
      (defun my-cperl-mode-customizations()
        (define-key cperl-mode-map "\177"      #'delete-backward-char)
        (define-key cperl-mode-map (kbd "C-j") #'pull-up-line))
      (add-hook 'cperl-mode-hook #'my-cperl-mode-customizations))

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
    (use-package isend-mode
      :config
      (progn

        (defun isend--perl (buf-name)
          "Prepend 'x ' to normal perl instructions.
Leave 'print' instructions untouched."
          (with-current-buffer buf-name
            (goto-char (point-min))
            (unless (looking-at "[[:space:]]*print")
              (insert "x ")))
          (insert-buffer-substring buf-name))

        (defun isend-default-perl-setup ()
          (when (derived-mode-p 'cperl-mode)
            (setq-local isend-send-line-function #'isend--perl)))

        (add-hook 'isend-mode-hook #'isend-default-perl-setup)))))


(provide 'setup-perl)
