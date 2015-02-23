;; Time-stamp: <2015-02-23 11:25:05 kmodi>

;; Smart Compile

(use-package smart-compile
  :config
  (progn
    ;; http://stackoverflow.com/a/15724162/1219634
    (defun do-execute (exe)
      (with-current-buffer "*eshell*"
        (goto-char (point-max))
        (insert exe)
        (eshell-send-input))
      (switch-to-buffer-other-window "*eshell*")
      (end-of-buffer))

    (defun save-compile-execute ()
      (interactive)
      (lexical-let ((exe (smart-compile-string "./%n"))
                    finish-callback)
        ;; when compilation is done, execute the program
        ;; and remove the callback from
        ;; compilation-finish-functions
        (setq finish-callback
              (lambda (buf msg)
                (do-execute exe)
                (setq compilation-finish-functions
                      (delq finish-callback compilation-finish-functions))))
        (push finish-callback compilation-finish-functions))
      (smart-compile 1))))


(provide 'setup-smart-compile)
