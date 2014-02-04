;; Time-stamp: <2013-12-02 17:08:27 kmodi>

;; Smart Compile

(require 'cl)  ; for lexical-let
(require 'smart-compile)

;; Below code was given by user4815162342 from StackOverflow.
;; Source: http://stackoverflow.com/questions/15723871/single-shortcut-to-save-compile-execute-c-program-in-emacs
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
  (smart-compile 1))


(setq setup-smart-compile-loaded t)
(provide 'setup-smart-compile)
