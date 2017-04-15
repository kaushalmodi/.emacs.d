;; Time-stamp: <2017-04-14 23:45:33 kmodi>

;; Smart Compile
;; https://www.emacswiki.org/emacs/SmartCompile
;; https://github.com/zenitani/elisp/blob/master/smart-compile.el

(use-package smart-compile
  :commands (modi/save-compile-execute)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     :filter (not (or (derived-mode-p 'emacs-lisp-mode)
                      (derived-mode-p 'verilog-mode)))
      ("<f9>" . modi/save-compile-execute)))
  :config
  (progn
    ;; Always use C99 standard for compilation
    (setcdr (assoc "\\.c\\'" smart-compile-alist) "gcc -O2 %f -lm -o %n -std=gnu99")

    ;; http://stackoverflow.com/a/15724162/1219634
    (defun modi/do-execute (exe)
      "Run EXE in eshell."
      (eshell) ; Start eshell or switch to an existing eshell session
      (goto-char (point-max))
      (insert exe)
      (eshell-send-input))

    (defun modi/save-compile-execute ()
      "Save, compile and execute"
      (interactive)
      (lexical-let ((code-buf (buffer-name))
                    (exe (smart-compile-string "./%n"))
                    finish-callback)
        (setq finish-callback
              (lambda (buf msg)
                ;; Bury the compilation buffer
                (with-selected-window (get-buffer-window "*compilation*")
                  (bury-buffer))
                (modi/do-execute exe)
                ;; When compilation is done, execute the program and remove the
                ;; callback from `compilation-finish-functions'
                (setq compilation-finish-functions
                      (delq finish-callback compilation-finish-functions))))
        (push finish-callback compilation-finish-functions))
      (smart-compile 1))))


(provide 'setup-smart-compile)
