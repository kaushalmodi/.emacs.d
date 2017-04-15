;; Time-stamp: <2017-04-15 00:03:36 kmodi>

;;; Compile

(use-package compile
  :defer t
  :config
  (progn
    ;; http://stackoverflow.com/a/13408008/1219634
    (require 'ansi-color)
    (defun modi/colorize-compilation-buffer ()
      (ansi-color-apply-on-region compilation-filter-start (point)))
    (add-hook 'compilation-filter-hook #'modi/colorize-compilation-buffer)))

;;; Smart Compile
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
    (defun modi/do--execute (bin)
      "Execute BIN in eshell."
      (eshell) ; Start eshell or switch to an existing eshell session
      (goto-char (point-max))
      (insert bin)
      (eshell-send-input))

    (defun modi/save-compile-execute ()
      "Save, compile and execute"
      (interactive)
      (save-buffer)
      (lexical-let ((code-buf (buffer-name))
                    (bin (smart-compile-string "./%n"))
                    ;; %n - file name without extension
                    ;; See `smart-compile-alist'.
                    finish-callback)
        (setq finish-callback
              (lambda (buf msg)
                ;; Bury the compilation buffer
                (with-selected-window (get-buffer-window "*compilation*")
                  (bury-buffer))
                ;; Execute the binary
                (modi/do--execute bin)
                ;; When compilation is done, execute the program and remove the
                ;; callback from `compilation-finish-functions'
                (setq compilation-finish-functions
                      (delq finish-callback compilation-finish-functions))))
        (push finish-callback compilation-finish-functions))
      (smart-compile 1))))


(provide 'setup-compile)
