;; Time-stamp: <2017-04-15 01:10:39 kmodi>

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
    (defun modi/do--execute (bin dir)
      "Execute BIN in eshell in DIR directory."
      (let ((default-directory dir))
        (eshell) ; Start eshell or switch to an existing eshell session
        (goto-char (point-max))
        (insert bin)
        (eshell-send-input)
        (sit-for 1) ;Let's assume the binary finishes executing in this time.
        ;; After that time, if the point is after the eshell prompt (i.e. user
        ;; input not expected), switch to the other window (which should be the
        ;; original code window).
        (save-excursion
          (forward-line 0)
          ;; This moves the point to the beginning of the line even if that
          ;; happens to be over the eshell prompt.
          (when (looking-at-p eshell-prompt-regexp)
            (other-window 1)))))

    (defun modi/save-compile-execute ()
      "Save, compile and execute."
      (interactive)
      (save-buffer)
      (lexical-let ((bin (smart-compile-string "./%n"))
                    ;; %n - file name without extension
                    ;; See `smart-compile-alist'.
                    finish-callback)
        (setq finish-callback
              (lambda (buf msg)
                (with-selected-window (get-buffer-window "*compilation*")
                  (bury-buffer))
                ;; Bring up the buried compilation buffer in the other window
                ;; if the compilation failed. Else execute the binary.
                ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2012-02/msg00133.html
                (if (string= "finished\n" msg)
                    (progn
                      ;; Execute the binary
                      ;; Start eshell in a different window. But save the
                      ;; `default-directory' for eshell before doing that.
                      (let ((dir default-directory))
                        (other-window 1)
                        (modi/do--execute bin dir)))
                  (switch-to-buffer-other-window "*compilation*")
                  (other-window 1)) ;And then switch back to the other window (code)
                ;; When compilation is done, execute the program and remove the
                ;; callback from `compilation-finish-functions'
                (setq compilation-finish-functions
                      (delq finish-callback compilation-finish-functions))))
        (push finish-callback compilation-finish-functions))
      (smart-compile 1))))


(provide 'setup-compile)
