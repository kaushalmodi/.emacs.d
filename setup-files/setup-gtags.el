;; Time-stamp: <2015-02-17 16:13:20 kmodi>

;; gtags, GNU global

(req-package ggtags
  :require (verilog-mode key-chord)
  :config
  (progn

    (defun my/ggtags-project-name ()
      "Return gtags project name."
      (if (stringp ggtags-project-root)
          (let ((project-root ggtags-project-root)
                project-name)
            (setq project-name (file-name-nondirectory
                                (directory-file-name project-root)))
            (while (string-match "\\(sos_\\|\\bsrc\\b\\)" project-name)
              (setq project-root (replace-regexp-in-string
                                  "\\(.*\\)/.+/*$" "\\1" project-root))
              (setq project-name (file-name-nondirectory
                                  (directory-file-name project-root))))
            project-name)
        nil))

    ;; (setq ggtags-navigation-mode-lighter nil)
    ;; (setq ggtags-mode-line-project-name nil)
    (setq ggtags-mode-line-project-name
          '("[" (:eval (let ((name (if (stringp (my/ggtags-project-name))
                                       (my/ggtags-project-name)
                                     "?")))
                         (propertize
                          name 'face compilation-info-face
                          'help-echo (if (stringp ggtags-project-root)
                                         (concat "mouse-1 to visit " ggtags-project-root)
                                       "mouse-1 to set project")
                          'mouse-face 'mode-line-highlight
                          'keymap ggtags-mode-line-project-keymap)))
            "]"))
    (setq ggtags-oversize-limit (* 30 1024 1024))

    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook
                    sh-mode-hook
                    cperl-mode-hook
                    c-mode-hook))
      (add-hook hook #'ggtags-mode))

    ;; Don't consider ` (back quote) as part of `tag' when looking for a
    ;; Verilog macro definition
    (defun ggtags-tag-at-point ()
      (pcase (funcall ggtags-bounds-of-tag-function)
        (`(,beg . ,end)
         (if (eq ?` (string-to-char (buffer-substring beg end)))
             ;; If `(buffer-substring beg end)' returns "`uvm_info" (for example),
             ;; discard the ` and return just "uvm_info"
             (buffer-substring (1+ beg) end)
           ;; else return the whole `(buffer-substring beg end)'
           (buffer-substring beg end)))))

    (key-chord-define-global "??" 'ggtags-show-definition)))


(provide 'setup-gtags)
