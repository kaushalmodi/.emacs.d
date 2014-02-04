;; Time-stamp: <2014-02-03 12:00:44 kmodi>

;; Source: http://stackoverflow.com/questions/12558019/shortcut-to-open-a-specific-file-in-emacs
;; Source: http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Registers.html#File-Registers
;; Save the frequently accessed file locations in registers for quick access

;; .alias can be accessed using `C-x r j a`
(set-register ?a (cons 'file "~/.alias" ))

;; init.el can be accessed using `C-x r j e`
(set-register ?e (cons 'file (concat user-emacs-directory
                                     "/init.el")))

;; index.html can be accessed using `C-x r j i`
(set-register ?i (cons 'file "~/public_html/index.html" ))

;; index.html can be accessed using `C-x r j j`
(set-register ?j (cons 'file "~/org/journal.org" ))

;; setup-key-bindings.el can be accessed using `C-x r j k`
(set-register ?k (cons 'file (concat user-emacs-directory
                                     "/setup-files/setup-key-bindings.el")))

;; .alias can be accessed using `C-x r j t`
(set-register ?t (cons 'file "~/.tmux.conf" ))

(setq setup-registers-loaded t)
(provide 'setup-registers)
