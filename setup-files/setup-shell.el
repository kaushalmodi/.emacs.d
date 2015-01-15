;; Time-stamp: <2015-01-15 13:33:25 kmodi>

;; Shell Script Mode

(defun my/tcsh-set-indent-functions ()
  (when (or (string-match ".*\\.alias" (buffer-file-name))
            (string-match ".*\\.setup.*" (buffer-file-name))
            (string-match ".*\\.gpms" (buffer-file-name))
            (string-match ".*\\.cfg" (buffer-file-name))
            (string-match ".*csh$" (file-name-extension (buffer-file-name))))
    (require 'csh-mode) ; https://github.com/Tux/tcsh/blob/master/csh-mode.el
    (setq-local indent-line-function   'csh-indent-line)
    (setq-local indent-region-function 'csh-indent-region)))
(add-hook 'sh-set-shell-hook #'my/tcsh-set-indent-functions)


(provide 'setup-shell)
