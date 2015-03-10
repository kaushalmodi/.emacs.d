;; Time-stamp: <2015-03-10 17:20:00 kmodi>

;; Paradox
;; https://github.com/Bruce-Connor/paradox

(use-package paradox
    ;; :load-path "elisp/paradox"
    :config
  (progn
    ;; The ".paradox-token.el" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (expand-file-name ".paradox-token.el" user-emacs-directory)
          :noerror :nomessage)
    (setq paradox-lines-per-entry 1)

    (paradox-enable)))


(provide 'setup-paradox)
