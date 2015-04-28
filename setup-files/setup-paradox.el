;; Time-stamp: <2015-04-28 08:59:14 kmodi>

;; Paradox
;; https://github.com/Malabarba/paradox

(use-package paradox
  ;; :load-path "elisp/paradox"
  :config
  (progn
    ;; The ".paradox-token.el" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (expand-file-name ".paradox-token.el" user-emacs-directory)
          :noerror :nomessage)
    (setq paradox-lines-per-entry 1)
    (setq paradox-automatically-star t)

    (paradox-enable)))


(provide 'setup-paradox)
