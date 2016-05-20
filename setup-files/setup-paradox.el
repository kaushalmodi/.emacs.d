;; Time-stamp: <2016-05-19 21:38:52 kmodi>

;; Paradox
;; https://github.com/Malabarba/paradox

(use-package paradox
  :defer t
  :config
  (progn
    ;; The "paradox-token" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)

    (setq paradox-lines-per-entry 1)
    (setq paradox-automatically-star t)

    (paradox-enable)))


(provide 'setup-paradox)
