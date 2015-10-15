;; Time-stamp: <2015-10-15 12:58:33 kmodi>

;; Paradox
;; https://github.com/Malabarba/paradox

(use-package paradox
  :commands (package-list-packages
             paradox-list-packages
             hydra-launch/paradox-list-packages-and-exit)
  :config
  (progn
    ;; The "paradox-token" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)

    (setq paradox-lines-per-entry 1)
    (setq paradox-automatically-star t)

    (paradox-enable)))


(provide 'setup-paradox)
