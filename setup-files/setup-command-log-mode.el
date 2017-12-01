;; Time-stamp: <2017-12-01 12:34:04 kmodi>

;; Command Log Mode
;; https://github.com/lewang/command-log-mode
;; Maintained fork of http://www.foldr.org/~michaelw/emacs/mwe-log-commands.el

(use-package command-log-mode
  :commands (hydra-command-log/body)
  :init
  (progn
    (setq clm/logging-dir (let ((dir (concat user-emacs-directory
                                             "command-log")))
                            (make-directory dir :parents)
                            dir))
    ;; Do not bind `clm/open-command-log-buffer' by default to "C-c o"
    (setq command-log-mode-key-binding-open-log nil)
    (bind-to-modi-map "c" #'hydra-command-log/body))
  :config
  (progn
    (setq command-log-mode-window-size 60)

    (defhydra hydra-command-log (:color teal
                                 :columns 6)
      "Command Log"
      ("c" command-log-mode "toggle mode")
      ("o" clm/open-command-log-buffer "open log buffer")
      ("l" clm/open-command-log-buffer "open log buffer")
      ("C" clm/command-log-clear "clear log buffer")
      ("t" clm/toggle-command-log-buffer "toggle log buffer")
      ("s" clm/save-command-log "save log")
      ("x" clm/close-command-log-buffer "close log buffer")
      ("q" nil "cancel" :color blue))))


(provide 'setup-command-log-mode)
