;; Time-stamp: <2015-08-26 09:54:01 kmodi>

;; Pomodoro
;; https://github.com/baudtack/pomodoro.el

(use-package pomodoro
  :config
  (progn
    (setq pomodoro-play-sounds              nil)

    (setq pomodoro-work-start-message       "Back to work!")
    (setq pomodoro-work-cycle               "w")
    (setq pomodoro-work-time                25)

    (setq pomodoro-break-start-message      "Break time!")
    (setq pomodoro-break-cycle              "b")
    (setq pomodoro-break-time               5)

    (setq pomodoro-long-break-start-message "Time for a longer break!")
    (setq pomodoro-long-break-time          15)
    (setq pomodoro-nth-for-longer-break     4)

    (setq pomodoro-extra-time               2)

    (add-to-list 'mode-line-format
                 '(pomodoro-mode-line-string pomodoro-mode-line-string))

    (defhydra hydra-pomodoro (:color blue)
      "pomodoro"
      ("0" pomodoro-start  "start")
      ("s" pomodoro-start  "start")
      ("x" pomodoro-stop   "stop")
      ("p" pomodoro-pause  "pause")
      ("r" pomodoro-resume "resume")
      ("q" nil "cancel"))
    (bind-key "C-c 0" #'hydra-pomodoro/body modi-mode-map)))


(provide 'setup-pomodoro)
