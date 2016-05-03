;; Time-stamp: <2016-05-03 11:57:11 kmodi>

;; Pomodoro
;; https://github.com/baudtack/pomodoro.el

(use-package pomodoro
  :bind (:map modi-mode-map
         ("C-c C-`" . hydra-pomodoro/body))
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
      ("q" nil "cancel"))))


(provide 'setup-pomodoro)
