;;; go-home-alert.el --- Minibuffer alert

;; Author: Kaushal Modi <kaushal.modi@gmail.com>
;; Version: 0.1
;; Keywords: minibuffer, alert
;; Package-Requires: ((minibuffer-line "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package alerts user on specific times set for specific days by changing
;; the face of the `minibuffer-line' face.

;; Put the below in your config to use to package (using `use-package'):
;;
;;   (use-package go-home-alert
;;     :defer 1 ; to prevent 'Invalid face reference: minibuffer-line'
;;              ; when launching emacsclient
;;     :config
;;     (go-home-alert-start))

;;; Code:

(require 'minibuffer-line)

(defvar go-home-alert--today (intern (format-time-string "%a"))
  "Symbol containing 3-letter abbreviation of today's day.")

(defvar go-home-alert-reset-time "12:01am"
  "Time when to modify the time face to remove the alert face.")

(defvar go-home-alert-update-time '((Mon . "05:15pm")
                                    (Tue . "05:15pm")
                                    (Wed . "05:15pm")
                                    (Thu . "04:20pm")
                                    (Fri . "04:20pm")
                                    (Sat . "")
                                    (Sun . ""))
  "Time when to modify the time face to alert it's time to go home.")

(defvar go-home-alert--reset-timer)
(defvar go-home-alert--alert-timer)

(defun go-home-alert--set-default-face ()
  "Set the `minibuffer-line' face for non-alert state."
  (set-face-attribute 'minibuffer-line nil :inherit font-lock-type-face))

(defun go-home-alert--set-alert-face ()
  "Set the `minibuffer-line' face for alert state."
  (set-face-attribute 'minibuffer-line nil :inherit font-lock-warning-face))

(defun go-home-alert--update ()
  "Reset the `minibuffer-line' face to default and set next alert time."
  (go-home-alert--set-default-face)
  (setq go-home-alert--today (intern (format-time-string "%a")))
  (go-home-alert-set (cdr (assq go-home-alert--today
                                go-home-alert-update-time))))

(defun go-home-alert-reset (time)
  "Set the `go home' alert reset time interactively.
The `go-home-alert--reset-timer' timer is canceled and not restarted if
TIME is \"nil\" or \"\"."
  (interactive "sReset alert time (e.g. 7:00am): ")
  (setq go-home-alert-reset-time time)
  (when (and (boundp 'go-home-alert--reset-timer) ; cancel the timer if
             (timerp  go-home-alert--reset-timer)) ; already running
    (cancel-timer go-home-alert--reset-timer))
  (when (not (or (string= time "")
                 (string= time "nil")))
    (let ((daily (* 60 60 24)))
      (setq go-home-alert--reset-timer (run-at-time
                                        time daily
                                        #'go-home-alert--update)))))

;;;###autoload
(defun go-home-alert-set (time)
  "Set the `go home' alert time interactively.
The `go-home-alert--alert-timer' timer is canceled and not restarted if
TIME is \"nil\" or \"\"."
  (interactive "s'Go Home' alert time (e.g. 5:00pm): ")
  ;; http://emacs.stackexchange.com/a/3415/115
  (setcdr (assq go-home-alert--today go-home-alert-update-time) time)
  (let (old-go-home-time old-go-home-time-str)
    (when (and (boundp 'go-home-alert--alert-timer) ; cancel the timer if
               (timerp  go-home-alert--alert-timer)) ; already running
      (setq old-go-home-time (list (elt go-home-alert--alert-timer 1)
                                   (elt go-home-alert--alert-timer 2)))
      (setq old-go-home-time-str (downcase (format-time-string
                                            "%I:%M%p" old-go-home-time)))
      (cancel-timer go-home-alert--alert-timer))
    (if (or (string= time "")
            (string= time "nil"))
        (progn
          (go-home-alert--set-default-face)
          (setq go-home-alert--alert-timer nil)
          (message "%s: `Go Home' alert removed." go-home-alert--today))
      (let ((daily (* 60 60 24))
            new-go-home-time new-go-home-time-str)
        (setq go-home-alert--alert-timer (run-at-time
                                          time daily
                                          #'go-home-alert--set-alert-face))
        (setq new-go-home-time (list (elt go-home-alert--alert-timer 1)
                                     (elt go-home-alert--alert-timer 2)))
        (setq new-go-home-time-str (downcase (format-time-string
                                              "%I:%M%p" new-go-home-time)))
        (if old-go-home-time
            (if (string= new-go-home-time-str old-go-home-time-str)
                (message "`Go Home' time unchanged: %s"
                         new-go-home-time-str)
              (message "%s: Changed `Go Home' time from %s to %s."
                       go-home-alert--today
                       old-go-home-time-str new-go-home-time-str))
          (message "%s: `Go Home' alert time set to %s."
                   go-home-alert--today new-go-home-time-str))))))

;;;###autoload
(defun go-home-alert-start ()
  "Start the alert timers."
  (interactive)
  (go-home-alert-reset go-home-alert-reset-time))

(provide 'go-home-alert)

;;; go-home-alert.el ends here
