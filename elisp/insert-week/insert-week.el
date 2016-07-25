;; Time-stamp: <2016-07-09 01:48:47 kmodi>

;; Get the dates corresponding to Monday and Friday of the current week.
;; If current date is on Sat/Sun, get those dates for the past week.

;; Constants associated to the %u identifier of `format-time-string'
;; %u is the numeric day of week from 1 (Monday) to 7
(defconst iw-MON 1)
(defconst iw-TUE 2)
(defconst iw-WED 3)
(defconst iw-THU 4)
(defconst iw-FRI 5)
(defconst iw-SAT 6)
(defconst iw-SUN 7)
(defconst iw-day-number-alist `((,iw-MON . "Monday")
                                (,iw-TUE . "Tuesday")
                                (,iw-WED . "Wednesday")
                                (,iw-THU . "Thursday")
                                (,iw-FRI . "Friday")
                                (,iw-SAT . "Saturday")
                                (,iw-SUN . "Sunday")))

(defvar insert-week-date-format "%m/%d"
  "Time format for the inserted dates. Refer to `format-time-string' function
documentation for more information on this time string format.")

(defvar insert-week-date-separator "-"
  "Separator string to be used between dates.")

(defun insert-week--human-time-to-actual-date (human-time time-format)
  "Return a time string formatted using TIME-FORMAT for a time specified by
HUMAN-TIME."
  ;; http://emacs.stackexchange.com/a/575/115
  (let* ((decoded-time (parse-time-string
                        (with-temp-buffer
                          (call-process "env" nil t nil
                                        "LC_ALL=C" "LANGUAGE=" "date" "-d" human-time)
                          (or (bobp) (delete-backward-char 1)) ; delete newline character
                          (buffer-string))))
         (encoded-time (apply #'encode-time decoded-time)))
    (format-time-string time-format encoded-time)))

(defun insert-week--get-date-for (xday next-week)
  "Return the date for XDAY of this week.
If NEXT-WEEK is non-nil, return the date for XDAY for the next week."
  (let* ((today (string-to-number (format-time-string "%u")))
         (days-till (- xday today))
         (day-str (cdr (assoc xday iw-day-number-alist)))
         (next-week-str "")
         human-time)
    (when (consp next-week)
      (let ((num-next-weeks (log (car next-week) 4)))
        (dotimes (i num-next-weeks)
          (setq next-week-str (concat next-week-str
                                      " next week")))))

    (setq human-time (cond
                      ((eq days-till 0) (concat "today" next-week-str))
                      ((> days-till 0) (concat "next " day-str next-week-str))
                      (t (concat "last " day-str next-week-str))))
    (insert-week--human-time-to-actual-date human-time insert-week-date-format)))

(defun insert-week (next-week)
  "Return the date range of this week.
If today is Saturday or Sunday, get the date range for the past week.

If universal prefix argument is used, return the date range for upcoming weeks.

        C-u -> Returns date range for next week.
    C-u C-u -> Returns date range for next to next week.
C-u C-u C-u -> Returns date range for next to next to next week.
... and so on. "
  (interactive "P")
  (insert (concat (insert-week--get-date-for iw-MON next-week)
                  insert-week-date-separator
                  (insert-week--get-date-for iw-FRI next-week))))


(provide 'insert-week)
