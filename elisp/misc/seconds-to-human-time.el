;;; seconds-to-human-time.el --- Convert seconds to human time -*- lexical-binding: t -*-
;; Time-stamp: <2018-02-20 14:10:01 kmodi>
;; https://scripter.co/convert-seconds-to-human-time/

(defun seconds-to-human-time (&optional seconds)
  "Convert SECONDS to \"DDd HHh MMm SSs\" string.

SECONDS is a non-negative integer or fractional number.

SECONDS can also be a list of such numbers, which is the case
when this function is called recursively.

When called interactively, if a region is selected SECONDS is
extracted from that, else the user is prompted to enter those.

With \\[universal-argument] prefix, copy the returned string to
the kill ring.

With \\[universal-argument] \\[universal-argument] prefix, replace
the selected region with the return value."
  (interactive)
  (let ((inter (called-interactively-p 'interactive)))
    (when inter
      (let ((seconds-str (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (read-string "Enter seconds: "))))
        (setq seconds (string-to-number seconds-str)))) ;"1" -> 1, "1.2" -> 1.2, "" -> 0
    (let* ((MINUTE 60)
           (HOUR (* 60 MINUTE))
           (DAY (* 24 HOUR))
           (sec (cond
                 ((and seconds
                       (listp seconds)) ;This is entered only by recursive calls
                  (car (last seconds)))
                 ((and (numberp seconds) ;This is entered only in the first entry
                       (>= seconds 0))
                  seconds)
                 (t
                  (user-error "Invalid argument %S" seconds))))
           (gen-time-string
            (lambda (time inter)
              "Return string representation of TIME.
TIME is of the type (DD HH MM SS), where each of those elements
are numbers.  If INTER is non-nil, also echo the time string in a
well-formatted manner."
              (let ((filler "    ")
                    (str ""))
                (dolist (unit '("d" "h" "m" "s"))
                  (let* ((val (car (rassoc unit time)))
                         (val-str (cond
                                   ((and (string= unit "s") ;0 seconds
                                         (= val 0)
                                         (string-match-p "\\`\\s-*\\'" str))
                                    " 0s")
                                   ((and (string= unit "s")
                                         (> val 0))
                                    (if (integerp val)
                                        (format "%2d%s" val unit)
                                      (format "%5.2f%s" val unit)))
                                   ((and val (> val 0))
                                    (format "%2d%s " val unit))
                                   (t
                                    filler))))
                    (setq str (concat str val-str))))
                ;; (message "debug: %S" time)
                (if inter
                    (progn
                      (setq str (string-trim
                                 (replace-regexp-in-string " +"  " " str)))
                      (message "%0.2f seconds → %s" seconds str))
                  (setq str (string-trim-right str)))
                str)))
           (time (cond
                  ((>= sec DAY)          ;> day
                   (let* ((days (/ (floor sec) DAY))
                          (rem (- sec (* days DAY))))
                     ;; Note that (list rem) instead of just `rem' is
                     ;; being passed to the recursive call to
                     ;; `seconds-to-human-time'.  This helps us
                     ;; distinguish between direct and re-entrant
                     ;; calls to this function.
                     (append (list (cons days "d")) (seconds-to-human-time (list rem)))))
                  ((>= sec HOUR)         ;> hour AND < day
                   (let* ((hours (/ (floor sec) HOUR))
                          (rem (- sec (* hours HOUR))))
                     (append (list (cons hours "h")) (seconds-to-human-time (list rem)))))
                  ((>= sec MINUTE)       ;> minute AND < hour
                   (let* ((mins (/ (floor sec) MINUTE))
                          (rem (- sec (* mins MINUTE))))
                     (append (list (cons mins "m")) (seconds-to-human-time (list rem)))))
                  (t                    ;< minute
                   (list (cons sec "s"))))))
      ;; If `seconds' is a number and not a list, this is *not* a
      ;; recursive call.  Return the time as a string only then.  For
      ;; re-entrant executions, return the `time' list instead.
      (if (numberp seconds)
          (let ((ret-val (funcall gen-time-string time inter))
                (arg current-prefix-arg))
            (cond
             ((equal arg '(4))
              (kill-new ret-val))
             ((and (use-region-p)
                   (equal arg '(16)))
              (delete-active-region)
              (insert ret-val))))
        time))))

;; Tests
(defun seconds-to-human-time--test ()
  "Test generator for `seconds-to-human-time'."
  (let* ((rand-bool (lambda()
                      "(random 2) will return either 1 or 0, so
                    frac will be either t or nil"
                      (= 1 (random 2))))
         (count 0)
         (secs '(0 1 60 61
                   3600 3601 3660 3661
                   86400 86401 86460 86461
                   90000 90001 90060 90061))
         (len-secs (length secs))
         (secs-rand1 (mapcar (lambda (s)
                               (let ((add-sec (funcall rand-bool))
                                     (add-min (funcall rand-bool))
                                     (add-hr (funcall rand-bool))
                                     (add-day (funcall rand-bool)))
                                 (when add-sec
                                   (setq s (+ s 1)))
                                 (when add-min
                                   (setq s (+ s 60)))
                                 (when add-hr
                                   (setq s (+ s (* 60 60))))
                                 (when add-day
                                   (setq s (+ s (* 60 60 24))))
                                 s))
                             secs))
         secs-rand2)
    (dotimes (_ (* 2 len-secs))
      (let* ((frac (funcall rand-bool))
             (sec (if frac
                      (/ (random 100000000) 100.00)
                    (random 1000000))))
        (push sec secs-rand2)))
    (dolist (sec (append secs secs-rand1 secs-rand2))
      (message "%9.2f seconds → %s" sec (seconds-to-human-time sec))
      (cl-incf count)
      (when (= 0 (mod count len-secs))
        (message (make-string 40 ?─))))))


(provide 'seconds-to-human-time)

;; (format-seconds "%2D %2H %2M %Z%S" sec) does something similar, but
;; not quite.
