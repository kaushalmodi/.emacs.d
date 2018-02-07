;; Time-stamp: <2018-02-07 17:07:13 kmodi>

(defun modi/seconds-to-human-time (&optional seconds)
  "Convert SECONDS to \"DDd HHh MMm SSs\" string.

SECONDS is a non-negative integer or fractional number.

SECONDS can also be a list of such numbers, which is the case
when this function is called recursively.

When called interactively, if a region is selected SECONDS is
extracted from that, else the user is prompted to enter those."
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
                 ((listp seconds)         ;This is entered only by recursive calls
                  (car (last seconds)))
                 ((and (numberp seconds)  ;This is entered only in the first entry
                       (>= seconds 0))
                  seconds)
                 (t
                  (user-error "Invalid argument %S" seconds))))
           (gen-time-string
            (lambda (time inter)
              "Return string representation of TIME.
TIME is of the type (DD HH MM SS), where each of those elements
are numbers.  If INTER is non-nil, echo the time string in a
well-formatted manner instead of returning it."
              (let* ((rev-time (reverse time))
                     (sec (nth 0 rev-time))
                     (min (nth 1 rev-time))
                     (hr (nth 2 rev-time))
                     (day (nth 3 rev-time))
                     (filler "    ")
                     (sec-str (cond
                               ((> sec 0)
                                (if (integerp sec)
                                    (format "%2ds" sec)
                                  (format "%5.2fs" sec)))
                               ((and (= sec 0) (null min) (null hr) (null day)) ;0 seconds
                                " 0s")))
                     (min-str (if (and min (> min 0))
                                  (format "%2dm " min)
                                filler))
                     (hr-str (if (and hr (> hr 0))
                                 (format "%2dh " hr)
                               filler))
                     (day-str (if (and day (> day 0))
                                  (format "%2dd " day)
                                filler))
                     (str (string-trim-right
                           (concat day-str hr-str min-str sec-str))))
                (if inter
                    (message "%0.2f seconds → %s"
                             seconds
                             (string-trim (replace-regexp-in-string " +"  " " str)))
                  str))))
           (time (cond
                  ((>= sec DAY)          ;> day
                   (let* ((days (/ (floor sec) DAY))
                          (rem (- sec (* days DAY))))
                     (cond
                      ((= rem 0)
                       (list days 0 0 0))
                      ((< rem MINUTE)
                       ;; Note that (list rem) instead of just `rem' is being
                       ;; passed to the recursive call to
                       ;; `modi/seconds-to-human-time'.  This helps us
                       ;; distinguish between direct and re-entrant calls to
                       ;; this function.
                       (append (list days 0 0) (modi/seconds-to-human-time (list rem))))
                      ((< rem HOUR)
                       (append (list days 0) (modi/seconds-to-human-time (list rem))))
                      (t
                       (append (list days) (modi/seconds-to-human-time (list rem)))))))
                  ((>= sec HOUR)         ;> hour AND < day
                   (let* ((hours (/ (floor sec) HOUR))
                          (rem (- sec (* hours HOUR))))
                     (cond
                      ((= rem 0)
                       (list hours 0 0))
                      ((< rem MINUTE)
                       (append (list hours 0) (modi/seconds-to-human-time (list rem))))
                      (t
                       (append (list hours) (modi/seconds-to-human-time (list rem)))))))
                  ((>= sec MINUTE)       ;> minute AND < hour
                   (let* ((mins (/ (floor sec) MINUTE))
                          (rem (- sec (* mins MINUTE))))
                     (cond
                      ((= rem 0)
                       (list mins 0))
                      (t
                       (append (list mins) (modi/seconds-to-human-time (list rem)))))))
                  (t                    ;< minute
                   (list sec)))))
      ;; If `seconds' is a number and not a list, this is *not* a recursive
      ;; call.  Return the time as a string only then.  For re-entrant
      ;; executions, return the `time' list instead.
      (if (numberp seconds)
          (funcall gen-time-string time inter)
        time))))

;; Tests
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
    (message "%9.2f seconds → %s" sec (modi/seconds-to-human-time sec))
    (cl-incf count)
    (when (= 0 (mod count len-secs))
      (message (make-string 40 ?─)))))
