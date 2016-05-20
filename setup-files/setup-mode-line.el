;; Time-stamp: <2016-05-19 22:35:02 kmodi>

;; Customize the mode-line

(defvar modi/show-date-time-in-mode-line nil
  "If non-nil, show the date-time in the mode-line.
If nil, show the same in the minibuffer.")

;; Date, time, load average, mail in mode-line
(if modi/show-date-time-in-mode-line
    (setq display-time-format "%l:%M %b %d %a" )
  (setq display-time-format ""))

;; Do NOT show average system load time
(setq display-time-default-load-average nil)

(display-time-mode 1)

;; Show line and column numbers in the mode-line
(line-number-mode 1)
(column-number-mode 1)

;; Display date+time in the minibuffer instead of in the mode-line
(use-package minibuffer-line
  :if (null modi/show-date-time-in-mode-line)
  :defer 1 ; to prevent 'Invalid face reference: minibuffer-line'
                                        ; when launching emacsclient
  :init
  (progn
    (defvar modi/minibuffer-line-right-aligned nil
      "If non-nil, right-align the minibuffer-line display.")

    (setq minibuffer-line-format
          '((:eval
             (let ((time-string (format-time-string "%l:%M %b %d %a")))
               ;; http://emacs.stackexchange.com/a/19856/115
               (if modi/minibuffer-line-right-aligned
                   (concat
                    (make-string (- (frame-text-cols) (string-width time-string))
                                 ? )
                    time-string)
                 time-string)))))

    (defvar modi/today--day-sym (intern (format-time-string "%a"))
      "Symbol containing 3-letter abbreviation of today's day.")

    (defvar modi/time-go-home-reset "12:01am"
      "Time when to modify the time face to remove the alert face.")

    (defvar modi/time-go-home-alert '((Mon . "05:15pm")
                                      (Tue . "05:15pm")
                                      (Wed . "05:15pm")
                                      (Thu . "04:20pm")
                                      (Fri . "04:20pm")
                                      (Sat . "")
                                      (Sun . ""))
      "Time when to modify the time face to alert it's time to go home."))
  :config
  (progn
    (defun modi/minibuffer-line-set-default-face ()
      (set-face-attribute 'minibuffer-line nil :inherit font-lock-type-face))
    (defun modi/minibuffer-line-set-alert-face ()
      (set-face-attribute 'minibuffer-line nil :inherit font-lock-warning-face))

    (defun modi/update-go-home-alert ()
      "Reset the `minibuffer-line' face to default and update the `Go Home'
alert time."
      (modi/minibuffer-line-set-default-face)
      (setq modi/today--day-sym (intern (format-time-string "%a")))
      (modi/set-go-home-alert (cdr (assq modi/today--day-sym
                                         modi/time-go-home-alert))))

    (defun modi/reset-go-home-alert (time)
      "Set the `go home' alert reset time interactively.
The `modi/timer--go-home-reset' timer is canceled and not restarted if
TIME is \"nil\" or \"\"."
      (interactive "sReset alert time (e.g. 7:00am): ")
      (setq modi/time-go-home-reset time)
      (when (and (boundp 'modi/timer--go-home-reset) ; cancel the timer if
                 (timerp  modi/timer--go-home-reset)) ; already running
        (cancel-timer modi/timer--go-home-reset))
      (when (not (or (string= time "")
                     (string= time "nil")))
        (let ((daily (* 60 60 24)))
          (setq modi/timer--go-home-reset (run-at-time
                                           time daily
                                           #'modi/update-go-home-alert)))))

    (defun modi/set-go-home-alert (time)
      "Set the `go home' alert time interactively.
The `modi/timer--go-home-alert' timer is canceled and not restarted if
TIME is \"nil\" or \"\"."
      (interactive "s'Go Home' alert time (e.g. 5:00pm): ")
      ;; http://emacs.stackexchange.com/a/3415/115
      (setcdr (assq modi/today--day-sym modi/time-go-home-alert) time)
      (let (old-go-home-time old-go-home-time-str)
        (when (and (boundp 'modi/timer--go-home-alert) ; cancel the timer if
                   (timerp  modi/timer--go-home-alert)) ; already running
          (setq old-go-home-time (list (elt modi/timer--go-home-alert 1)
                                       (elt modi/timer--go-home-alert 2)))
          (setq old-go-home-time-str (downcase (format-time-string
                                                "%I:%M%p" old-go-home-time)))
          (cancel-timer modi/timer--go-home-alert))
        (if (or (string= time "")
                (string= time "nil"))
            (progn
              (modi/minibuffer-line-set-default-face)
              (setq modi/timer--go-home-alert nil)
              (message "%s: `Go Home' alert removed." modi/today--day-sym))
          (let ((daily (* 60 60 24))
                new-go-home-time new-go-home-time-str)
            (setq modi/timer--go-home-alert (run-at-time
                                             time daily
                                             #'modi/minibuffer-line-set-alert-face))
            (setq new-go-home-time (list (elt modi/timer--go-home-alert 1)
                                         (elt modi/timer--go-home-alert 2)))
            (setq new-go-home-time-str (downcase (format-time-string
                                                  "%I:%M%p" new-go-home-time)))
            (if old-go-home-time
                (if (string= new-go-home-time-str old-go-home-time-str)
                    (message "`Go Home' time unchanged: %s"
                             new-go-home-time-str)
                  (message "%s: Changed `Go Home' time from %s to %s."
                           modi/today--day-sym
                           old-go-home-time-str new-go-home-time-str))
              (message "%s: `Go Home' alert time set to %s."
                       modi/today--day-sym new-go-home-time-str))))))

    ;; To set the `Go Home' alert, put the below in `setup-personal.el'
    ;; (with-eval-after-load 'setup-mode-line
    ;;   (with-eval-after-load 'minibuffer-line
    ;;     (modi/reset-go-home-alert modi/time-go-home-reset)))

    (modi/minibuffer-line-set-default-face)

    (minibuffer-line-mode)))

;; smart-mode-line
;; emacs modeline aka statusbar
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :init
  (progn
    (setq sml/line-number-format    "%4l")
    (setq sml/name-width            40) ; buffer name width in the mode-line
    (setq sml/mode-width            'full) ; minor mode lighters area width
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme (if (boundp 'dark-theme)
                        (if (not dark-theme) 'light 'dark)
                      'dark))
    (setq sml/replacer-regexp-list
          `(
            ("^~/org/"                          ":Org:")
            ("^~/\\.emacs\\.d/"                   ":ED:")
            ("^~/.*box/uvm/uvm_examples/"       ":UVM_EX:")
            ("^~/.*box/uvm/adsim_uvm_examples/" ":AD_UVM_EX:")
            (":\\(.*_EX\\):\\([a-z0-9_]\\{3\\}\\).*?/"
             (lambda (string) (concat ":\\1:"
                                      (match-string 2 string)
                                      ":")))
            ;; Prefix with first 2 letters and last letter of project name
            ;; To distinguish between projects that could have same first 3 letters
            ;; Using "\,(upcase ...)" only works when calling `replace-regexp` interactively.
            ;; In lisp code you have to give it a function. So we need to change the
            ;; replacement string to,
            ;; `(lambda (string) (concat ":" (upcase (match-string 1 string)) ":")))`.

            (,(concat "/proj.*?" ; project base
                      "/\\([a-z0-9_]\\{2\\}\\).*?\\([a-z0-9_]\\)" ; project name
                      "/[so]+_\\([a-z0-9]+\\)" ; user project root
                      "/\\([a-z0-9_]\\{0,3\\}\\).*?/") ; dir in user project root
             (lambda (string)
               ;; (concat ":"
               ;;         (capitalize (match-string 1 string))
               ;;         (upcase (match-string 2 string))
               ;;         (when (not (string= (match-string 3 string) (getenv "USER")))
               ;;           (concat "[" (match-string 3 string) "]"))
               ;;         ":" (upcase (match-string 4 string)) ":"
               ;;         )
               (let ((user       (match-string-no-properties 3 string))
                     (abbrev-dir (match-string-no-properties 4 string)))
                 (concat ":" ; The first char HAS to be `:'
                         (when (not (string= user (getenv "USER")))
                           (concat "~" user "/"))
                         (upcase abbrev-dir) ":"))))
            ("\\(:.*\\)DIG:tb/"                    "\\1TB:"  )
            ("\\(:.*\\)TB:agents/"                 "\\1AGT:" )
            ("\\(:.*\\)TB:patterns/"               "\\1PAT:" )
            ("\\(:.*\\)TB:tests/"                  "\\1TST:" )
            ("\\(:.*\\)TB:uvm.*src/"               "\\1UVM:" )
            ("\\(:.*\\)DIG:design_code/"           "\\1DSGN:")
            ("\\(:.*\\)DSGN:rtl/"                  "\\1RTL:" )
            ("\\(:.*\\)DSGN:analog_partition_rtl/" "\\1ANA:" ))))
  :config
  (progn
    (use-package rich-minority
      :config
      (progn
        (setq rm-blacklist
              '(" WK"        ; which-key
                " hc"        ; hardcore mode
                " AC"        ; auto-complete
                " vl"        ; global visual line mode enabled
                " Wrap"      ; shows up if visual-line-mode is enabled for that buffer
                " Omit"      ; omit mode in dired
                " yas"       ; yasnippet
                " drag"      ; drag-stuff-mode
                " VHl"       ; volatile highlights
                " ctagsU"    ; ctags update
                " Undo-Tree" ; undo tree
                " wr"        ; Wrap Region
                " SliNav"    ; elisp-slime-nav
                " Fly"       ; Flycheck
                " PgLn"      ; page-line-break
                " ElDoc"     ; eldoc
                " GG"        ; ggtags
                " hs"        ; hideshow
                " hs+"       ;
                " ez-esc"    ; easy-escape
                " ivy"       ; ivy
                " h"         ; hungry-delete-mode
                ))
        (setq rm-text-properties '(("\\` Ovwrt\\'" 'face 'font-lock-warning-face))) ; default
        (add-to-list 'rm-text-properties '("\\` Abbrev\\'" 'display "​@")) ; Abbrev
        (add-to-list 'rm-text-properties '("\\` Ind\\'"    'display "​*>")) ; org indent
        (add-to-list 'rm-text-properties '("\\` Outl\\'"   'display "​ø")) ; outline
        (add-to-list 'rm-text-properties '("\\` Server\\'" 'display "​Σ")) ; Server
        (add-to-list 'rm-text-properties '("\\` μ\\'"      'display "​μ")) ; modi-mode
        (add-to-list 'rm-text-properties '("\\` Wg\\'"     'display "​w")) ; writegood
        (add-to-list 'rm-text-properties '("\\` =>\\'"     'display "​a")) ; aggressive indent
        (add-to-list 'rm-text-properties '("\\` Vis\\'"    'display "​V")) ; visible-mode
        (with-eval-after-load 'setup-font-check
          (if font-symbola-p
              (progn
                (add-to-list 'rm-text-properties '("\\` Tail\\'" 'display "​🢛")) ; auto revert tail
                (add-to-list 'rm-text-properties '("\\` Temp\\'" 'display "​𝘵")) ; temp
                (add-to-list 'rm-text-properties '("\\` rk\\'"   'display "​▯")) ; region bindings
                (add-to-list 'rm-text-properties '("\\` (\\*)\\'" 'display "​💡")) ; beacon
                (add-to-list 'rm-text-properties '("\\` Hi\\'"   'display "​🞵"))) ; Hi-Lock
            (progn
              (add-to-list 'rm-text-properties '("\\` Tail\\'" 'display "​Tail|"))
              (add-to-list 'rm-text-properties '("\\` Temp\\'" 'display "​t"))
              (add-to-list 'rm-text-properties '("\\` rk\\'"   'display "​r"))
              (add-to-list 'rm-text-properties '("\\` (\\*)\\'" 'display "​*"))
              (add-to-list 'rm-text-properties '("\\` Hi\\'"   'display "​H")))))))

    (sml/setup)))


(provide 'setup-mode-line)


;; Variables used in display-time-format
;; Source: http://docs.splunk.com/Documentation/Splunk/5.0.2/SearchReference/Commontimeformatvariables
;; %y = year in numbers (2-digit)
;; %Y = year in numbers (4-digit)
;; %m = month in number (eg: 12)
;; %B = full month name (eg: December)
;; %b = short month name (eg: Dec)
;; %d = day in numbers, with leading zeros (eg: 08)
;; %e = day in numbers, no leading zeros (eg: 8)
;; %A = full weekday name (eg: Sunday)
;; %a = short weekday name (eg: Sun)
;; %H = hours in 24-clock, with leading zeros
;; %k = hours in 24-clock, no leading zeros
;; %l = hours in 12-clock, with leading zeros
;; %p = am/pm
;; %T = time in 24-hour notation (%H:%M:%S)
