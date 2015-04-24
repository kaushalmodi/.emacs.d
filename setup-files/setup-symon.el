;; Time-stamp: <2015-04-24 14:22:54 kmodi>

;; Mode-line system monitor
;; https://github.com/zk-phi/symon

(use-package symon
  :config
  (progn
    (setq symon-refresh-rate 4)
    (setq symon-delay 5)
    (setq symon-sparkline-type 'symon-sparkline-type-plain)

    (defmacro my/define-symon-monitor (name &rest plist)
      "define a new symon monitor NAME. following keywords are
supported in PLIST:

:setup (default: nil)

    an expression evaluated when activating symon-mode, and
    expected to do some preparation.

:cleanup (default: nil)

    an expression evaluated when deactivating symon-mode, and
    expected to do some cleanup.

:disp-string (default: nil)

   an expression that evaluates to a string. If non-nil, display
   this string instead of the default numeric value+unit+sparkline(optional)

:fetch (default: nil)

    an expression that evaluates to the latest status value. the
    value must be a number (otherwise `N/A' is displayed as the
    value).

:interval (default: symon-refresh-rate)

    fetch interval in seconds.

:index (default: \"\")

    string prepended to the status value (\"MEM:\" for memory
    monitor, for example).

:unit (default: \"\")

    string appended to the status value (\"%\" for memory
    monitor, for example).

:annotation (default: nil)

    an expression that evaluates to the annotation string for the
    metrics (\"xxxKB Swapped\" for memory monitor, for
    example). if this expression returns a non-nil value, it is
    surrounded with parentheses and appended to the status value.

:display (default: nil)

    an expression evaluated before updating symon display. when
    this expression evaluates to a non-nil value, it will be
    displayed instead of standard symon display format.

:sparkline (default: nil)

    when non-nil, sparklines are rendered.

:lower-bound (default: 100.0)

    upper bound of sparkline.

:upper-bound (default: 0.0)

    lower bound of sparkline."
      (let* ((cell (make-vector 2 nil))
             (sparkline (plist-get plist :sparkline))
             (interval (or (plist-get plist :interval) 'symon-refresh-rate))
             (display (plist-get plist :display))
             (disp-string (plist-get plist :disp-string))
             (update-fn
              `(lambda ()
                 (ring-insert (aref ,cell 0) ,(plist-get plist :fetch))))
             (setup-fn
              `(lambda ()
                 (aset ,cell 0 (symon--make-history-ring))
                 (aset ,cell 1 (run-with-timer 0 ,interval ,update-fn))
                 ,(plist-get plist :setup)
                 (funcall ,update-fn)))
             (cleanup-fn
              `(lambda ()
                 (cancel-timer (aref ,cell 1))
                 ,(plist-get plist :cleanup)))
             (display-fn
              (if display `(lambda () (concat ,display " "))
                `(lambda ()
                   (let* ((lst (ring-elements (aref ,cell 0)))
                          (val (car lst)))
                     (concat ,(plist-get plist :index)
                             (if ,disp-string
                                 ,disp-string
                               (if (not (numberp val)) "N/A "
                                 (concat (format "%d%s " val ,(or (plist-get plist :unit) ""))
                                         (let ((annot ,(plist-get plist :annotation)))
                                           (when annot
                                             (if annotation-no-paren
                                                 (concat annot " ")
                                               (concat "(" annot ") ")))))))
                             ,(when sparkline
                                `(when (window-system)
                                   (let ((sparkline (symon--make-sparkline
                                                     lst
                                                     ,(plist-get plist :lower-bound)
                                                     ,(plist-get plist :upper-bound))))
                                     (when symon-sparkline-use-xpm
                                       (setq sparkline
                                             (symon--convert-sparkline-to-xpm sparkline)))
                                     (concat (propertize " " 'display sparkline) " "))))))))))
        `(put ',name 'symon-monitor (vector ,setup-fn ,cleanup-fn ,display-fn))))

    (my/define-symon-monitor symon-date-time
                             :interval 60
                             :disp-string (concat
                                           (propertize
                                            (format-time-string "%l:%M %b %d %a")
                                            'face 'font-lock-type-face)
                                           "     "))

    (setq symon-monitors
          (cond ((memq system-type '(gnu/linux cygwin))
                 '(symon-date-time
                   symon-linux-memory-monitor
                   symon-linux-cpu-monitor
                   symon-linux-network-rx-monitor
                   symon-linux-network-tx-monitor))
                ((memq system-type '(windows-nt))
                 '(symon-date-time
                   symon-windows-memory-monitor
                   symon-windows-cpu-monitor
                   symon-windows-network-rx-monitor
                   symon-windows-network-tx-monitor))))
    (symon-mode)))


(provide 'setup-symon)
