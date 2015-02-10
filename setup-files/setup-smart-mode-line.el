;; Time-stamp: <2015-02-10 10:19:52 kmodi>

;; smart-mode-line
;; emacs modeline aka statusbar
;; Source: https://github.com/Bruce-Connor/smart-mode-line


;; It is crucial that this require line happens after the above setq block
(req-package smart-mode-line
  :require (rich-minority setup-visual)
  :init
  (progn
    (setq sml/name-width            40 ;; space allocated for the buffer name in the mode-line
          sml/line-number-format    "%4l"
          sml/mode-width            'full
          sml/theme                 'respectful
          sml/no-confirm-load-theme t
          sml/replacer-regexp-list
          '(
            ("^~/org/"                          ":Org:")
            ("^~/\\.emacs\\.d/"                 ":ED:")
            ("^~/.*box/uvm/uvm_examples/"       ":UVM_EX:")
            ("^~/.*box/uvm/adsim_uvm_examples/" ":AD_UVM_EX:")
            (":\\(.*_EX\\):\\([a-z0-9_]\\{3\\}\\).*?/"
             (lambda (string) (concat ":\\1:"
                                      (match-string 2 string)
                                      ":") ) )
            ;; Prefix with first 2 letters and last letter of project name
            ;; To distinguish between projects that could have same first 3 letters
            ;; Using "\,(upcase ...)" only works when calling `replace-regexp` interactively.
            ;; In lisp code you have to give it a function. So we need to change the
            ;; replacement string to,
            ;; `(lambda (string) (concat ":" (upcase (match-string 1 string)) ":")))`.
            ("/proj.*?/\\([a-z0-9_]\\{2\\}\\).*?\\([a-z0-9_]\\)/[so]+_\\([a-z0-9]+\\)/\\([a-z0-9_]\\{3\\}\\).*?/"
             (lambda (string) (concat ":"
                                      (capitalize (match-string 1 string))
                                      (upcase (match-string 2 string))
                                      (when (not (string= (match-string 3 string) (getenv "USER")))
                                        (concat "[" (match-string 3 string) "]"))
                                      ":" (upcase (match-string 4 string)) ":"
                                      ) ) )
            (":\\(.*\\):DIG:tb/"                        ":\\1:TB:" )
            (":\\(.*\\):TB:agents/"                     ":\\1:AGT:" )
            (":\\(.*\\):TB:patterns/"                   ":\\1:PAT:" )
            (":\\(.*\\):DIG:design_code/"               ":\\1:DSGN:")
            (":\\(.*\\):DSGN:rtl/"                      ":\\1:RTL:" )
            (":\\(.*\\):DSGN:analog_partition_rtl/"     ":\\1:ANA:" )
            )
          display-time-format "%l:%M %b %d %a" ;; customize the date and time display format in mode-line
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
          display-time-default-load-average nil ;; do NOT show average system load time
          line-number-mode t ;; show line # in mode-line
          column-number-mode t ;; show column # in mode-line
          )
    (setq rm-excluded-modes
          '(" Guide"        ; guide-key mode
            " hc"           ; hardcore mode
            " AC"           ; auto-complete
            " vl"           ; global visual line mode enabled
            " Wrap"         ; shows up if visual-line-mode is enabled for that buffer
            " Omit"         ; omit mode in dired
            " yas"          ; yasnippet
            " drag"         ; drag-stuff-mode
            " VHl"          ; volatile highlights
            " ctagsU"       ; ctags update
            " Undo-Tree"    ; undo tree
            " wr"           ; Wrap Region
            " SliNav"       ; elisp-slime-nav
            " Fly"          ; Flycheck
            " PgLn"         ; page-line-break
            " GG"           ; ggtags
            " ElDoc"        ; eldoc
            " hl-highlight" ; hl-anything
            " Helm"         ; Helm
            )))
  :config
  (progn
    (sml/setup)

    ;; http://bruce-connor.github.io/emacs-online-documentation/Var/display-time-mode
    ;; Just doing `(setq display-time-mode t)' does not work..
    ;;   You have to manually call the display-time-mode function if
    ;; `display-time-mode' is not set to t via `M-x customize'
    (display-time-mode 1)))


(provide 'setup-smart-mode-line)
