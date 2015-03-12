;; Time-stamp: <2015-03-12 12:55:53 kmodi>

;; smart-mode-line
;; emacs modeline aka statusbar
;; https://github.com/Bruce-Connor/smart-mode-line

;; It is crucial that this require line happens after the above setq block
(use-package smart-mode-line
    :init
  (progn
    (setq sml/line-number-format    "%4l")
    (setq sml/name-width            30) ; buffer name width in the mode-line
    (setq sml/mode-width            'full) ; minor mode lighters area width
    (setq sml/no-confirm-load-theme t)
    (setq sml/replacer-regexp-list
          `(
            ("^~/org/"                          ":Org:")
            ("^~/\\.emacs\\.d/"                 ":ED:")
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
                (concat (when (not (string= (match-string 3 string) (getenv "USER")))
                          (concat "[" (match-string 3 string) "]"))
                        ":" (upcase (match-string 4 string)) ":")
                ))
            ("\\(:.*\\)DIG:tb/"                    "\\1TB:" )
            ("\\(:.*\\)TB:agents/"                 "\\1AGT:" )
            ("\\(:.*\\)TB:patterns/"               "\\1PAT:" )
            ("\\(:.*\\)TB:uvm.*src/"               "\\1UVM:" )
            ("\\(:.*\\)DIG:design_code/"           "\\1DSGN:")
            ("\\(:.*\\)DSGN:rtl/"                  "\\1RTL:" )
            ("\\(:.*\\)DSGN:analog_partition_rtl/" "\\1ANA:" )
            ))
    ;; customize the date and time display format in mode-line
    (setq display-time-format               "%l:%M %b %d %a" )
    (setq display-time-default-load-average nil ) ; do NOT show average system load time
    (setq line-number-mode   t) ; show line # in mode-line
    (setq column-number-mode t)) ; show column # in mode-line
  :config
  (progn
    (use-package rich-minority
        :config
      (progn
        (setq rm-blacklist
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
                " ElDoc"        ; eldoc
                " hl-highlight" ; hl-anything
                " Helm"         ; Helm
                ))
        (add-to-list 'rm-text-properties '("Outl\\'"    'display " ø")) ; outline
        (add-to-list 'rm-text-properties '("Ind\\'"     'display " *>")) ; org indent
        (add-to-list 'rm-text-properties '("Server\\'"  'display " Σ")) ; Server
        ))

    (sml/setup)

    ;; http://bruce-connor.github.io/emacs-online-documentation/Var/display-time-mode
    ;; Just doing `(setq display-time-mode t)' does not work..
    ;;   You have to manually call the display-time-mode function if
    ;; `display-time-mode' is not set to t via `M-x customize'
    (display-time-mode 1)))


(provide 'setup-smart-mode-line)


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
