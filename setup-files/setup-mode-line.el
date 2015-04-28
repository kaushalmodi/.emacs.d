;; Time-stamp: <2015-04-28 10:06:51 kmodi>

;; Customize the mode-line

(defvar modi/show-date-time-in-mode-line nil
  "If non-nil, show the date-time in the mode-line.
If nil, show the same in the minibuffer.")

(setq line-number-mode t) ; show line # in mode-line
(setq column-number-mode t) ; show column # in mode-line

;; Date, time, mail, load average display in mode-line
(if modi/show-date-time-in-mode-line
    (setq display-time-format "%l:%M %b %d %a" )
  (setq display-time-format ""))
(setq display-time-default-load-average nil) ; do NOT show average system load time

;; http://doc.endlessparentheses.com/Var/display-time-mode
;; Just doing `(setq display-time-mode t)' does not work..
;;   You have to manually call the display-time-mode function if
;; `display-time-mode' is not set to t via `M-x customize'
(display-time-mode 1)

;; Display date+time in the minibuffer instead of in the mode-line
(use-package minibuffer-line
  :if (null modi/show-date-time-in-mode-line)
  :defer 1 ; to prevent 'Invalid face reference: minibuffer-line'
                                        ; when launching emacsclient
  :init
  (progn
    (setq minibuffer-line-format '((:eval (format-time-string "%l:%M %b %d %a")))))
  :config
  (progn
    (set-face-attribute 'minibuffer-line nil :inherit font-lock-type-face)
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
               (let ((user       (match-string-no-properties 3 string))
                     (abbrev-dir (match-string-no-properties 4 string)))
                 (concat ":" ; The first char HAS to be `:'
                         (when (not (string= user (getenv "USER")))
                           (concat "~" user "/"))
                         (upcase abbrev-dir) ":"))))
            ("\\(:.*\\)DIG:tb/"                    "\\1TB:"  )
            ("\\(:.*\\)TB:agents/"                 "\\1AGT:" )
            ("\\(:.*\\)TB:patterns/"               "\\1PAT:" )
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
                " GG"           ; ggtags
                ))
        (add-to-list 'rm-text-properties '("Outl\\'"    'display " ø")) ; outline
        (add-to-list 'rm-text-properties '("Ind\\'"     'display " *>")) ; org indent
        (add-to-list 'rm-text-properties '("Server\\'"  'display " Σ")) ; Server
        ))

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
