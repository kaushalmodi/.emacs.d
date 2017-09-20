;; Time-stamp: <2017-09-20 09:50:18 kmodi>

;; Customize the mode-line

;; Contents:
;;
;;  Date/Time
;;  Line and column numbers
;;  Buffer percentage
;;  Minibuffer-line
;;  Smart Mode-Line
;;    Rich Minority
;;  Organize the order of minor mode lighters

;;; Date/Time
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

;;; Line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;;; Buffer percentage
(>=e "26.0"
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=b0b02ca7f3e06d0f092df6f81babd1277bf93b0f
    (setq mode-line-percent-position '(-3 "%o")))

;;; Minibuffer-line
;; Display date+time in the minibuffer instead of in the mode-line
(use-package minibuffer-line
  :if (null modi/show-date-time-in-mode-line)
  :defer 1 ;Prevent 'Invalid face reference: minibuffer-line' when launching emacsclient
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
                 time-string))))))
  :config
  (progn
    (set-face-attribute 'minibuffer-line nil :inherit font-lock-type-face)

    (minibuffer-line-mode)))

;;; Smart Mode-Line
;; emacs modeline aka statusbar
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :init
  (progn
    (setq sml/line-number-format "%4l")
    (setq sml/name-width 70)            ;Buffer name width in the mode-line
    (setq sml/mode-width 'full)         ;Minor mode lighters area width
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme (if (boundp 'dark-theme)
                        (if (not dark-theme) 'light 'dark)
                      'dark))

    ;; Try doing projectile based replacements *after* going through regular
    ;; replacements.
    (with-eval-after-load 'projectile
      (setq sml/use-projectile-p 'after-prefixes)
      (defun modi/sml-projectile-replacement-maybe (orig-ret)
        "Process the ORIG-RET return value of the original function."
        (let (new-ret)
          (setq new-ret (if (and (eq sml/use-projectile-p 'after-prefixes)
                                 ;; If ORIG-RET still begins with a file path
                                 (string-match-p "\\`/" orig-ret))
                            (setq new-ret (sml/perform-projectile-replacement orig-ret))
                          orig-ret))
          ;; (message "dbg-adv: %s" new-ret)
          new-ret))
      (advice-add 'sml/replacer-raw :filter-return #'modi/sml-projectile-replacement-maybe))

    (setq sml/replacer-regexp-list
          `(
            ("^~/org/"                          ":Org:")
            ;; Allow projectile to set the prefix for the below
            ;; ("^~/\\.emacs\\.d/"                   ":ED:")
            ("^~/.*box/uvm/uvm_examples/"       ":UVM_EX:")
            ("^~/.*box/uvm/adsim_uvm_examples/" ":AD_UVM_EX:")
            (":\\(.*_EX\\):\\([a-z0-9_]\\{3\\}\\).*?/"
             (lambda (string)
               (concat ":\\1:"
                       (match-string 2 string)
                       ":")))
            ;; Prefix with first 2 letters and last letter of project name
            ;; To distinguish between projects that could have same first 3 letters
            ;; Using "\,(upcase ...)" only works when calling `replace-regexp` interactively.
            ;; In lisp code you have to give it a function. So we need to change the
            ;; replacement string to,
            ;; `(lambda (string) (concat ":" (upcase (match-string 1 string)) ":")))`.
            (,(concat "\\(?1:/proj.*?/"                         ;Project base
                      "\\(?2:[a-z0-9_]\\{3\\}\\).*?\\(?3:[a-z0-9_]\\)/" ;Project name
                      "[so]+_\\(?4:[a-z0-9]+\\)\\)/"                ;User project root
                      "\\(?5:[a-z0-9_]\\{0,3\\}\\).*?/") ;Dir in user project root
             (lambda (string)
               (let* ((prj-root (match-string-no-properties 1 string))
                      (prj-abbrev (concat (capitalize (match-string-no-properties 2 string))
                                          "." (upcase (match-string-no-properties 3 string))))
                      (user (match-string-no-properties 4 string))
                      (dir-abbrev (upcase (match-string-no-properties 5 string)))
                      (is-me (string= user (getenv "USER"))))
                 (concat (if is-me
                             (concat prj-root "/")
                           (concat ":" ;Has to begin with ':' to be identified in a different face
                                   prj-abbrev
                                   "/" user))
                         ":" dir-abbrev ":"))))
            ("\\(.*:\\)CAD:.*?/\\([^/]+\\)/systemVerilog/" "\\1\\2:")
            ("\\(.*:\\)DIG:tb/"                        "\\1TB:")
            ("\\(.*:\\)DIG:syslvl_tb/"                 "\\1TB:")
            ("\\(.*:\\)TB:agents/"                     "\\1AGT:")
            ("\\(.*:\\)TB:patterns/"                   "\\1PAT:")
            ("\\(.*:\\)TB:tests/"                      "\\1TST:")
            ("\\(.*:\\)TB:tests_sv/"                   "\\1TST:")
            ("\\(.*:\\)TB:uvm.*src/"                   "\\1UVM:")
            ("\\(.*:\\)DIG:design_code/"               "\\1DSGN:")
            ("\\(.*:\\)DSGN:rtl/"                      "\\1RTL:")
            ("\\(.*:\\)DSGN:analog_partition_rtl/"     "\\1ANA:"))))
  :config
  (progn

;;;; Rich Minority
    (use-package rich-minority
      :config
      (progn
        (setq rm-blacklist
              '(" WK"        ;which-key
                " hc"        ;hardcore mode
                " AC"        ;auto-complete
                " vl"        ;global visual line mode enabled
                " Wrap"      ;shows up if visual-line-mode is enabled for that buffer
                " Omit"      ;omit mode in dired
                " yas"       ;yasnippet
                " drag"      ;drag-stuff-mode
                " VHl"       ;volatile highlights
                " ctagsU"    ;ctags update
                " Undo-Tree" ;undo tree
                " wr"        ;Wrap Region
                " SliNav"    ;elisp-slime-nav
                " Fly"       ;Flycheck
                " PgLn"      ;page-line-break
                " ElDoc"     ;eldoc
                " GG"        ;ggtags
                " hs"        ;hideshow
                " hs+"       ;
                " ez-esc"    ;easy-escape
                " ivy"       ;ivy
                " h"         ;hungry-delete-mode
                " (*)"       ;beacon
                ))
        (setq rm-text-properties '(("\\` Ovwrt\\'" 'face 'font-lock-warning-face) ;Overwrite
                                   ("\\` mc:[0-9]+ *\\'" 'face 'font-lock-keyword-face) ;Multiple cursors
                                   ("\\` rk\\'" 'display (propertize "​[]" 'face 'font-lock-warning-face)) ;Region bindings

                                   ("\\` Abbrev\\'" 'display "​@")   ;Abbrev
                                   ("\\` Ind\\'"    'display "​*>")  ;org indent
                                   ("\\` Outl\\'"   'display "​ø")   ;outline
                                   ("\\` Server\\'" 'display "​Σ")   ;Server
                                   ("\\` μ\\'"      'display "​μ")   ;modi-mode
                                   ("\\` Wg\\'"     'display "​w")   ;writegood
                                   ("\\` Vis\\'"    'display "​V")   ;visible-mode
                                   ("\\` Temp\\'"   'display "​t"))) ;temp-mode
        (with-eval-after-load 'setup-font-check
          (if font-symbola-p
              (progn
                (add-to-list 'rm-text-properties '("\\` Hi\\'"   'display "​🞵")) ;Hi-Lock
                (add-to-list 'rm-text-properties '("\\` Tail\\'" 'display "​🢛")) ;Auto revert tail
                (add-to-list 'rm-text-properties '("\\` =>\\'"   'display "​➠")) ;aggressive indent
                (add-to-list 'rm-text-properties '("\\` ARev\\'" 'display "​⭮")) ;auto revert
                (add-to-list 'rm-text-properties '("\\` Fill\\'" 'display "​⮒"))) ;auto fill
            (progn
              (add-to-list 'rm-text-properties '("\\` Hi\\'"   'display "​H"))
              (add-to-list 'rm-text-properties '("\\` Tail\\'" 'display "​Tail."))
              (add-to-list 'rm-text-properties '("\\` =>\\'"   'display "​aI."))
              (add-to-list 'rm-text-properties '("\\` ARev\\'" 'display "​aR."))
              (add-to-list 'rm-text-properties '("\\` Fill\\'" 'display "​aF.")))))))

    (sml/setup)))

;;; Organize the order of minor mode lighters
(defvar mode-line-space-mode-lighter " "
  "Lighter for `mode-line-space-mode'." )
(define-minor-mode mode-line-space-mode
  "Global minor mode to keep a space between major mode and minor mode lighters."
  :lighter mode-line-space-mode-lighter
  :init-value t                         ;Enable this minor mode by default,
  :global t)                            ;globally.

(defun modi/organize-minor-mode-lighters ()
  "The `multiple-cursors-mode' lighter is very useful in showing how many
cursors are created or if multiple-cursors-mode is enabled. Move that lighter to
the foremost position in the `minor-mode-alist'.

Move the `mode-line-space-mode' lighter to the second-foremost position in the
mode line."
  ;; If `mode-line-space-mode' is not the first in `minor-mode-alist' ..
  (unless (equal 'mode-line-space-mode (car (car minor-mode-alist)))
    ;; First remove it from the alist
    (setq minor-mode-alist (assq-delete-all 'mode-line-space-mode minor-mode-alist))
    ;; Now add it back but to the beginning of the alist
    (add-to-list 'minor-mode-alist '(mode-line-space-mode mode-line-space-mode-lighter)))

  ;; If `multiple-cursors-mode' is not the first in `minor-mode-alist' ..
  (with-eval-after-load 'multiple-cursors
    (unless (equal 'multiple-cursors-mode (car (car minor-mode-alist)))
      ;; First remove it from the alist
      (setq minor-mode-alist (assq-delete-all 'multiple-cursors-mode minor-mode-alist))
      ;; Now add it back but to the beginning of the alist
      (add-to-list 'minor-mode-alist '(multiple-cursors-mode mc/mode-line)))))
;; Update the minor-mode lighter order on actions like window switching.
(add-hook 'buffer-list-update-hook #'modi/organize-minor-mode-lighters)
;; Also add the above fn to `after-revert-hook'. So in the event you don't find
;; the minor-mode lighters in right order, simply revert the buffer.
(add-hook 'after-revert-hook #'modi/organize-minor-mode-lighters)


(provide 'setup-mode-line)

;; Variables used in display-time-format
;; http://docs.splunk.com/Documentation/Splunk/5.0.2/SearchReference/Commontimeformatvariables
;;
;; | %y | year in numbers (2-digit)                   |
;; | %Y | year in numbers (4-digit)                   |
;; | %m | month in number (eg: 12)                    |
;; | %B | full month name (eg: December)              |
;; | %b | short month name (eg: Dec)                  |
;; | %d | day in numbers, with leading zeros (eg: 08) |
;; | %e | day in numbers, no leading zeros (eg: 8)    |
;; | %A | full weekday name (eg: Sunday)              |
;; | %a | short weekday name (eg: Sun)                |
;; | %H | hours in 24-clock, with leading zeros       |
;; | %k | hours in 24-clock, no leading zeros         |
;; | %l | hours in 12-clock, with leading zeros       |
;; | %p | am/pm                                       |
;; | %T | time in 24-hour notation (%H:%M:%S)         |
